// SPDX-License-Identifier: Apache-2.0

package chisel3.internal

import chisel3._
import chisel3.experimental.{attach, Analog, BaseModule}
import chisel3.internal.Builder.pushCommand
import chisel3.internal.firrtl.{Connect, Converter, DefInvalid}

import _root_.firrtl.passes.CheckTypes

/**
  * BiConnect.connect executes a bidirectional connection element-wise.
  *
  * Note that the arguments are left and right (not source and sink) so the
  * intent is for the operation to be commutative.
  *
  * The connect operation will recurse down the left Data (with the right Data).
  * An exception will be thrown if a movement through the left cannot be matched
  * in the right (or if the right side has extra fields).
  *
  * See elemConnect for details on how the root connections are issued.
  */

private[chisel3] object BiConnect {
  // These are all the possible exceptions that can be thrown.
  // These are from element-level connection
  def BothDriversException =
    BiConnectException(": Both Left and Right are drivers")
  def NeitherDriverException =
    BiConnectException(": Neither Left nor Right is a driver")
  def UnknownDriverException =
    BiConnectException(": Locally unclear whether Left or Right (both internal)")
  def UnknownRelationException =
    BiConnectException(": Left or Right unavailable to current module.")
  // These are when recursing down aggregate types
  def MismatchedVecException =
    BiConnectException(": Left and Right are different length Vecs.")
  def MissingLeftFieldException(field: String) =
    BiConnectException(s".$field: Left Record missing field ($field).")
  def MissingRightFieldException(field: String) =
    BiConnectException(s": Right Record missing field ($field).")
  def MismatchedException(left: String, right: String) =
    BiConnectException(s": Left ($left) and Right ($right) have different types.")
  def AttachAlreadyBulkConnectedException =
    BiConnectException("Analog previously bulk connected")
  def DontCareCantBeSink =
    BiConnectException(": DontCare cannot be a connection sink (LHS)")

  /** This function is what recursively tries to connect a left and right together
    *
    * There is some cleverness in the use of internal try-catch to catch exceptions
    * during the recursive decent and then rethrow them with extra information added.
    * This gives the user a 'path' to where in the connections things went wrong.
    *
    * == Chisel Semantics and how they emit to firrtl ==
    *
    * 1. Strict Bi-Connect (all fields as seen by firrtl must match exactly)
    *   `a <= b`
    *
    * 2. Strict Bi-Connect (implemented as being field-blasted because we know all firrtl fields would not match exactly)
    *   `a.foo <= b.foo, b.bar <= a.bar`
    *
    * 3. Not-Strict Bi-Connect (firrtl will allow fields to not match exactly)
    *   `a <- b`
    *
    * 4. Mixed Semantic Bi-Connect (some fields need to be handled differently)
    *   `a.foo <= b.foo` (case 2),  `b.bar <- a.bar` (case 3)
    *
    * - The decision on 1 vs 2 is based on structural type -- if same type once emitted to firrtl, emit 1, otherwise emit 2
    * - 1/2 vs 3 is based on CompileOptions at connection point e.g. at `<>` , emit 3 if `emitStrictConnects = false` for either side
    * - 4 is a special case of 2 turning into 3 for some subfields, when either side's subfield at `extends Bundle/Record` has `emitStrictConnects = false`
    */
  def connect(
    left:                  Data,
    right:                 Data,
    context_mod:           BaseModule
  ): Unit = {
    (left, right) match {
      // Handle element case (root case)
      case (left_a: Analog, right_a: Analog) =>
        try {
          markAnalogConnected(left_a, context_mod)
          markAnalogConnected(right_a, context_mod)
        } catch { // convert attach exceptions to BiConnectExceptions
          case attach.AttachException(message) => throw BiConnectException(message)
        }
        attach.impl(Seq(left_a, right_a), context_mod)
      case (left_a: Analog, DontCare) =>
        try {
          markAnalogConnected(left_a, context_mod)
        } catch { // convert attach exceptions to BiConnectExceptions
          case attach.AttachException(message) => throw BiConnectException(message)
        }
        pushCommand(DefInvalid(left_a.lref))
      case (DontCare, right_a: Analog) => connect(right, left, context_mod)
      case (left_e: Element, right_e: Element) => {
        elemConnect(left_e, right_e, context_mod)
        // TODO(twigg): Verify the element-level classes are connectable
      }
      // Handle Vec case
      case (left_v: Vec[Data @unchecked], right_v: Vec[Data @unchecked]) => {
        if (left_v.length != right_v.length) {
          throw MismatchedVecException
        }

        for (idx <- 0 until left_v.length) {
          try {
            connect(left_v(idx), right_v(idx), context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s"($idx)$message")
          }
        }
      }
      // Handle Vec connected to DontCare
      case (left_v: Vec[Data @unchecked], DontCare) => {
        for (idx <- 0 until left_v.length) {
          try {
            connect(left_v(idx), right, context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s"($idx)$message")
          }
        }
      }
      // Handle DontCare connected to Vec
      case (DontCare, right_v: Vec[Data @unchecked]) => {
        for (idx <- 0 until right_v.length) {
          try {
            connect(left, right_v(idx), context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s"($idx)$message")
          }
        }
      }
      // Handle Records defined in Chisel._ code by emitting a FIRRTL bulk
      // connect when possible and a partial connect otherwise
      case pair @ (left_r: Record, right_r: Record) =>
        val emitStrictConnects: Boolean = true

        // chisel3 <> is commutative but FIRRTL <- is not
        val flipConnection =
          !MonoConnect.canBeSink(left_r, context_mod) || !MonoConnect.canBeSource(right_r, context_mod)
        val (newLeft, newRight) = if (flipConnection) (right_r, left_r) else (left_r, right_r)

        recordConnect(left_r, right_r, context_mod)

      // Handle Records connected to DontCare
      case (left_r: Record, DontCare) =>
        // For each field in left, descend with right
        for ((field, left_sub) <- left_r.elements) {
          try {
            connect(left_sub, right, context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s".$field$message")
          }
        }
      case (DontCare, right_r: Record) =>
        // For each field in left, descend with right
        for ((field, right_sub) <- right_r.elements) {
          try {
            connect(left, right_sub, context_mod)
          } catch {
            case BiConnectException(message) => throw BiConnectException(s".$field$message")
          }
        }

      // Left and right are different subtypes of Data so fail
      case (left, right) => throw MismatchedException(left.toString, right.toString)
    }
  }

  // Do connection of two Records
  def recordConnect(
    left_r:                Record,
    right_r:               Record,
    context_mod:           BaseModule
  ): Unit = {
    // Verify right has no extra fields that left doesn't have

    // For each field in left, descend with right.
    // Don't bother doing this check if we don't expect it to necessarily pass.
    for ((field, right_sub) <- right_r.elements) {
      if (!left_r.elements.isDefinedAt(field)) {
        throw MissingLeftFieldException(field)
      }
    }
    // For each field in left, descend with right
    for ((field, left_sub) <- left_r.elements) {
      try {
        right_r.elements.get(field) match {
          case Some(right_sub) => connect(left_sub, right_sub, context_mod)
          case None => throw MissingRightFieldException(field)
        }
      } catch {
        case BiConnectException(message) => throw BiConnectException(s".$field$message")
      }
    }
  }

  /** Check whether two aggregates can be bulk connected (<=) in FIRRTL. From the
    * FIRRTL specification, the following must hold for bulk connection:
    *
    *   1. The types of the left-hand and right-hand side expressions must be
    *       equivalent.
    *   2. The bit widths of the two expressions must allow for data to always
    *        flow from a smaller bit width to an equal size or larger bit width.
    *   3. The flow of the left-hand side expression must be sink or duplex
    *   4. Either the flow of the right-hand side expression is source or duplex,
    *      or the right-hand side expression has a passive type.
    */
  private[chisel3] def canBulkConnectAggregates(
    sink:                  Aggregate,
    source:                Aggregate,
    context_mod:           BaseModule
  ): Boolean = {

    // check that the aggregates have the same types
    def typeCheck = CheckTypes.validConnect(
      Converter.extractType(sink),
      Converter.extractType(source)
    )

    // check records live in appropriate contexts
    def contextCheck =
      MonoConnect.aggregateConnectContextCheck(
        sink,
        source,
        context_mod
      )

    // sink must be writable and must also not be a literal
    def bindingCheck = sink.topBinding match {
      case _: ReadOnlyBinding => false
      case _ => true
    }

    // check data can flow between provided aggregates
    def flow_check = MonoConnect.canBeSink(sink, context_mod) && MonoConnect.canBeSource(source, context_mod)

    // do not bulk connect source literals (results in infinite recursion from calling .ref)
    def sourceNotLiteralCheck = source.topBinding match {
      case _: LitBinding => false
      case _ => true
    }

    typeCheck && contextCheck && bindingCheck && flow_check && sourceNotLiteralCheck
  }

  // These functions (finally) issue the connection operation
  // Issue with right as sink, left as source
  private def issueConnectL2R(left: Element, right: Element): Unit = {
    // Source and sink are ambiguous in the case of a Bi/Bulk Connect (<>).
    // If either is a DontCareBinding, just issue a DefInvalid for the other,
    //  otherwise, issue a Connect.
    (left.topBinding, right.topBinding) match {
      case (lb: DontCareBinding, _) => pushCommand(DefInvalid(right.lref))
      case (_, rb: DontCareBinding) => pushCommand(DefInvalid(left.lref))
      case (_, _) => pushCommand(Connect(right.lref, left.ref))
    }
  }
  // Issue with left as sink, right as source
  private def issueConnectR2L(left: Element, right: Element): Unit = {
    // Source and sink are ambiguous in the case of a Bi/Bulk Connect (<>).
    // If either is a DontCareBinding, just issue a DefInvalid for the other,
    //  otherwise, issue a Connect.
    (left.topBinding, right.topBinding) match {
      case (lb: DontCareBinding, _) => pushCommand(DefInvalid(right.lref))
      case (_, rb: DontCareBinding) => pushCommand(DefInvalid(left.lref))
      case (_, _) => pushCommand(Connect(left.lref, right.ref))
    }
  }

  // This function checks if element-level connection operation allowed.
  // Then it either issues it or throws the appropriate exception.
  def elemConnect(
    _left:                 Element,
    _right:                Element,
    context_mod:           BaseModule
  ): Unit = {
    import BindingDirection.{Input, Internal, Output} // Using extensively so import these
    val left = _left
    val right = _right
    // If left or right have no location, assume in context module
    // This can occur if one of them is a literal, unbound will error previously
    val left_mod:  BaseModule = left.topBinding.location.getOrElse(context_mod)
    val right_mod: BaseModule = right.topBinding.location.getOrElse(context_mod)

    val left_parent = Builder.retrieveParent(left_mod, context_mod).getOrElse(None)
    val right_parent = Builder.retrieveParent(right_mod, context_mod).getOrElse(None)

    val left_direction = BindingDirection.from(left.topBinding, left.direction)
    val right_direction = BindingDirection.from(right.topBinding, right.direction)

    // CASE: Context is same module as left node and right node is in a child module
    if ((left_mod == context_mod) && (right_parent == context_mod)) {
      // Thus, right node better be a port node and thus have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CURRENT MOD   CHILD MOD
        case (Input, Input)    => issueConnectL2R(left, right)
        case (Internal, Input) => issueConnectL2R(left, right)

        case (Output, Output)   => issueConnectR2L(left, right)
        case (Internal, Output) => issueConnectR2L(left, right)

        case (Input, Output) => throw BothDriversException
        case (Output, Input) => throw NeitherDriverException
        case (_, Internal)   => throw UnknownRelationException
      }
    }

    // CASE: Context is same module as right node and left node is in child module
    else if ((right_mod == context_mod) && (left_parent == context_mod)) {
      // Thus, left node better be a port node and thus have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CHILD MOD     CURRENT MOD
        case (Input, Input)    => issueConnectR2L(left, right)
        case (Input, Internal) => issueConnectR2L(left, right)

        case (Output, Output)   => issueConnectL2R(left, right)
        case (Output, Internal) => issueConnectL2R(left, right)

        case (Input, Output) => throw NeitherDriverException
        case (Output, Input) => throw BothDriversException
        case (Internal, _)   => throw UnknownRelationException
      }
    }

    // CASE: Context is same module that both left node and right node are in
    else if ((context_mod == left_mod) && (context_mod == right_mod)) {
      ((left_direction, right_direction): @unchecked) match {
        //    CURRENT MOD   CURRENT MOD
        case (Input, Output)    => issueConnectL2R(left, right)
        case (Input, Internal)  => issueConnectL2R(left, right)
        case (Internal, Output) => issueConnectL2R(left, right)

        case (Output, Input)    => issueConnectR2L(left, right)
        case (Output, Internal) => issueConnectR2L(left, right)
        case (Internal, Input)  => issueConnectR2L(left, right)

        case (Input, Input)   => throw BothDriversException
        case (Output, Output) => throw BothDriversException
        case (Internal, Internal) => throw UnknownDriverException
      }
    }

    // CASE: Context is the parent module of both the module containing left node
    //                                        and the module containing right node
    //   Note: This includes case when left and right in same module but in parent
    else if ((left_parent == context_mod) && (right_parent == context_mod)) {
      // Thus both nodes must be ports and have a direction hint
      ((left_direction, right_direction): @unchecked) match {
        //    CHILD MOD     CHILD MOD
        case (Input, Output) => issueConnectR2L(left, right)
        case (Output, Input) => issueConnectL2R(left, right)

        case (Input, Input)   => throw NeitherDriverException
        case (Output, Output) => throw BothDriversException
        case (_, Internal) => throw UnknownRelationException
        case (Internal, _) => throw UnknownRelationException
      }
    }

    // Not quite sure where left and right are compared to current module
    // so just error out
    else throw UnknownRelationException
  }

  // This function checks if analog element-level attaching is allowed, then marks the Analog as connected
  def markAnalogConnected(analog: Analog, contextModule: BaseModule): Unit = {
    println("not doing anything for analog till we have sourceinfos again")
  }
}
