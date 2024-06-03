// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.firrtl
import chisel3._
import chisel3.experimental._
import firrtl.{ir => fir}
import chisel3.internal.{castToInt, throwException, HasId}

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.{Queue, VectorBuilder}
import scala.collection.immutable.LazyList // Needed for 2.12 alias

@nowarn("msg=class Port") // delete when Port becomes private
private[chisel3] object Converter {
  // TODO modeled on unpack method on Printable, refactor?
  def unpack(pable: Printable, ctx: Component): (String, Seq[Arg]) = pable match {
    case Printables(pables) =>
      val (fmts, args) = pables.map(p => unpack(p, ctx)).unzip
      (fmts.mkString, args.flatten.toSeq)
    case PString(str) => (str.replaceAll("%", "%%"), List.empty)
    case format: FirrtlFormat =>
      ("%" + format.specifier, List(format.bits.ref))
    case Name(data)     => (data.ref.name, List.empty)
    case FullName(data) => (data.ref.fullName(ctx), List.empty)
    case Percent        => ("%%", List.empty)
  }

  private def reportInternalError(msg: String): Nothing = {
    val link = "https://github.com/chipsalliance/chisel3/issues/new"
    val fullMsg = s"Internal Error! $msg This is a bug in Chisel, please file an issue at '$link'"
    throwException(fullMsg)
  }

  def getRef(id: HasId): Arg =
    id.getOptionRef.getOrElse {
      val module = id._parent.map(m => s" '$id' was defined in module '$m'.").getOrElse("")
      val loc = ""
      reportInternalError(s"Could not get ref for '$id'$loc!$module")
    }

  private def clonedModuleIOError(mod: BaseModule, name: String): Nothing = {
    val loc = ""
    reportInternalError(s"Trying to convert a cloned IO of $mod inside of $mod itself$loc!")
  }

  // def convert(info: SourceInfo): fir.Info = fir.NoInfo

  def convert(op: PrimOp): fir.PrimOp = firrtl.PrimOps.fromString(op.name)

  def convert(dir: MemPortDirection): firrtl.MPortDir = dir match {
    case MemPortDirection.INFER => firrtl.MInfer
    case MemPortDirection.READ  => firrtl.MRead
    case MemPortDirection.WRITE => firrtl.MWrite
    case MemPortDirection.RDWR  => firrtl.MReadWrite
  }

  // TODO
  //   * Memoize?
  //   * Move into the Chisel IR?
  def convert(arg: Arg, ctx: Component): fir.Expression = arg match {
    case Node(id) =>
      convert(getRef(id), ctx)
    case Ref(name) =>
      fir.Reference(name, fir.UnknownType)
    case Slot(imm, name) =>
      fir.SubField(convert(imm, ctx), name, fir.UnknownType)
    case OpaqueSlot(imm) =>
      convert(imm, ctx)
    case Index(imm, ILit(idx)) =>
      fir.SubIndex(convert(imm, ctx), castToInt(idx, "Index"), fir.UnknownType)
    case Index(imm, value) =>
      fir.SubAccess(convert(imm, ctx), convert(value, ctx), fir.UnknownType)
    case ModuleIO(mod, name) =>
      if (mod eq ctx.id) fir.Reference(name, fir.UnknownType)
      else fir.SubField(fir.Reference(getRef(mod).name, fir.UnknownType), name, fir.UnknownType)
    case ModuleCloneIO(mod, name) =>
      if (mod eq ctx.id) clonedModuleIOError(mod, name)
      else fir.Reference(name)
    case u @ ULit(n, UnknownWidth()) =>
      fir.UIntLiteral(n, fir.IntWidth(u.minWidth))
    case ULit(n, w) =>
      fir.UIntLiteral(n, convert(w))
    case slit @ SLit(n, w) =>
      fir.SIntLiteral(n, convert(w))
      val unsigned = if (n < 0) (BigInt(1) << slit.width.get) + n else n
      val uint = convert(ULit(unsigned, slit.width), ctx)
      fir.DoPrim(firrtl.PrimOps.AsSInt, Seq(uint), Seq.empty, fir.UnknownType)
    // TODO Simplify
    case lit: ILit =>
      throwException(s"Internal Error! Unexpected ILit: $lit")
  }

  /** Convert Commands that map 1:1 to Statements */
  def convertSimpleCommand(cmd: Command, ctx: Component): Option[fir.Statement] = cmd match {
    case e: DefPrim[_] =>
      val consts = e.args.collect { case ILit(i) => i }
      val args = e.args.flatMap {
        case _: ILit => None
        case other => Some(convert(other, ctx))
      }
      val expr = e.op.name match {
        case "mux" =>
          assert(args.size == 3, s"Mux with unexpected args: $args")
          fir.Mux(args(0), args(1), args(2), fir.UnknownType)
        case _ =>
          fir.DoPrim(convert(e.op), args, consts, fir.UnknownType)
      }
      Some(fir.DefNode(fir.NoInfo, e.name, expr))
    case e @ DefWire(id) =>
      Some(fir.DefWire(fir.NoInfo, e.name, extractType(id)))
    case e @ DefReg(id, clock) =>
      Some(
        fir.DefRegister(
          fir.NoInfo,
          e.name,
          extractType(id),
          convert(clock, ctx),
          firrtl.Utils.zero,
          convert(getRef(id), ctx)
        )
      )
    case e @ DefRegInit(id, clock, reset, init) =>
      Some(
        fir.DefRegister(
          fir.NoInfo,
          e.name,
          extractType(id),
          convert(clock, ctx),
          convert(reset, ctx),
          convert(init, ctx)
        )
      )
    case e @ DefMemory(id, t, size) =>
      Some(firrtl.CDefMemory(fir.NoInfo, e.name, extractType(t), size, false))
    case e @ DefSeqMemory(id, t, size, ruw) =>
      Some(firrtl.CDefMemory(fir.NoInfo, e.name, extractType(t), size, true, ruw))
    case e: DefMemPort[_] =>
      val info = fir.NoInfo
      Some(
        firrtl.CDefMPort(
          fir.NoInfo,
          e.name,
          fir.UnknownType,
          e.source.fullName(ctx),
          Seq(convert(e.index, ctx), convert(e.clock, ctx)),
          convert(e.dir)
        )
      )
    case Connect(loc, exp) =>
      Some(fir.Connect(fir.NoInfo, convert(loc, ctx), convert(exp, ctx)))
    case BulkConnect(loc, exp) =>
      Some(fir.PartialConnect(fir.NoInfo, convert(loc, ctx), convert(exp, ctx)))
    case Attach(locs) =>
      Some(fir.Attach(fir.NoInfo, locs.map(l => convert(l, ctx))))
    case DefInvalid(arg) =>
      Some(fir.IsInvalid(fir.NoInfo, convert(arg, ctx)))
    case e @ DefInstance(id, _) =>
      Some(fir.DefInstance(fir.NoInfo, e.name, id.name))
    case e @ Printf(_, clock, pable) =>
      val (fmt, args) = unpack(pable, ctx)
      Some(
        fir.Print(
          fir.NoInfo,
          fir.StringLit(fmt),
          args.map(a => convert(a, ctx)),
          convert(clock, ctx),
          firrtl.Utils.one,
          e.name
        )
      )
    case _ => None
  }

  /** Internal datastructure to help translate Chisel's flat Command structure to FIRRTL's AST
    *
    * In particular, when scoping is translated from flat with begin end to a nested datastructure
    *
    * @param when Current when Statement, holds info, condition, and consequence as they are
    *        available
    * @param outer Already converted Statements that precede the current when block in the scope in
    *        which the when is defined (ie. 1 level up from the scope inside the when)
    * @param alt Indicates if currently processing commands in the alternate (else) of the when scope
    */
  // TODO we should probably have a different structure in the IR to close elses
  private case class WhenFrame(when: fir.Conditionally, outer: VectorBuilder[fir.Statement], alt: Boolean)

  /** Convert Chisel IR Commands into FIRRTL Statements
    *
    * @note ctx is needed because references to ports translate differently when referenced within
    *   the module in which they are defined vs. parent modules
    * @param cmds Chisel IR Commands to convert
    * @param ctx Component (Module) context within which we are translating
    * @return FIRRTL Statement that is equivalent to the input cmds
    */
  def convert(cmds: Seq[Command], ctx: Component): fir.Statement = {
    var stmts = new VectorBuilder[fir.Statement]()
    var scope: List[WhenFrame] = Nil
    var cmdsIt = cmds.iterator.buffered
    // Extra var because sometimes we want to push a Command to the head of cmdsIt
    // This is more efficient than changing the iterator
    var nextCmd: Command = null
    while (nextCmd != null || cmdsIt.hasNext) {
      val cmd = if (nextCmd != null) {
        val _nextCmd = nextCmd
        nextCmd = null
        _nextCmd
      } else {
        cmdsIt.next()
      }
      convertSimpleCommand(cmd, ctx) match {
        // Most Commands map 1:1
        case Some(stmt) =>
          stmts += stmt
        // When scoping logic does not map 1:1 and requires pushing/popping WhenFrames
        // Please see WhenFrame for more details
        case None =>
          cmd match {
            case WhenBegin(pred) =>
              val when = fir.Conditionally(fir.NoInfo, convert(pred, ctx), fir.EmptyStmt, fir.EmptyStmt)
              val frame = WhenFrame(when, stmts, false)
              stmts = new VectorBuilder[fir.Statement]
              scope = frame :: scope
            case WhenEnd(depth, _) =>
              val frame = scope.head
              val when =
                if (frame.alt) frame.when.copy(alt = fir.Block(stmts.result()))
                else frame.when.copy(conseq = fir.Block(stmts.result()))
              // Check if this when has an else
              cmdsIt.headOption match {
                case Some(AltBegin()) =>
                  assert(!frame.alt, "Internal Error! Unexpected when structure!") // Only 1 else per when
                  scope = frame.copy(when = when, alt = true) :: scope.tail
                  cmdsIt.next() // Consume the AltBegin
                  stmts = new VectorBuilder[fir.Statement]
                case _ => // Not followed by otherwise
                  // If depth > 0 then we need to close multiple When scopes so we add a new WhenEnd
                  // If we're nested we need to add more WhenEnds to ensure each When scope gets
                  // properly closed
                  if (depth > 0) {
                    nextCmd = WhenEnd(depth - 1, false)
                  }
                  stmts = frame.outer
                  stmts += when
                  scope = scope.tail
              }
            case OtherwiseEnd(depth) =>
              val frame = scope.head
              val when = frame.when.copy(alt = fir.Block(stmts.result()))
              // TODO For some reason depth == 1 indicates the last closing otherwise whereas
              //  depth == 0 indicates last closing when
              if (depth > 1) {
                nextCmd = OtherwiseEnd(depth - 1)
              }
              stmts = frame.outer
              stmts += when
              scope = scope.tail
          }
      }
    }
    assert(scope.isEmpty)
    fir.Block(stmts.result())
  }

  def convert(width: Width): fir.Width = width match {
    case UnknownWidth()    => fir.UnknownWidth
    case KnownWidth(value) => fir.IntWidth(value)
  }

  def convert(bp: BinaryPoint): fir.Width = bp match {
    case UnknownBinaryPoint      => fir.UnknownWidth
    case KnownBinaryPoint(value) => fir.IntWidth(value)
  }

  private def firrtlUserDirOf(d: Data): SpecifiedDirection = d match {
    case d: Vec[_] =>
      SpecifiedDirection.fromParent(d.specifiedDirection, firrtlUserDirOf(d.sample_element))
    case d => d.specifiedDirection
  }

  def extractType(data: Data): fir.Type = extractType(data, false)

  def extractType(data: Data, clearDir: Boolean): fir.Type = data match {
    case _: Clock      => fir.ClockType
    case _: AsyncReset => fir.AsyncResetType
    case _: ResetType  => fir.ResetType
    case d: EnumType   => fir.UIntType(convert(d.width))
    case d: UInt       => fir.UIntType(convert(d.width))
    case d: SInt       => fir.SIntType(convert(d.width))
    case d: Analog => fir.AnalogType(convert(d.width))
    case d: Vec[_] =>
      val childClearDir = clearDir ||
        d.specifiedDirection == SpecifiedDirection.Input || d.specifiedDirection == SpecifiedDirection.Output
      fir.VectorType(extractType(d.sample_element, childClearDir), d.length)
    case d: Record => {
      val childClearDir = clearDir ||
        d.specifiedDirection == SpecifiedDirection.Input || d.specifiedDirection == SpecifiedDirection.Output
      def eltField(elt: Data): fir.Field = (childClearDir, firrtlUserDirOf(elt)) match {
        case (true, _) => fir.Field(getRef(elt).name, fir.Default, extractType(elt, true))
        case (false, SpecifiedDirection.Unspecified | SpecifiedDirection.Output) =>
          fir.Field(getRef(elt).name, fir.Default, extractType(elt, false))
        case (false, SpecifiedDirection.Flip | SpecifiedDirection.Input) =>
          fir.Field(getRef(elt).name, fir.Flip, extractType(elt, false))
      }
      if (!d._isOpaqueType)
        fir.BundleType(d.elements.toIndexedSeq.reverse.map { case (_, e) => eltField(e) })
      else
        extractType(d.elements.head._2, childClearDir)
    }
  }

  def convert(port: Port, topDir: SpecifiedDirection = SpecifiedDirection.Unspecified): fir.Port = {
    val resolvedDir = SpecifiedDirection.fromParent(topDir, port.dir)
    val dir = resolvedDir match {
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Output => fir.Output
      case SpecifiedDirection.Flip | SpecifiedDirection.Input         => fir.Input
    }
    val clearDir = resolvedDir match {
      case SpecifiedDirection.Input | SpecifiedDirection.Output     => true
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip => false
    }

    val tpe = extractType(port.id, clearDir)
    fir.Port(fir.NoInfo, getRef(port.id).name, dir, tpe)
  }

  def convert(component: Component): fir.DefModule = component match {
    case ctx @ DefModule(_, name, ports, cmds) =>
      fir.Module(fir.NoInfo, name, ports.map(p => convert(p)), convert(cmds, ctx))
  }

  def convert(circuit: Circuit): fir.Circuit =
    fir.Circuit(fir.NoInfo, circuit.components.map(convert), circuit.name)

  // TODO Unclear if this should just be the default
  def convertLazily(circuit: Circuit): fir.Circuit = {
    val lazyModules = LazyList() ++ circuit.components
    fir.Circuit(fir.NoInfo, lazyModules.map(convert), circuit.name)
  }
}
