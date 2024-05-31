// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental

import scala.collection.mutable
import chisel3._
import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl.PrimOp._
import chisel3.internal.firrtl._
import chisel3.internal.{throwException, Binding, Builder, ChildBinding, ConstrainedBinding, InstanceId}
import firrtl.annotations._

object EnumAnnotations {

  /** An annotation for strong enum instances that are ''not'' inside of Vecs
    *
    * @param target the enum instance being annotated
    * @param enumTypeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
    */
  case class EnumComponentAnnotation(target: Named, enumTypeName: String) extends SingleTargetAnnotation[Named] {
    def duplicate(n: Named): EnumComponentAnnotation = this.copy(target = n)
  }

  case class EnumComponentChiselAnnotation(target: InstanceId, enumTypeName: String) extends ChiselAnnotation {
    def toFirrtl: EnumComponentAnnotation = EnumComponentAnnotation(target.toNamed, enumTypeName)
  }

  /** An annotation for Vecs of strong enums.
    *
    * The ''fields'' parameter deserves special attention, since it may be difficult to understand. Suppose you create a the following Vec:
    *
    *               {{{
    *               VecInit(new Bundle {
    *                 val e = MyEnum()
    *                 val b = new Bundle {
    *                   val inner_e = MyEnum()
    *                 }
    *                 val v = Vec(3, MyEnum())
    *               }
    *               }}}
    *
    *               Then, the ''fields'' parameter will be: ''Seq(Seq("e"), Seq("b", "inner_e"), Seq("v"))''. Note that for any Vec that doesn't contain Bundles, this field will simply be an empty Seq.
    *
    * @param target the Vec being annotated
    * @param typeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
    * @param fields a list of all chains of elements leading from the Vec instance to its inner enum fields.
    */
  case class EnumVecAnnotation(target: Named, typeName: String, fields: Seq[Seq[String]])
      extends SingleTargetAnnotation[Named] {
    def duplicate(n: Named): EnumVecAnnotation = this.copy(target = n)
  }

  case class EnumVecChiselAnnotation(target: InstanceId, typeName: String, fields: Seq[Seq[String]])
      extends ChiselAnnotation {
    override def toFirrtl: EnumVecAnnotation = EnumVecAnnotation(target.toNamed, typeName, fields)
  }

  /** An annotation for enum types (rather than enum ''instances'').
    *
    * @param typeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
    * @param definition a map describing which integer values correspond to which enum names
    */
  case class EnumDefAnnotation(typeName: String, definition: Map[String, BigInt]) extends NoTargetAnnotation

  case class EnumDefChiselAnnotation(typeName: String, definition: Map[String, BigInt]) extends ChiselAnnotation {
    override def toFirrtl: Annotation = EnumDefAnnotation(typeName, definition)
  }
}

import EnumAnnotations._

@deprecated("This type has moved to chisel3", "Chisel 3.5")
abstract class EnumType(private[chisel3] val factory: ChiselEnum, selfAnnotating: Boolean = true) extends Element {

  // Use getSimpleName instead of enumTypeName because for debugging purposes
  //   the fully qualified name isn't necessary (compared to for the
  //  Enum annotation), and it's more consistent with Bundle printing.
  override def toString: String = {
    litOption match {
      case Some(value) =>
        factory.nameOfValue(value) match {
          case Some(name) => s"${factory.getClass.getSimpleName.init}($value=$name)"
          case None       => stringAccessor(s"${factory.getClass.getSimpleName.init}($value=(invalid))")
        }
      case _ => stringAccessor(s"${factory.getClass.getSimpleName.init}")
    }
  }

  override def cloneType: this.type = factory().asInstanceOf[this.type]

  private[chisel3] def compop(op: PrimOp, other: EnumType): Bool = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")

    if (!this.typeEquivalent(other)) {
      throwException(s"Enum types are not equivalent: ${this.enumTypeName}, ${other.enumTypeName}")
    }

    pushOp(DefPrim(Bool(), op, this.ref, other.ref))
  }

  private[chisel3] override def typeEquivalent(that: Data): Boolean = {
    this.getClass == that.getClass &&
    this.factory == that.asInstanceOf[EnumType].factory
  }

  private[chisel3] override def connectFromBits(
    that: Bits
  ): Unit = {
    this := factory.apply(that.asUInt)
  }

  def ===(that: EnumType): Bool =
    compop(EqualOp, that)
  def =/=(that: EnumType): Bool =
    compop(NotEqualOp, that)
  def <(that: EnumType): Bool =
    compop(LessOp, that)
  def >(that: EnumType): Bool =
    compop(GreaterOp, that)
  def <=(that: EnumType): Bool =
    compop(LessEqOp, that)
  def >=(that: EnumType): Bool =
    compop(GreaterEqOp, that)

  override def asUInt: UInt =
    pushOp(DefPrim(UInt(width), AsUIntOp, ref))

  protected[chisel3] override def width: Width = factory.width

  def isValid: Bool = {
    if (litOption.isDefined) {
      true.B
    } else {
      if (factory.isTotal) true.B else factory.all.map(this === _).reduce(_ || _)
    }
  }

  /** Test if this enumeration is equal to any of the values in a given sequence
    *
    * @param s a [[scala.collection.Seq$ Seq]] of enumeration values to look for
    * @return a hardware [[Bool]] that indicates if this value matches any of the given values
    */
  final def isOneOf(s: Seq[EnumType]): Bool = {
    VecInit(s.map(this === _)).asUInt().orR()
  }

  /** Test if this enumeration is equal to any of the values given as arguments
    *
    * @param u1 the first value to look for
    * @param u2 zero or more additional values to look for
    * @return a hardware [[Bool]] that indicates if this value matches any of the given values
    */
  final def isOneOf(
    u1: EnumType,
    u2: EnumType*
  ): Bool = isOneOf(u1 +: u2.toSeq)

  def next: this.type = {
    if (litOption.isDefined) {
      val index = factory.all.indexOf(this)

      if (index < factory.all.length - 1) {
        factory.all(index + 1).asInstanceOf[this.type]
      } else {
        factory.all.head.asInstanceOf[this.type]
      }
    } else {
      val enums_with_nexts = factory.all.zip(factory.all.tail :+ factory.all.head)
      val next_enum = SeqUtils.priorityMux(enums_with_nexts.map { case (e, n) => (this === e, n) })
      next_enum.asInstanceOf[this.type]
    }
  }

  private[chisel3] def bindToLiteral(num: BigInt, w: Width): Unit = {
    val lit = ULit(num, w)
    lit.bindLitArg(this)
  }

  override private[chisel3] def bind(
    target:          Binding,
    parentDirection: SpecifiedDirection = SpecifiedDirection.Unspecified
  ): Unit = {
    super.bind(target, parentDirection)

    // Make sure we only annotate hardware and not literals
    if (selfAnnotating && isSynthesizable && topBindingOpt.get.isInstanceOf[ConstrainedBinding]) {
      annotateEnum()
    }
  }

  // This function conducts a depth-wise search to find all enum-type fields within a vector or bundle (or vector of bundles)
  private def enumFields(d: Aggregate): Seq[Seq[String]] = d match {
    case v: Vec[_] =>
      v.sample_element match {
        case b: Bundle => enumFields(b)
        case _ => Seq()
      }
    case b: Bundle =>
      b.elements.collect {
        case (name, e: EnumType) if this.typeEquivalent(e) => Seq(Seq(name))
        case (name, v: Vec[_]) if this.typeEquivalent(v.sample_element) => Seq(Seq(name))
        case (name, b2: Bundle) => enumFields(b2).map(name +: _)
      }.flatten.toSeq
  }

  private def outerMostVec(d: Data = this): Option[Vec[_]] = {
    val currentVecOpt = d match {
      case v: Vec[_] => Some(v)
      case _ => None
    }

    d.binding match {
      case Some(ChildBinding(parent)) =>
        outerMostVec(parent) match {
          case outer @ Some(_) => outer
          case None            => currentVecOpt
        }
      case _ => currentVecOpt
    }
  }

  private def annotateEnum(): Unit = {
    val anno = outerMostVec() match {
      case Some(v) => EnumVecChiselAnnotation(v, enumTypeName, enumFields(v))
      case None    => EnumComponentChiselAnnotation(this, enumTypeName)
    }

    if (!Builder.enumAnnos.contains(anno)) {
      Builder.enumAnnos += anno
      annotate(anno)
    }

    if (!Builder.enumAnnos.contains(factory.globalAnnotation)) {
      Builder.enumAnnos += factory.globalAnnotation
      annotate(factory.globalAnnotation)
    }
  }

  protected def enumTypeName: String = factory.enumTypeName

  def toPrintable: Printable = {
    val allNames = factory.allNames.zip(factory.all)
    val nameSize = allNames.map(_._1.length).max
    def leftPad(str: String): String = {
      str.reverse.padTo(nameSize, ' ').reverse
    }
    val allNamesPadded = allNames.map { case (name, value) => leftPad(name) -> value }

    val result = Wire(Vec(nameSize, UInt(8.W))).suggestName(s"_${enumTypeName}Printable")
    result.foreach(_ := '?'.U)

    for ((name, value) <- allNamesPadded) {
      when(this === value) {
        for ((r, c) <- result.zip(name)) {
          r := c.toChar.U
        }
      }
    }
    result.map(Character(_)).foldLeft(p"")(_ + _)
  }
}

@deprecated("This type has been moved and renamed to chisel3.ChiselEnum", "Chisel 3.5")
abstract class EnumFactory {
  class Type extends EnumType(this)
  object Type {
    def apply(): Type = EnumFactory.this.apply()
  }

  private var id:             BigInt = 0
  private[chisel3] var width: Width = 0.W

  private case class EnumRecord(inst: Type, name: String)
  private val enumRecords = mutable.ArrayBuffer.empty[EnumRecord]

  private def enumNames = enumRecords.map(_.name).toSeq
  private def enumValues = enumRecords.map(_.inst.litValue).toSeq
  private def enumInstances = enumRecords.map(_.inst).toSeq

  private[chisel3] val enumTypeName = getClass.getName.init

  // Do all bitvectors of this Enum's width represent legal states?
  private[chisel3] def isTotal: Boolean = {
    (this.getWidth < 31) && // guard against Integer overflow
    (enumRecords.size == (1 << this.getWidth))
  }

  private[chisel3] def globalAnnotation: EnumDefChiselAnnotation =
    EnumDefChiselAnnotation(enumTypeName, (enumNames, enumValues).zipped.toMap)

  def getWidth: Int = width.get

  def all: Seq[Type] = enumInstances
  /* Accessor for Seq of names in enumRecords */
  def allNames: Seq[String] = enumNames

  private[chisel3] def nameOfValue(id: BigInt): Option[String] = {
    enumRecords.find(_.inst.litValue == id).map(_.name)
  }

  protected def Value(name: String): Type = {
    val result = new Type

    // We have to use UnknownWidth here, because we don't actually know what the final width will be
    result.bindToLiteral(id, UnknownWidth())

    enumRecords.append(EnumRecord(result, name))

    width = (1.max(id.bitLength)).W
    id += 1

    result
  }

  protected def Value(name: String, id: UInt): Type = {
    // TODO: These throw ExceptionInInitializerError which can be confusing to the user. Get rid of the error, and just
    // throw an exception
    if (id.litOption.isEmpty) {
      throwException(s"$enumTypeName defined with a non-literal type")
    }
    if (id.litValue < this.id) {
      throwException(s"Enums must be strictly increasing: $enumTypeName")
    }

    this.id = id.litValue
    Value(name)
  }

  def apply(): Type = new Type

  private def castImpl(
    n:    UInt,
    warn: Boolean
  ): Type = {
    if (n.litOption.isDefined) {
      enumInstances.find(_.litValue == n.litValue) match {
        case Some(result) => result
        case None         => throwException(s"${n.litValue} is not a valid value for $enumTypeName")
      }
    } else if (!n.isWidthKnown) {
      throwException(s"Non-literal UInts being cast to $enumTypeName must have a defined width")
    } else if (n.getWidth > this.getWidth) {
      throwException(s"The UInt being cast to $enumTypeName is wider than $enumTypeName's width ($getWidth)")
    } else {
      if (!Builder.suppressEnumCastWarning && warn && !this.isTotal) {
        Builder.warning(
          s"Casting non-literal UInt to $enumTypeName. You can use $enumTypeName.safe to cast without this warning."
        )
      }
      val glue = Wire(new UnsafeEnum(width))
      glue := n
      val result = Wire(new Type)
      result := glue
      result
    }
  }

  /** Cast an [[UInt]] to the type of this Enum
    *
    * @note will give a Chisel elaboration time warning if the argument could hit invalid states
    * @param n the UInt to cast
    * @return the equivalent Enum to the value of the cast UInt
    */
  def apply(n: UInt): Type =
    castImpl(n, warn = true)

  /** Safely cast an [[UInt]] to the type of this Enum
    *
    * @param n the UInt to cast
    * @return the equivalent Enum to the value of the cast UInt and a Bool indicating if the
    *         Enum is valid
    */
  def safe(n: UInt): (Type, Bool) = {
    val t = castImpl(n, warn = false)
    (t, t.isValid)
  }
}

// This is an enum type that can be connected directly to UInts. It is used as a "glue" to cast non-literal UInts
// to enums.
private[chisel3] class UnsafeEnum(override val width: Width) extends EnumType(UnsafeEnum, selfAnnotating = false) {
  override def cloneType: this.type = new UnsafeEnum(width).asInstanceOf[this.type]
}
private object UnsafeEnum extends ChiselEnum

/** Suppress enum cast warnings
  *
  * Users should use [[ChiselEnum.safe <EnumType>.safe]] when possible.
  *
  * This is primarily used for casting from [[UInt]] to a Bundle type that contains an Enum.
  * {{{
  * class MyBundle extends Bundle {
  *   val addr = UInt(8.W)
  *   val op = OpEnum()
  * }
  *
  * // Since this is a cast to a Bundle, cannot use OpCode.safe
  * val bundle = suppressEnumCastWarning {
  *   someUInt.asTypeOf(new MyBundle)
  * }
  * }}}
  */
@deprecated("This type has moved to chisel3", "Chisel 3.5")
object suppressEnumCastWarning {
  def apply[T](block: => T): T = {
    val parentWarn = Builder.suppressEnumCastWarning

    Builder.suppressEnumCastWarning = true

    val res = block // execute block

    Builder.suppressEnumCastWarning = parentWarn
    res
  }
}
