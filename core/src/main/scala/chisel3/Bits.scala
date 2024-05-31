// SPDX-License-Identifier: Apache-2.0

package chisel3

import chisel3.internal._
import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl._
import chisel3.internal.firrtl.PrimOp._
import _root_.firrtl.{ir => firrtlir}
import _root_.firrtl.{constraint => firrtlconstraint}

/** Exists to unify common interfaces of [[Bits]] and [[Reset]].
  *
  * @note This is a workaround because macros cannot override abstract methods.
  */
private[chisel3] sealed trait ToBoolable extends Element {
  def asBool: Bool
}

/** A data type for values represented by a single bitvector. This provides basic bitwise operations.
  *
  * @groupdesc Bitwise Bitwise hardware operators
  * @define coll [[Bits]]
  * @define sumWidthInt    @note The width of the returned $coll is `width of this` + `that`.
  * @define sumWidth       @note The width of the returned $coll is `width of this` + `width of that`.
  * @define unchangedWidth @note The width of the returned $coll is unchanged, i.e., the `width of this`.
  */
sealed abstract class Bits(private[chisel3] val width: Width) extends Element with ToBoolable {
  // TODO: perhaps make this concrete?
  // Arguments for: self-checking code (can't do arithmetic on bits)
  // Arguments against: generates down to a FIRRTL UInt anyways

  // Only used for in a few cases, hopefully to be removed
  private[chisel3] def cloneTypeWidth(width: Width): this.type

  def cloneType: this.type = cloneTypeWidth(width)

  /** Tail operator
    *
    * @param n the number of bits to remove
    * @return This $coll with the `n` most significant bits removed.
    * @group Bitwise
    */

  /** Head operator
    *
    * @param n the number of bits to take
    * @return The `n` most significant bits of this $coll
    * @group Bitwise
    */

  def tail(n: Int): UInt = {
    val w = width match {
      case KnownWidth(x) =>
        require(x >= n, s"Can't tail($n) for width $x < $n")
        Width(x - n)
      case UnknownWidth() => Width()
    }
    binop(UInt(width = w), TailOp, n)
  }

  def head(n: Int): UInt = {
    width match {
      case KnownWidth(x)  => require(x >= n, s"Can't head($n) for width $x < $n")
      case UnknownWidth() =>
    }
    binop(UInt(Width(n)), HeadOp, n)
  }

  /** Returns the specified bit on this $coll as a [[Bool]], statically addressed.
    *
    * @param x an index
    * @return the specified bit
    */

  final def extract(x: BigInt): Bool = {
    if (x < 0) {
      Builder.error(s"Negative bit indices are illegal (got $x)")
    }
    // This preserves old behavior while a more more consistent API is under debate
    // See https://github.com/freechipsproject/chisel3/issues/867
    litOption.map { value =>
      (((value >> castToInt(x, "Index")) & 1) == 1).asBool
    }.getOrElse {
      requireIsHardware(this, "bits to be indexed")

      widthOption match {
        case Some(w) if x >= w => Builder.error(s"High index $x is out of range [0, ${w - 1}]")
        case _                 =>
      }

      pushOp(DefPrim(Bool(), BitsExtractOp, this.ref, ILit(x), ILit(x)))
    }
  }

  /** Returns the specified bit on this $coll as a [[Bool]], statically addressed.
    *
    * @param x an index
    * @return the specified bit
    */
  final def apply(x: BigInt): Bool =
    extract(x)

  /** Returns the specified bit on this $coll as a [[Bool]], statically addressed.
    *
    * @param x an index
    * @return the specified bit
    */
  final def apply(x: Int): Bool =
    extract(BigInt(x))

  /** Returns the specified bit on this wire as a [[Bool]], dynamically addressed.
    *
    * @param x a hardware component whose value will be used for dynamic addressing
    * @return the specified bit
    */
  final def extract(x: UInt): Bool = {
    val theBits = this >> x
    theBits(0)
  }

  /** Returns the specified bit on this wire as a [[Bool]], dynamically addressed.
    *
    * @param x a hardware component whose value will be used for dynamic addressing
    * @return the specified bit
    */
  final def apply(x: UInt): Bool =
    extract(x)

  /** Returns a subset of bits on this $coll from `hi` to `lo` (inclusive), statically addressed.
    *
    * @example
    * {{{
    * myBits = 0x5 = 0b101
    * myBits(1,0) => 0b01  // extracts the two least significant bits
    * }}}
    * @param x the high bit
    * @param y the low bit
    * @return a hardware component contain the requested bits
    */
  final def apply(x: Int, y: Int): UInt = {
    if (x < y || y < 0) {
      Builder.error(s"Invalid bit range ($x,$y)")
    }
    val w = x - y + 1
    // This preserves old behavior while a more more consistent API is under debate
    // See https://github.com/freechipsproject/chisel3/issues/867
    litOption.map { value =>
      ((value >> y) & ((BigInt(1) << w) - 1)).asUInt(w.W)
    }.getOrElse {
      requireIsHardware(this, "bits to be sliced")

      widthOption match {
        case Some(w) if y >= w => Builder.error(s"High and low indices $x and $y are both out of range [0, ${w - 1}]")
        case Some(w) if x >= w => Builder.error(s"High index $x is out of range [0, ${w - 1}]")
        case _                 =>
      }

      pushOp(DefPrim(UInt(Width(w)), BitsExtractOp, this.ref, ILit(x), ILit(y)))
    }
  }

  // REVIEW TODO: again, is this necessary? Or just have this and use implicits?
  /** Returns a subset of bits on this $coll from `hi` to `lo` (inclusive), statically addressed.
    *
    * @example
    * {{{
    * myBits = 0x5 = 0b101
    * myBits(1,0) => 0b01  // extracts the two least significant bits
    * }}}
    * @param x the high bit
    * @param y the low bit
    * @return a hardware component contain the requested bits
    */
  final def apply(x: BigInt, y: BigInt): UInt =
    apply(castToInt(x, "High index"), castToInt(y, "Low index"))

  private[chisel3] def unop[T <: Data](dest: T, op: PrimOp): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(dest, op, this.ref))
  }
  private[chisel3] def binop[T <: Data](dest: T, op: PrimOp, other: BigInt): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(dest, op, this.ref, ILit(other)))
  }
  private[chisel3] def binop[T <: Data](dest: T, op: PrimOp, other: Bits): T = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(DefPrim(dest, op, this.ref, other.ref))
  }
  private[chisel3] def compop(op: PrimOp, other: Bits): Bool = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(DefPrim(Bool(), op, this.ref, other.ref))
  }
  private[chisel3] def redop(op: PrimOp): Bool = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(Bool(), op, this.ref))
  }

  /** Pad operator
    *
    * @param that the width to pad to
    * @return this @coll zero padded up to width `that`. If `that` is less than the width of the original component,
    * this method returns the original component.
    * @note For [[SInt]]s only, this will do sign extension.
    * @group Bitwise
    */
  def pad(that: Int): this.type = this.width match {
    case KnownWidth(w) if w >= that => this
    case _                          => binop(cloneTypeWidth(this.width.max(Width(that))), PadOp, that)
  }

  /** Bitwise inversion operator
    *
    * @return this $coll with each bit inverted
    * @group Bitwise
    */
  def unary_~(): Bits

  /** Static left shift operator
    *
    * @param that an amount to shift by
    * @return this $coll with `that` many zeros concatenated to its least significant end
    * $sumWidthInt
    * @group Bitwise
    */
  def <<(that: BigInt): Bits

  /** Static left shift operator
    *
    * @param that an amount to shift by
    * @return this $coll with `that` many zeros concatenated to its least significant end
    * $sumWidthInt
    * @group Bitwise
    */
  def <<(that: Int): Bits

  /** Dynamic left shift operator
    *
    * @param that a hardware component
    * @return this $coll dynamically shifted left by `that` many places, shifting in zeros from the right
    * @note The width of the returned $coll is `width of this + pow(2, width of that) - 1`.
    * @group Bitwise
    */
  def <<(that: UInt): Bits

  /** Static right shift operator
    *
    * @param that an amount to shift by
    * @return this $coll with `that` many least significant bits truncated
    * $unchangedWidth
    * @group Bitwise
    */
  def >>(that: BigInt): Bits

  /** Static right shift operator
    *
    * @param that an amount to shift by
    * @return this $coll with `that` many least significant bits truncated
    * $unchangedWidth
    * @group Bitwise
    */
  def >>(that: Int): Bits

  /** Dynamic right shift operator
    *
    * @param that a hardware component
    * @return this $coll dynamically shifted right by the value of `that` component, inserting zeros into the most
    * significant bits.
    * $unchangedWidth
    * @group Bitwise
    */
  def >>(that: UInt): Bits

  /** Returns the contents of this wire as a [[scala.collection.Seq]] of [[Bool]]. */
  def asBools: Seq[Bool] =
    Seq.tabulate(this.getWidth)(i => this(i))

  /** Reinterpret this $coll as an [[SInt]]
    *
    * @note The arithmetic value is not preserved if the most-significant bit is set. For example, a [[UInt]] of
    * width 3 and value 7 (0b111) would become an [[SInt]] of width 3 and value -1.
    */
  def asSInt: SInt

  final def asBool: Bool = {
    width match {
      case KnownWidth(1) => this(0)
      case _             => throwException(s"can't covert ${this.getClass.getSimpleName}$width to Bool")
    }
  }

  /** Concatenation operator
    *
    * @param that a hardware component
    * @return this $coll concatenated to the most significant end of `that`
    * $sumWidth
    * @group Bitwise
    */
  def ##(that: Bits): UInt = {
    val w = this.width + that.width
    pushOp(DefPrim(UInt(w), ConcatOp, this.ref, that.ref))
  }

  /** Default print as [[Decimal]] */
  final def toPrintable: Printable = Decimal(this)

  protected final def validateShiftAmount(x: Int): Int = {
    if (x < 0)
      Builder.error(s"Negative shift amounts are illegal (got $x)")
    x
  }
}

object Bits {
  def apply(): UInt = apply(Width())

  /** Create a UInt port with specified width. */
  def apply(width: Width): UInt = new UInt(width)

  /** Create a UInt literal with specified width. */
  protected[chisel3] def Lit(value: BigInt, width: Width): UInt = {
    val lit = ULit(value, width)
    val result = new UInt(lit.width)
    // Bind result to being an Literal
    lit.bindLitArg(result)
  }  
}
/** A data type for unsigned integers, represented as a binary bitvector. Defines arithmetic operations between other
  * integer types.
  *
  * @define coll [[UInt]]
  * @define numType $coll
  * @define expandingWidth @note The width of the returned $coll is `width of this` + `1`.
  * @define constantWidth  @note The width of the returned $coll is unchanged, i.e., `width of this`.
  */
sealed class UInt private[chisel3] (width: Width) extends Bits(width) with Num[UInt] {
  override def toString: String = {
    litOption match {
      case Some(value) => s"UInt$width($value)"
      case _           => stringAccessor(s"UInt$width")
    }
  }

  private[chisel3] override def typeEquivalent(that: Data): Boolean =
    that.isInstanceOf[UInt] && this.width == that.width

  private[chisel3] override def cloneTypeWidth(w: Width): this.type =
    new UInt(w).asInstanceOf[this.type]

  // TODO: refactor to share documentation with Num or add independent scaladoc
  /** Unary negation (expanding width)
    *
    * @return a $coll equal to zero minus this $coll
    * $constantWidth
    * @group Arithmetic
    */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  /** Unary negation (constant width)
    *
    * @return a $coll equal to zero minus this $coll shifted right by one.
    * $constantWidth
    * @group Arithmetic
    */

  def unary_-(): UInt = 0.U - this


  def unary_-%(): UInt = 0.U -% this

  override def +(that: UInt): UInt = this +% that
  override def -(that: UInt): UInt = this -% that
  override def /(that: UInt): UInt =
    binop(UInt(this.width), DivideOp, that)
  override def %(that: UInt): UInt =
    binop(UInt(this.width.min(that.width)), RemOp, that)
  override def *(that: UInt): UInt =
    binop(UInt(this.width + that.width), TimesOp, that)

  /** Multiplication operator
    *
    * @param that a hardware [[SInt]]
    * @return the product of this $coll and `that`
    * $sumWidth
    * $singleCycleMul
    * @group Arithmetic
    */
  def *(that: SInt): SInt = that * this

  /** Addition operator (expanding width)
    *
    * @param that a hardware $coll
    * @return the sum of this $coll and `that`
    * $maxWidthPlusOne
    * @group Arithmetic
    */

  /** Addition operator (constant width)
    *
    * @param that a hardware $coll
    * @return the sum of this $coll and `that`
    * $maxWidth
    * @group Arithmetic
    */

  /** Subtraction operator (increasing width)
    *
    * @param that a hardware $coll
    * @return the difference of this $coll less `that`
    * $maxWidthPlusOne
    * @group Arithmetic
    */

  /** Subtraction operator (constant width)
    *
    * @param that a hardware $coll
    * @return the difference of this $coll less `that`
    * $maxWidth
    * @group Arithmetic
    */

  def +&(that: UInt): UInt =
    binop(UInt((this.width.max(that.width)) + 1), AddOp, that)

  def +%(that: UInt): UInt =
    (this +& that).tail(1)

  def -&(that: UInt): UInt =
    (this.subtractAsSInt(that)).asUInt

  def -%(that: UInt): UInt =
    (this.subtractAsSInt(that)).tail(1)

  /** Bitwise and operator
    *
    * @param that a hardware $coll
    * @return the bitwise and of  this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */

  /** Bitwise or operator
    *
    * @param that a hardware $coll
    * @return the bitwise or of this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */

  /** Bitwise exclusive or (xor) operator
    *
    * @param that a hardware $coll
    * @return the bitwise xor of this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */

  def abs: UInt = this

  def &(that: UInt): UInt =
    binop(UInt(this.width.max(that.width)), BitAndOp, that)

  def |(that: UInt): UInt =
    binop(UInt(this.width.max(that.width)), BitOrOp, that)

  def ^(that: UInt): UInt =
    binop(UInt(this.width.max(that.width)), BitXorOp, that)

  def unary_~(): UInt =
    unop(UInt(width = width), BitNotOp)

  // REVIEW TODO: Can these be defined on Bits?
  /** Or reduction operator
    *
    * @return a hardware [[Bool]] resulting from every bit of this $coll or'd together
    * @group Bitwise
    */

  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  /** And reduction operator
    *
    * @return a hardware [[Bool]] resulting from every bit of this $coll and'd together
    * @group Bitwise
    */

  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  /** Exclusive or (xor) reduction operator
    *
    * @return a hardware [[Bool]] resulting from every bit of this $coll xor'd together
    * @group Bitwise
    */

  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  def orR: Bool = redop(OrReduceOp)

  def andR: Bool = redop(AndReduceOp)

  def xorR: Bool = redop(XorReduceOp)

  override def <(that: UInt): Bool =
    compop(LessOp, that)
  override def >(that: UInt): Bool =
    compop(GreaterOp, that)
  override def <=(that: UInt): Bool =
    compop(LessEqOp, that)
  override def >=(that: UInt): Bool =
    compop(GreaterEqOp, that)

  /** Dynamic not equals operator
    *
    * @param that a hardware $coll
    * @return a hardware [[Bool]] asserted if this $coll is not equal to `that`
    * @group Comparison
    */

  /** Dynamic equals operator
    *
    * @param that a hardware $coll
    * @return a hardware [[Bool]] asserted if this $coll is equal to `that`
    * @group Comparison
    */
  def =/=(that: UInt): Bool =
    compop(NotEqualOp, that)

  def ===(that: UInt): Bool =
    compop(EqualOp, that)

  /** Unary not
    *
    * @return a hardware [[Bool]] asserted if this $coll equals zero
    * @group Bitwise
    */

  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  def unary_!(): Bool = this === 0.U(1.W)

  override def <<(that: Int): UInt =
    binop(UInt(this.width + that), ShiftLeftOp, validateShiftAmount(that))
  override def <<(that: BigInt): UInt =
    this << castToInt(that, "Shift amount")
  override def <<(that: UInt): UInt =
    binop(UInt(this.width.dynamicShiftLeft(that.width)), DynamicShiftLeftOp, that)
  override def >>(that: Int): UInt =
    binop(UInt(this.width.shiftRight(that)), ShiftRightOp, validateShiftAmount(that))
  override def >>(that: BigInt): UInt =
    this >> castToInt(that, "Shift amount")
  override def >>(that: UInt): UInt =
    binop(UInt(this.width), DynamicShiftRightOp, that)

  /**
    * Circular shift to the left
    * @param that number of bits to rotate
    * @return UInt of same width rotated left n bits
    */
  def rotateLeft(n: Int): UInt = width match {
    case _ if (n == 0)             => this
    case KnownWidth(w) if (w <= 1) => this
    case KnownWidth(w) if n >= w   => rotateLeft(n % w)
    case _ if (n < 0)              => rotateRight(-n)
    case _                         => tail(n) ## head(n)
  }

  /**
    * Circular shift to the right
    * @param that number of bits to rotate
    * @return UInt of same width rotated right n bits
    */
  def rotateRight(n: Int): UInt = width match {
    case _ if (n <= 0)             => rotateLeft(-n)
    case KnownWidth(w) if (w <= 1) => this
    case KnownWidth(w) if n >= w   => rotateRight(n % w)
    case _                         => this(n - 1, 0) ## (this >> n)
  }

  private def dynamicShift(
    n:           UInt,
    staticShift: (UInt, Int) => UInt
  ): UInt =
    n.asBools().zipWithIndex.foldLeft(this) {
      case (in, (en, sh)) => Mux(en, staticShift(in, 1 << sh), in)
    }

  def rotateRight(n: UInt): UInt =
    dynamicShift(n, _ rotateRight _)

  def rotateLeft(n: UInt): UInt =
    dynamicShift(n, _ rotateLeft _)

  /** Conditionally set or clear a bit
    *
    * @param off a dynamic offset
    * @param dat set if true, clear if false
    * @return a hrdware $coll with bit `off` set or cleared based on the value of `dat`
    * $unchangedWidth
    */
  // def bitSet(off: UInt, dat: Bool): UInt = {
  //   val bit = 1.U(1.W) << off
  //   Mux(dat, this | bit, ~(~(this()) | bit))
  // }

  // TODO: this eventually will be renamed as toSInt, once the existing toSInt
  // completes its deprecation phase.
  /** Zero extend as [[SInt]]
    *
    * @return an [[SInt]] equal to this $coll with an additional zero in its most significant bit
    * @note The width of the returned [[SInt]] is `width of this` + `1`.
    */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )
  def zext: SInt =
    pushOp(DefPrim(SInt(width + 1), ConvertOp, ref))

  override def asSInt: SInt =
    pushOp(DefPrim(SInt(width), AsSIntOp, ref))
  override def asUInt: UInt = this

  private[chisel3] override def connectFromBits(
    that: Bits
  ): Unit = {
    this := that.asUInt
  }

  private def subtractAsSInt(that: UInt): SInt =
    binop(SInt((this.width.max(that.width)) + 1), SubOp, that)
}

object UInt {
  def apply(): UInt = apply(Width())

  /** Create a UInt port with specified width. */
  def apply(width: Width): UInt = new UInt(width)

  /** Create a UInt literal with specified width. */
  protected[chisel3] def Lit(value: BigInt, width: Width): UInt = {
    val lit = ULit(value, width)
    val result = new UInt(lit.width)
    // Bind result to being an Literal
    lit.bindLitArg(result)
  }
}
/** A data type for signed integers, represented as a binary bitvector. Defines arithmetic operations between other
  * integer types.
  *
  * @define coll [[SInt]]
  * @define numType $coll
  * @define expandingWidth @note The width of the returned $coll is `width of this` + `1`.
  * @define constantWidth  @note The width of the returned $coll is unchanged, i.e., `width of this`.
  */
sealed class SInt private[chisel3] (width: Width) extends Bits(width) with Num[SInt] {
  override def toString: String = {
    litOption match {
      case Some(value) => s"SInt$width($value)"
      case _           => stringAccessor(s"SInt$width")
    }
  }

  
  private[chisel3] override def typeEquivalent(that: Data): Boolean =
    this.getClass == that.getClass && this.width == that.width // TODO: should this be true for unspecified widths?

  private[chisel3] override def cloneTypeWidth(w: Width): this.type =
    new SInt(w).asInstanceOf[this.type]

  /** Unary negation (constant width)
    *
    * @return a hardware $coll equal to zero minus this $coll
    * $constantWidth
    * @group Arithmetic
    */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )
  /** Unary negation (constant width)
    *
    * @return a hardware $coll equal to zero minus `this` shifted right by one
    * $constantWidth
    * @group Arithmetic
    */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )

  def unary_-(): SInt = 0.S - this

  def unary_-%(): SInt = 0.S -% this

  /** add (default - no growth) operator */
  override def +(that: SInt): SInt =
    this +% that

  /** subtract (default - no growth) operator */
  override def -(that: SInt): SInt =
    this -% that
  override def *(that: SInt): SInt =
    binop(SInt(this.width + that.width), TimesOp, that)
  override def /(that: SInt): SInt =
    binop(SInt(this.width + 1), DivideOp, that)
  override def %(that: SInt): SInt =
    binop(SInt(this.width.min(that.width)), RemOp, that)

  /** Multiplication operator
    *
    * @param that a hardware $coll
    * @return the product of this $coll and `that`
    * $sumWidth
    * $singleCycleMul
    * @group Arithmetic
    */
  def *(that: UInt): SInt = {
    val thatToSInt = that.zext
    val result = binop(SInt(this.width + thatToSInt.width), TimesOp, thatToSInt)
    result.tail(1).asSInt
  }

  /** Addition operator (expanding width)
    *
    * @param that a hardware $coll
    * @return the sum of this $coll and `that`
    * $maxWidthPlusOne
    * @group Arithmetic
    */
  /** Addition operator (constant width)
    *
    * @param that a hardware $coll
    * @return the sum of this $coll and `that` shifted right by one
    * $maxWidth
    * @group Arithmetic
    */
  /** Subtraction operator (increasing width)
    *
    * @param that a hardware $coll
    * @return the difference of this $coll less `that`
    * $maxWidthPlusOne
    * @group Arithmetic
    */
  /** Subtraction operator (constant width)
    *
    * @param that a hardware $coll
    * @return the difference of this $coll less `that` shifted right by one
    * $maxWidth
    * @group Arithmetic
    */
  def +&(that: SInt): SInt =
    binop(SInt((this.width.max(that.width)) + 1), AddOp, that)

  def +%(that: SInt): SInt =
    (this +& that).tail(1).asSInt

  def -&(that: SInt): SInt =
    binop(SInt((this.width.max(that.width)) + 1), SubOp, that)

  def -%(that: SInt): SInt =
    (this -& that).tail(1).asSInt

  /** Bitwise and operator
    *
    * @param that a hardware $coll
    * @return the bitwise and of  this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */
  /** Bitwise or operator
    *
    * @param that a hardware $coll
    * @return the bitwise or of this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */
  /** Bitwise exclusive or (xor) operator
    *
    * @param that a hardware $coll
    * @return the bitwise xor of this $coll and `that`
    * $maxWidth
    * @group Bitwise
    */
  def &(that: SInt): SInt =
    binop(UInt(this.width.max(that.width)), BitAndOp, that).asSInt

  def |(that: SInt): SInt =
    binop(UInt(this.width.max(that.width)), BitOrOp, that).asSInt

  def ^(that: SInt): SInt =
    binop(UInt(this.width.max(that.width)), BitXorOp, that).asSInt

  def unary_~(): SInt =
    unop(UInt(width = width), BitNotOp).asSInt

  override def <(that: SInt): Bool =
    compop(LessOp, that)
  override def >(that: SInt): Bool =
    compop(GreaterOp, that)
  override def <=(that: SInt): Bool =
    compop(LessEqOp, that)
  override def >=(that: SInt): Bool =
    compop(GreaterEqOp, that)

  /** Dynamic not equals operator
    *
    * @param that a hardware $coll
    * @return a hardware [[Bool]] asserted if this $coll is not equal to `that`
    * @group Comparison
    */
  /** Dynamic equals operator
    *
    * @param that a hardware $coll
    * @return a hardware [[Bool]] asserted if this $coll is equal to `that`
    * @group Comparison
    */
  def =/=(that: SInt): Bool =
    compop(NotEqualOp, that)

  def ===(that: SInt): Bool =
    compop(EqualOp, that)

  def abs: SInt = {
    Mux(this < 0.S, -this, this)
  }

  override def <<(that: Int): SInt =
    binop(SInt(this.width + that), ShiftLeftOp, validateShiftAmount(that))
  override def <<(that: BigInt): SInt =
    this << castToInt(that, "Shift amount")
  override def <<(that: UInt): SInt =
    binop(SInt(this.width.dynamicShiftLeft(that.width)), DynamicShiftLeftOp, that)
  override def >>(that: Int): SInt =
    binop(SInt(this.width.shiftRight(that)), ShiftRightOp, validateShiftAmount(that))
  override def >>(that: BigInt): SInt =
    this >> castToInt(that, "Shift amount")
  override def >>(that: UInt): SInt =
    binop(SInt(this.width), DynamicShiftRightOp, that)

  override def asUInt: UInt = pushOp(
    DefPrim(UInt(this.width), AsUIntOp, ref)
  )
  override def asSInt: SInt = this

  private[chisel3] override def connectFromBits(
    that: Bits
  ) = {
    this := that.asSInt
  }
}

object SInt {
  /** Create an SInt type with inferred width. */
  def apply(): SInt = apply(Width())

  /** Create a SInt type or port with fixed width. */
  def apply(width: Width): SInt = new SInt(width)

  /** Create an SInt literal with specified width. */
  protected[chisel3] def Lit(value: BigInt, width: Width): SInt = {
    val lit = SLit(value, width)
    val result = new SInt(lit.width)
    lit.bindLitArg(result)
  }
}

sealed trait Reset extends Element with ToBoolable {

  /** Casts this $coll to an [[AsyncReset]] */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )
  def asAsyncReset: AsyncReset
}

object Reset {
  def apply(): Reset = new ResetType
}

/** "Abstract" Reset Type inferred in FIRRTL to either [[AsyncReset]] or [[Bool]]
  *
  * @note This shares a common interface with [[AsyncReset]] and [[Bool]] but is not their actual
  * super type due to Bool inheriting from abstract class UInt
  */
final class ResetType(private[chisel3] val width: Width = Width(1)) extends Element with Reset {
  override def toString: String = stringAccessor("Reset")

  def cloneType: this.type = Reset().asInstanceOf[this.type]

  private[chisel3] def typeEquivalent(that: Data): Boolean =
    this.getClass == that.getClass

  override def litOption = None

  /** Not really supported */
  def toPrintable: Printable = PString("Reset")

  override def do_asUInt: UInt = pushOp(
    DefPrim(UInt(this.width), AsUIntOp, ref)
  )

  private[chisel3] override def connectFromBits(
    that: Bits
  ): Unit = {
    this := that
  }

  def asAsyncReset: AsyncReset =
    pushOp(DefPrim(AsyncReset(), AsAsyncResetOp, ref))

  def asBool: Bool =
    pushOp(DefPrim(Bool(), AsUIntOp, ref))

  def toBool: Bool = asBool
}

object AsyncReset {
  def apply(): AsyncReset = new AsyncReset
}

/** Data type representing asynchronous reset signals
  *
  * These signals are similar to [[Clock]]s in that they must be glitch-free for proper circuit
  * operation. [[Reg]]s defined with the implicit reset being an [[AsyncReset]] will be
  * asychronously reset registers.
  */
sealed class AsyncReset(private[chisel3] val width: Width = Width(1)) extends Element with Reset {
  override def toString: String = stringAccessor("AsyncReset")

  def cloneType: this.type = AsyncReset().asInstanceOf[this.type]

  private[chisel3] def typeEquivalent(that: Data): Boolean =
    this.getClass == that.getClass

  override def litOption = None

  /** Not really supported */
  def toPrintable: Printable = PString("AsyncReset")

  override def asUInt: UInt = pushOp(
    DefPrim(UInt(this.width), AsUIntOp, ref)
  )

  // TODO Is this right?
  private[chisel3] override def connectFromBits(
    that: Bits
  ): Unit = {
    this := that.asBool.asAsyncReset
  }

  def asAsyncReset: AsyncReset = this

  def asBool: Bool =
    pushOp(DefPrim(Bool(), AsUIntOp, ref))

  def toBool: Bool = asBool
}

// REVIEW TODO: Why does this extend UInt and not Bits? Does defining airth
// operations on a Bool make sense?
/** A data type for booleans, defined as a single bit indicating true or false.
  *
  * @define coll [[Bool]]
  * @define numType $coll
  */
sealed class Bool() extends UInt(1.W) with Reset {
  override def toString: String = {
    litToBooleanOption match {
      case Some(value) => s"Bool($value)"
      case _           => stringAccessor("Bool")
    }
  }

  private[chisel3] override def cloneTypeWidth(w: Width): this.type = {
    require(!w.known || w.get == 1)
    new Bool().asInstanceOf[this.type]
  }

  /** Convert to a [[scala.Option]] of [[scala.Boolean]] */
  def litToBooleanOption: Option[Boolean] = litOption.map {
    case intVal if intVal == 1 => true
    case intVal if intVal == 0 => false
    case intVal                => throwException(s"Boolean with unexpected literal value $intVal")
  }

  /** Convert to a [[scala.Boolean]] */
  def litToBoolean: Boolean = litToBooleanOption.get

  // REVIEW TODO: Why does this need to exist and have different conventions
  // than Bits?

  /** Bitwise and operator
    *
    * @param that a hardware $coll
    * @return the bitwise and of  this $coll and `that`
    * @group Bitwise
    */

  /** Bitwise or operator
    *
    * @param that a hardware $coll
    * @return the bitwise or of this $coll and `that`
    * @group Bitwise
    */

  /** Bitwise exclusive or (xor) operator
    *
    * @param that a hardware $coll
    * @return the bitwise xor of this $coll and `that`
    * @group Bitwise
    */

  def &(that: Bool): Bool =
    binop(Bool(), BitAndOp, that)

  def |(that: Bool): Bool =
    binop(Bool(), BitOrOp, that)

  def ^(that: Bool): Bool =
    binop(Bool(), BitXorOp, that)

  override def unary_~(): Bool =
    unop(Bool(), BitNotOp)

  /** Logical or operator
    *
    * @param that a hardware $coll
    * @return the logical or of this $coll and `that`
    * @note this is equivalent to [[Bool!.|(that:chisel3\.Bool)* Bool.|)]]
    * @group Logical
    */

  def ||(that: Bool): Bool = this | that

  /** Logical and operator
    *
    * @param that a hardware $coll
    * @return the logical and of this $coll and `that`
    * @note this is equivalent to [[Bool!.&(that:chisel3\.Bool)* Bool.&]]
    * @group Logical
    */
  def &&(that: Bool): Bool = this & that

  /** Reinterprets this $coll as a clock */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )
  def asClock: Clock = pushOp(
    DefPrim(Clock(), AsClockOp, ref)
  )

  def asAsyncReset: AsyncReset =
    pushOp(DefPrim(AsyncReset(), AsAsyncResetOp, ref))
}

object Bool {
  def Lit(x: Boolean): Bool = {
    val result = new Bool()
    val lit = ULit(if (x) 1 else 0, Width(1))
    lit.bindLitArg(result)
  }
  def apply(): Bool = new Bool()
}

package experimental {

  import chisel3.internal.firrtl.BinaryPoint

  /** Chisel types that have binary points support retrieving
    * literal values as `Double` or `BigDecimal`
    */
  trait HasBinaryPoint { self: Bits =>
    def binaryPoint: BinaryPoint

    /** Return the [[Double]] value of this instance if it is a Literal
      * @note this method may throw an exception if the literal value won't fit in a Double
      */
    def litToDoubleOption: Option[Double] = {
      litOption match {
        case Some(bigInt: BigInt) =>
          Some(Num.toDouble(bigInt, binaryPoint))
        case _ => None
      }
    }

    /** Return the double value of this instance assuming it is a literal (convenience method)
      */
    def litToDouble: Double = litToDoubleOption.get

    /** Return the [[BigDecimal]] value of this instance if it is a Literal
      * @note this method may throw an exception if the literal value won't fit in a BigDecimal
      */
    def litToBigDecimalOption: Option[BigDecimal] = {
      litOption match {
        case Some(bigInt: BigInt) =>
          Some(Num.toBigDecimal(bigInt, binaryPoint))
        case _ => None
      }
    }

    /** Return the [[BigDecimal]] value of this instance assuming it is a literal (convenience method)
      * @return
      */
    def litToBigDecimal: BigDecimal = litToBigDecimalOption.get
  }
}
