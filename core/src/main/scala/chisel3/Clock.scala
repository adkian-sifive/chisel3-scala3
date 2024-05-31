// SPDX-License-Identifier: Apache-2.0

package chisel3

import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl._
import chisel3.internal.firrtl.PrimOp.AsUIntOp

object Clock {
  def apply(): Clock = new Clock
}

// TODO: Document this.
sealed class Clock(private[chisel3] val width: Width = Width(1)) extends Element {
  override def toString: String = stringAccessor("Clock")

  def cloneType: this.type = Clock().asInstanceOf[this.type]

  private[chisel3] def typeEquivalent(that: Data): Boolean =
    this.getClass == that.getClass

  override def connect(that: Data): Unit =
    that match {
      case _: Clock | DontCare => super.connect(that)
      case _ => super.badConnect(that)
    }

  override def litOption: Option[BigInt] = None

  /** Not really supported */
  def toPrintable: Printable = PString("CLOCK")

  /** Returns the contents of the clock wire as a [[Bool]]. */
  @deprecated(
    "Calling this function with an empty argument list is invalid in Scala 3. Use the form without parentheses instead",
    "Chisel 3.5"
  )
  def asBool: Bool = this.asUInt.asBool

  override def asUInt: UInt = pushOp(
    DefPrim(UInt(this.width), AsUIntOp, ref)
  )
  private[chisel3] override def connectFromBits(
    that: Bits
  ): Unit = {
    this := that.asBool.asClock
  }
}
