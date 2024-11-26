// SPDX-License-Identifier: Apache-2.0

package chisel3

import firrtl.annotations.{IsModule, ModuleTarget}
import chisel3.experimental.BaseModule
import chisel3.internal.firrtl.{Component, DefModule} // TODO move to object ir
import chisel3.internal.Builder.Prefix
import chisel3.ChiselException

import scala.util.Try
import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.quoted
import scala.quoted._

package object internal {
  
  @implicitNotFound("You are trying to access a macro-only API. Please use the @public annotation instead.")
  trait MacroGenerated

  /** Marker trait for modules that are not true modules */
  private[chisel3] trait PseudoModule extends BaseModule

  /* Check if a String name is a temporary name */
  def isTemp(name: String): Boolean = name.nonEmpty && name.head == '_'

  /** Creates a name String from a prefix and a seed
    * @param prefix The prefix associated with the seed (must be in correct order, *not* reversed)
    * @param seed The seed for computing the name (if available)
    */
  def buildName(seed: String, prefix: Prefix): String = {
    // Don't bother copying the String if there's no prefix
    if (prefix.isEmpty) {
      seed
    } else {
      // Using Java's String builder to micro-optimize appending a String excluding 1st character
      // for temporaries
      val builder = new java.lang.StringBuilder()
      // Starting with _ is the indicator of a temporary
      val temp = isTemp(seed)
      // Make sure the final result is also a temporary if this is a temporary
      if (temp) {
        builder.append('_')
      }
      prefix.foreach { p =>
        builder.append(p)
        builder.append('_')
      }
      if (temp) {
        // We've moved the leading _ to the front, drop it here
        builder.append(seed, 1, seed.length)
      } else {
        builder.append(seed)
      }
      builder.toString
    }
  }

  // Sanitizes a name, e.g. from a `HasId`, by stripping all non ANSI-C characters
  private[chisel3] def sanitize(s: String, leadingDigitOk: Boolean = false): String = {
    // TODO what character set does FIRRTL truly support? using ANSI C for now
    def legalStart(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    def legal(c:      Char) = legalStart(c) || (c >= '0' && c <= '9')
    val res = if (s.forall(legal)) s else s.filter(legal)
    val headOk = (!res.isEmpty) && (leadingDigitOk || legalStart(res.head))
    if (headOk) res else s"_$res"
  }

  // Workaround for https://github.com/chipsalliance/chisel/issues/4162
  // We can't use the .asTypeOf workaround because this is used to implement .asTypeOf
  private[chisel3] def _padHandleBool[A <: Bits](
    x:     A,
    width: Int
  )(using Quotes, quoted.Type[A]): A = x match {
    case b: Bool if !b.isLit && width > 1 && Type.of[A] == Type.of[UInt] =>
      val _pad = Wire(UInt(width.W))
      _pad := b
      _pad.asInstanceOf[A] // This cast is safe because we know A is UInt on this path
    case u => u.pad(width)
  }

  // Resize that to this width (if known)
  // TODO fix the "take not member of A"  error here
  // private[chisel3] def _resizeToWidth[A <: Bits](
  //   that:           A,
  //   targetWidthOpt: Option[Int]
  // )(fromUInt:       UInt => A
  // )(using Quotes): A =
  //   (targetWidthOpt, that.widthOption) match {
  //     case (Some(targetWidth), Some(thatWidth)) =>
  //       if (targetWidth == thatWidth) that
  //       else if (targetWidth > thatWidth) _padHandleBool(that, targetWidth)
  //       else fromUInt(that.take(targetWidth))
  //     case (Some(targetWidth), None) => fromUInt(_padHandleBool(that, targetWidth).take(targetWidth))
  //     case (None, Some(thatWidth))   => that
  //     case (None, None)              => that
  //   }

  /** Internal API for [[ViewParent]] */
  sealed private[chisel3] class ViewParentAPI extends RawModule() with PseudoModule {
    // We must provide `absoluteTarget` but not `toTarget` because otherwise they would be exactly
    // the same and we'd have no way to distinguish the kind of target when renaming view targets in
    // the Converter
    // Note that this is not overriding .toAbsoluteTarget, that is a final def in BaseModule that delegates
    // to this method
    private[chisel3] val absoluteTarget: IsModule = ModuleTarget(this.circuitName, "_$$AbsoluteView$$_")

    // This module is not instantiable
    override private[chisel3] def generateComponent():  Option[Component] = None
    override private[chisel3] def initializeInParent(): Unit = ()
    // This module is not really part of the circuit
    _parent = None

    // Sigil to mark views, starts with '_' to make it a legal FIRRTL target
    override def desiredName = "_$$View$$_"

    private[chisel3] val fakeComponent: Component = DefModule(this, desiredName, Nil, Nil)
  }

  /** Special internal object representing the parent of all views
    *
    * @note this is a val instead of an object because of the need to wrap in Module(...)
    * @note this is a lazy val so that calling functions in this package object doesn't create it
    */
  private[chisel3] lazy val ViewParent =
    Module.apply(new ViewParentAPI)


  // TODO this exists in cats.Traverse, should we just use that?
  private[chisel3] implicit class ListSyntax[A](xs: List[A]) {
    def mapAccumulate[B, C](z: B)(f: (B, A) => (B, C)): (B, List[C]) = {
      val (zz, result) = xs.foldLeft((z, List.empty[C])) {
        case ((acc, res), a) =>
          val (accx, c) = f(acc, a)
          (accx, c :: res)
      }
      (zz, result.reverse)
    }
  }

  /** This is effectively a "LazyVal" box type, we can create the object but delay executing the argument
    *
    * @note This is similar to cats.Eval.later but we don't depend on Cats
    */
  private[chisel3] class Delayed[A](a: => A) {
    lazy val value: A = a
  }
  private[chisel3] object Delayed {
    def apply[A](a: => A): Delayed[A] = new Delayed(a)
  }

  /** The list of banned type alias words which will cause generation of bad FIRRTL. These are usually
    * keyword tokens that would be automatically lexed by firtool, and so cause parsing errors.
    */
  private[chisel3] val illegalTypeAliases = Seq(
    "FIRRTL",
    "Clock",
    "UInt",
    "Reset",
    "AsyncReset",
    "Analog",
    "Probe",
    "RWProbe",
    "version",
    "type",
    "circuit",
    "parameter",
    "input",
    "output",
    "extmodule",
    "module",
    "intmodule",
    "intrinsic",
    "defname",
    "const",
    "flip",
    "reg",
    "smem",
    "cmem",
    "mport",
    "define",
    "attach",
    "inst",
    "of",
    "reset",
    "printf",
    "skip",
    "node"
  )

  /** Similar to Seq.groupBy except that it preserves ordering of elements within each group */
  private[chisel3] def groupByIntoSeq[A, K](xs: Iterable[A])(f: A => K): Seq[(K, Seq[A])] = {
    val map = mutable.LinkedHashMap.empty[K, mutable.ListBuffer[A]]
    for (x <- xs) {
      val key = f(x)
      val l = map.getOrElseUpdate(key, mutable.ListBuffer.empty[A])
      l += x
    }
    map.view.map({ case (k, vs) => k -> vs.toList }).toList
  }
}