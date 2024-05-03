// SPDX-License-Identifier: Apache-2.0

package chisel3

import scala.util.Try
import scala.language.experimental.macros
import scala.annotation.nowarn
import chisel3.experimental.BaseModule
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.UnlocatableSourceInfo
import _root_.firrtl.annotations.{IsModule, ModuleTarget}
import scala.collection.immutable.VectorBuilder

/** Abstract base class for Modules that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
@nowarn("msg=class Port") // delete when Port becomes private
abstract class RawModule(implicit moduleCompileOptions: CompileOptions) extends BaseModule {
  //
  // RTL construction internals
  //
  // Perhaps this should be an ArrayBuffer (or ArrayBuilder), but DefModule is public and has Seq[Command]
  // so our best option is to share a single Seq datastructure with that
  private val _commands = new VectorBuilder[Command]()
  private[chisel3] def addCommand(c: Command) {
    require(!_closed, "Can't write to module after module close")
    _commands += c
  }
  protected def getCommands: Seq[Command] = {
    require(_closed, "Can't get commands before module close")
    // Unsafe cast but we know that any RawModule uses a DefModule
    // _component is defined as a var on BaseModule and we cannot override mutable vars
    _component.get.asInstanceOf[DefModule].commands
  }

  val compileOptions = moduleCompileOptions

  // This could be factored into a common utility
  private def canBeNamed(id: HasId): Boolean = id match {
    case d: Data =>
      d.binding match {
        case Some(_: ConstrainedBinding) => true
        case _ => false
      }
    case b: BaseModule => true
    case m: MemBase[_] => true
    // These names don't affect hardware
    case _: VerificationStatement => false
    // While the above should be comprehensive, since this is used in warning we want to be careful
    // to never accidentally have a match error
    case _ => false
  }

  private[chisel3] override def generateComponent(): Option[Component] = {
    require(!_closed, "Can't generate module more than once")
    _closed = true

    val names = nameIds(classOf[RawModule])

    // Then everything else gets named
    val warnReflectiveNaming = Builder.warnReflectiveNaming
    for ((node, name) <- names) {
      node match {
        case d: HasId if warnReflectiveNaming && canBeNamed(d) =>
          val result = d._suggestNameCheck(name)
          result match {
            case None => // All good, no warning
            case Some((oldName, oldPrefix)) =>
              val prevName = buildName(oldName, oldPrefix.reverse)
              val newName = buildName(name, Nil)
              val msg = s"[module ${this.name}] '$prevName' is renamed by reflection to '$newName'. " +
                s"Chisel 3.6 removes reflective naming so the name will remain '$prevName'."
              Builder.warningNoLoc(msg)
          }
        // Note that unnamable things end up here (eg. literals), this is supporting backwards
        // compatibility
        case _ => node.suggestName(name)
      }
    }

    // All suggestions are in, force names to every node.
    for (id <- getIds) {
      id match {
        case id: BaseModule       => id.forceName(default = id.desiredName, _namespace)
        case id: MemBase[_]       => id.forceName(default = "MEM", _namespace)
        case id: stop.Stop        => id.forceName(default = "stop", _namespace)
        case id: assert.Assert    => id.forceName(default = "assert", _namespace)
        case id: assume.Assume    => id.forceName(default = "assume", _namespace)
        case id: cover.Cover      => id.forceName(default = "cover", _namespace)
        case id: printf.Printf => id.forceName(default = "printf", _namespace)
        case id: Data =>
          if (id.isSynthesizable) {
            id.topBinding match {
              case OpBinding(_, _) =>
                id.forceName(default = "_T", _namespace)
              case MemoryPortBinding(_, _) =>
                id.forceName(default = "MPORT", _namespace)
              case PortBinding(_) =>
                id.forceName(default = "PORT", _namespace)
              case RegBinding(_, _) =>
                id.forceName(default = "REG", _namespace)
              case WireBinding(_, _) =>
                id.forceName(default = "_WIRE", _namespace)
              case _ => // don't name literals
            }
          } // else, don't name unbound types
      }
      id._onModuleClose
    }

    closeUnboundIds(names)

    val component = DefModule(this, name, null, Seq.empty)
    _component = Some(component)
    _component
  }

  private[chisel3] def initializeInParent(parentCompileOptions: CompileOptions): Unit = {}
}

trait RequireAsyncReset extends Module {
  override private[chisel3] def mkReset: AsyncReset = AsyncReset()
}

trait RequireSyncReset extends Module {
  override private[chisel3] def mkReset: Bool = Bool()
}

/** Mix with a [[RawModule]] to automatically connect DontCare to the module's ports, wires, and children instance IOs. */

package object internal {

  import scala.annotation.implicitNotFound
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
}
