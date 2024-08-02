// SPDX-License-Identifier: Apache-2.0

package chisel3

import scala.util.Try
import chisel3.experimental.BaseModule
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import _root_.firrtl.annotations.{IsModule, ModuleTarget}
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ArrayBuffer

/** Abstract base class for Modules that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
abstract class RawModule extends BaseModule {

  /** Hook to invoke hardware generators after the rest of the Module is constructed.
    *
    * This is a power-user API, and should not normally be needed.
    *
    * In rare cases, it is necessary to run hardware generators at a late stage, but still within the scope of the
    * Module. In these situations, atModuleBodyEnd may be used to register such generators. For example:
    *
    *  {{{
    *    class Example extends RawModule {
    *      atModuleBodyEnd {
    *        val extraPort0 = IO(Output(Bool()))
    *        extraPort0 := 0.B
    *      }
    *    }
    *  }}}
    *
    * Any generators registered with atModuleBodyEnd are the last code to execute when the Module is constructed. The
    * execution order is:
    *
    *   - The constructors of any super classes or traits the Module extends
    *   - The constructor of the Module itself
    *   - The atModuleBodyEnd generators
    *
    * The atModuleBodyEnd generators execute in the lexical order they appear in the Module constructor.
    *
    * For example:
    *
    *  {{{
    *    trait Parent {
    *      // Executes first.
    *      val foo = ...
    *    }
    *
    *    class Example extends Parent {
    *      // Executes second.
    *      val bar = ...
    *
    *      atModuleBodyEnd {
    *        // Executes fourth.
    *        val qux = ...
    *      }
    *
    *      atModuleBodyEnd {
    *        // Executes fifth.
    *        val quux = ...
    *      }
    *
    *      // Executes third..
    *      val baz = ...
    *    }
    *  }}}
    *
    * If atModuleBodyEnd is used in a Definition, any generated hardware will be included in the Definition. However, it
    * is currently not possible to annotate any val within atModuleBodyEnd as @public.
    */
  protected def atModuleBodyEnd(gen: => Unit): Unit = {
    _atModuleBodyEnd += { () => gen }
  }
  private val _atModuleBodyEnd = new ArrayBuffer[() => Unit]

  //
  // RTL construction internals
  //
  // Perhaps this should be an ArrayBuffer (or ArrayBuilder), but DefModule is public and has Seq[Command]
  // so our best option is to share a single Seq datastructure with that
  private val _commands = new VectorBuilder[Command]()

  /** The current region to which commands will be added. */
  private var _currentRegion = _commands

  private[chisel3] def changeRegion(newRegion: VectorBuilder[Command]): Unit = {
    _currentRegion = newRegion
  }

  private[chisel3] def withRegion[A](newRegion: VectorBuilder[Command])(thunk: => A): A = {
    var oldRegion = _currentRegion
    changeRegion(newRegion)
    val result = thunk
    changeRegion(oldRegion)
    result
  }

  private[chisel3] def addCommand(c: Command): Unit = {
    require(!_closed, "Can't write to module after module close")
    _currentRegion += c
  }
  protected def getCommands: Seq[Command] = {
    require(_closed, "Can't get commands before module close")
    // Unsafe cast but we know that any RawModule uses a DefModule
    // _component is defined as a var on BaseModule and we cannot override mutable vars
    _component.get.asInstanceOf[DefModule].commands
  }

  //
  // Other Internal Functions
  //
  private var _firrtlPorts: Option[Seq[Port]] = None

  @deprecated("Use DataMirror.modulePorts instead. this API will be removed in Chisel 3.6", "Chisel 3.5")
  lazy val getPorts: Seq[Port] = _firrtlPorts.get

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
    // case _: VerificationStatement => false
    // While the above should be comprehensive, since this is used in warning we want to be careful
    // to never accidentally have a match error
    case _ => false
  }

  private[chisel3] override def generateComponent(): Option[Component] = {
    require(!_closed, "Can't generate module more than once")
    _closed = true

    val names = nameIds(classOf[RawModule])

    // Ports get first naming priority, since they are part of a Module's IO spec
    namePorts(names)

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
        // removed till macros are fixed
        // case id: assert.Assert    => id.forceName(default = "assert", _namespace)
        // case id: assume.Assume    => id.forceName(default = "assume", _namespace)
        // case id: cover.Cover      => id.forceName(default = "cover", _namespace)
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
    }

    val firrtlPorts = getModulePorts.map { port =>
      // Special case Vec to make FIRRTL emit the direction of its
      // element.
      // Just taking the Vec's specifiedDirection is a bug in cases like
      // Vec(Flipped()), since the Vec's specifiedDirection is
      // Unspecified.
      val direction = port match {
        case v: Vec[_] =>
          v.specifiedDirection match {
            case SpecifiedDirection.Input       => SpecifiedDirection.Input
            case SpecifiedDirection.Output      => SpecifiedDirection.Output
            case SpecifiedDirection.Flip        => SpecifiedDirection.flip(v.sample_element.specifiedDirection)
            case SpecifiedDirection.Unspecified => v.sample_element.specifiedDirection
          }
        case _ => port.specifiedDirection
      }

      Port(port, direction)
    }
    _firrtlPorts = Some(firrtlPorts)

    val component = DefModule(this, name, firrtlPorts, _commands.result())
    _component = Some(component)
    _component
  }

  private[chisel3] def initializeInParent(): Unit = {}
}

trait RequireAsyncReset extends Module {
  override private[chisel3] def mkReset: AsyncReset = AsyncReset()
}

trait RequireSyncReset extends Module {
  override private[chisel3] def mkReset: Bool = Bool()
}

