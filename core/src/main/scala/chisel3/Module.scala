// SPDX-License-Identifier: Apache-2.0

package chisel3

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer, HashMap}

import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.experimental.BaseModule
import _root_.firrtl.annotations.{IsModule, ModuleName, ModuleTarget}
import _root_.firrtl.AnnotationSeq

object Module {

  /** A wrapper method that all Module instantiations must be wrapped in
    * (necessary to help Chisel track internal state).
    *
    * @param bc the Module being created
    *
    * @return the input module `m` with Chisel metadata properly set
    */
  def apply[T <: BaseModule](bc: => T): T = {
    if (Builder.readyForModuleConstr) {
      throwException(
        "Error: Called Module() twice without instantiating a Module."
      )
    }
    Builder.readyForModuleConstr = true

    val parent = Builder.currentModule
    val parentWhenStack = Builder.whenStack

    // Save then clear clock and reset to prevent leaking scope, must be set again in the Module
    val (saveClock, saveReset) = (Builder.currentClock, Builder.currentReset)
    val savePrefix = Builder.getPrefix
    Builder.clearPrefix()
    Builder.currentClock = None
    Builder.currentReset = None

    // Execute the module, this has the following side effects:
    //   - set currentModule
    //   - unset readyForModuleConstr
    //   - reset whenStack to be empty
    //   - set currentClockAndReset
    val module: T = bc // bc is actually evaluated here

    if (Builder.whenDepth != 0) {
      throwException("Internal Error! when() scope depth is != 0, this should have been caught!")
    }
    if (Builder.readyForModuleConstr) {
      throwException(
        "Error: attempted to instantiate a Module, but nothing happened. " +
          "This is probably due to rewrapping a Module instance with Module()."
      )
    }
    Builder.currentModule = parent // Back to parent!
    Builder.whenStack = parentWhenStack
    Builder.currentClock = saveClock // Back to clock and reset scope
    Builder.currentReset = saveReset

    // Only add the component if the module generates one
    val componentOpt = module.generateComponent()
    for (component <- componentOpt) {
      Builder.components += component
    }

    Builder.setPrefix(savePrefix)

    // Handle connections at enclosing scope
    // We use _component because Modules that don't generate them may still have one
    if (Builder.currentModule.isDefined && module._component.isDefined) {
      val component = module._component.get
      pushCommand(DefInstance(module, component.ports))
      module.initializeInParent
    }
    module
  }

  /** Returns the implicit Clock */
  def clock: Clock = Builder.forcedClock

  /** Returns the implicit Reset */
  def reset: Reset = Builder.forcedReset

  /** Returns the current Module */
  def currentModule: Option[BaseModule] = Builder.currentModule

  private[chisel3] def do_pseudo_apply[T <: BaseModule](
    bc: => T
  ): T = {
    val parent = Builder.currentModule
    val module: T = bc // bc is actually evaluated here
    if (!parent.isEmpty) { Builder.currentModule = parent }

    module
  }

  /**  Assign directionality on any IOs that are still Unspecified/Flipped
    *
    *  Chisel2 did not require explicit direction on nodes
    *  (unspecified treated as output, and flip on nothing was input).
    *  As of 3.6, chisel3 is now also using these semantics, so we need to make it work
    *  even for chisel3 code.
    *  This assigns the explicit directions required by both semantics on all Bundles.
    * This recursively walks the tree, and assigns directions if no explicit
    *   direction given by upper-levels (override Input / Output)
    */
  private[chisel3] def assignCompatDir(data: Data): Unit = {
    data match {
      case data: Element => data._assignCompatibilityExplicitDirection
      case data: Aggregate =>
        data.specifiedDirection match {
          // Recurse into children to ensure explicit direction set somewhere
          case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip =>
            data match {
              case record: Record =>
                record.elementsIterator.foreach(assignCompatDir(_))
              case vec: Vec[_] =>
                vec.elementsIterator.foreach(assignCompatDir(_))
                assignCompatDir(vec.sample_element) // This is used in fromChildren computation
            }
          case SpecifiedDirection.Input | SpecifiedDirection.Output =>
          // forced assign, nothing to do
          // The .bind algorithm will automatically assign the direction here.
          // Thus, no implicit assignment is necessary.
        }
    }
  }
}

/** Abstract base class for Modules, which behave much like Verilog modules.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note Module instantiations must be wrapped in a Module() call.
  */
abstract class Module extends RawModule {
  // Implicit clock and reset pins
  final val clock: Clock = IO(Input(Clock())).suggestName("clock")
  final val reset: Reset = IO(Input(mkReset)).suggestName("reset")

  // TODO It's hard to remove these deprecated override methods because they're used by
  //   Chisel.QueueCompatibility which extends chisel3.Queue which extends chisel3.Module
  private var _override_clock: Option[Clock] = None
  private var _override_reset: Option[Bool] = None
  @deprecated("Use withClock at Module instantiation", "Chisel 3.5")
  protected def override_clock: Option[Clock] = _override_clock
  @deprecated("Use withClock at Module instantiation", "Chisel 3.5")
  protected def override_reset: Option[Bool] = _override_reset
  @deprecated("Use withClock at Module instantiation", "Chisel 3.5")
  protected def override_clock_=(rhs: Option[Clock]): Unit = {
    _override_clock = rhs
  }
  @deprecated("Use withClock at Module instantiation", "Chisel 3.5")
  protected def override_reset_=(rhs: Option[Bool]): Unit = {
    _override_reset = rhs
  }

  private[chisel3] def mkReset: Reset = {
    // Top module and compatibility mode use Bool for reset
    // Note that a Definition elaboration will lack a parent, but still not be a Top module
    val inferReset = (_parent.isDefined)
    if (inferReset) Reset() else Bool()
  }

  // Setup ClockAndReset
  Builder.currentClock = Some(clock)
  Builder.currentReset = Some(reset)
  Builder.clearPrefix()

  private[chisel3] override def initializeInParent(): Unit = {

    super.initializeInParent
    clock := _override_clock.getOrElse(Builder.forcedClock)
    reset := _override_reset.getOrElse(Builder.forcedReset)
  }
}

package experimental {
  object IO {
    @deprecated("chisel3.experimental.IO is deprecated, use chisel3.IO instead", "Chisel 3.5")
    def apply[T <: Data](iodef: T): T = {
      chisel3.IO.apply(iodef)
    }
  }
}

package internal {
  import chisel3.experimental.BaseModule

  object BaseModule {}
}

package experimental {

  /** Abstract base class for Modules, an instantiable organizational unit for RTL.
    */
  // TODO: seal this?
  abstract class BaseModule extends HasId {
    _parent.foreach(_.addId(this))

    //
    // Builder Internals - this tracks which Module RTL construction belongs to.
    //
    this match {
      case _: PseudoModule =>
      case other =>
        if (!Builder.readyForModuleConstr) {
          throwException("Error: attempted to instantiate a Module without wrapping it in Module().")
        }
    }
    if (Builder.hasDynamicContext) {
      readyForModuleConstr = false

      Builder.currentModule = Some(this)
      Builder.whenStack = Nil
    }

    //
    // Module Construction Internals
    //
    protected var _closed = false

    /** Internal check if a Module is closed */
    private[chisel3] def isClosed = _closed

    // Fresh Namespace because in Firrtl, Modules namespaces are disjoint with the global namespace
    private[chisel3] val _namespace = Namespace.empty
    private val _ids = ArrayBuffer[HasId]()
    private[chisel3] def addId(d: HasId) = {
      require(!_closed, "Can't write to module after module close")
      _ids += d
    }

    // Returns the last id contained within a Module
    private[chisel3] def _lastId: Long = _ids.last match {
      case mod: BaseModule => mod._lastId
      case _ =>
        // Ideally we could just take last._id, but Records store and thus bind their Data in reverse order
        _ids.maxBy(_._id)._id
    }

    private[chisel3] def getIds = {
      require(_closed, "Can't get ids before module close")
      _ids.toSeq
    }

    /** Generates the FIRRTL Component (Module or Blackbox) of this Module.
      * Also closes the module so no more construction can happen inside.
      */
    private[chisel3] def generateComponent(): Option[Component]

    /** Sets up this module in the parent context
      */
    private[chisel3] def initializeInParent: Unit

    //
    // Chisel Internals
    //

    /** The desired name of this module (which will be used in generated FIRRTL IR or Verilog).
      *
      * The name of a module approximates the behavior of the Java Reflection [[`getSimpleName` method
      * https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getSimpleName--]] with some modifications:
      *
      * - Anonymous modules will get an `"_Anon"` tag
      * - Modules defined in functions will use their class name and not a numeric name
      *
      * @note If you want a custom or parametric name, override this method.
      */
    def desiredName: String = {
      /* The default module name is derived from the Java reflection derived class name. */
      val baseName = this.getClass.getName

      /* A sequence of string filters applied to the name */
      val filters: Seq[String => String] =
        Seq(((a: String) => raw"\$$+anon".r.replaceAllIn(a, "_Anon")) // Merge the "$$anon" name with previous name
        )

      filters
        .foldLeft(baseName) { case (str, filter) => filter(str) } // 1. Apply filters to baseName
        .split("\\.|\\$") // 2. Split string at '.' or '$'
        .filterNot(_.forall(_.isDigit)) // 3. Drop purely numeric names
        .last // 4. Use the last name
    }

    /** Legalized name of this module. */
    final lazy val name =
      try {
        // PseudoModules are not "true modules" and thus should share
        // their original modules names without uniquification
        this match {
          case _: PseudoModule => desiredName
          case _ => Builder.globalNamespace.name(desiredName)
        }
      } catch {
        case e: NullPointerException =>
          throwException(
            s"Error: desiredName of ${this.getClass.getName} is null. Did you evaluate 'name' before all values needed by desiredName were available?",
            e
          )
        case t: Throwable => throw t
      }

    /** Returns a FIRRTL ModuleName that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toNamed: ModuleName = ModuleTarget(this.circuitName, this.name).toNamed

    /** Returns a FIRRTL ModuleTarget that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toTarget: ModuleTarget = ModuleTarget(this.circuitName, this.name)

    private[chisel3] def getTarget: IsModule = this.toTarget

    /** Returns a FIRRTL ModuleTarget that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toAbsoluteTarget: IsModule = {
      _parent match {
        case Some(parent) => parent.toAbsoluteTarget.instOf(this.instanceName, name)
        case None         => getTarget
      }
    }

    /** Called at the Module.apply(...) level after this Module has finished elaborating.
      * Returns a map of nodes -> names, for named nodes.
      *
      * Helper method.
      */
    protected def nameIds(rootClass: Class[_]): HashMap[HasId, String] = {
      val names = new HashMap[HasId, String]()

      def name(node: HasId, name: String) = {
        // First name takes priority, like suggestName
        // TODO: DRYify with suggestName
        if (!names.contains(node)) {
          names.put(node, name)
        }
      }

      /** Scala generates names like chisel3$util$Queue$$ram for private vals
        * This extracts the part after $$ for names like this and leaves names
        * without $$ unchanged
        */
      def cleanName(name: String): String = name.split("""\$\$""").lastOption.getOrElse(name)

      for (m <- getPublicFields(rootClass)) {
        Builder.nameRecursively(cleanName(m.getName), m.invoke(this), name)
      }

      names
    }

    /** Invokes _onModuleClose on HasIds found via reflection but not bound to hardware
      * (thus not part of _ids)
      * This maintains old naming behavior for non-hardware Data
      */
    private[chisel3] def closeUnboundIds(names: HashMap[HasId, String]): Unit = {
      val idLookup = _ids.toSet
      for ((id, _) <- names if !idLookup(id)) {
        id._onModuleClose
      }
    }

    /** Compatibility function. Allows Chisel2 code which had ports without the IO wrapper to
      * compile under Bindings checks. Does nothing in non-compatibility mode.
      *
      * Should NOT be used elsewhere. This API will NOT last.
      *
      * TODO: remove this, perhaps by removing Bindings checks in compatibility mode.
      */
    def _compatAutoWrapPorts() = {}

    /** Chisel2 code didn't require the IO(...) wrapper and would assign a Chisel type directly to
      * io, then do operations on it. This binds a Chisel type in-place (mutably) as an IO.
      */
    protected def _bindIoInPlace(iodef: Data): Unit = {
      // Assign any signals (Chisel or chisel3) with Unspecified/Flipped directions to Output/Input
      Module.assignCompatDir(iodef)

      iodef.bind(PortBinding(this))
    }

    /** Private accessor for _bindIoInPlace */
    private[chisel3] def bindIoInPlace(iodef: Data): Unit = _bindIoInPlace(iodef)

    /**
      * This must wrap the datatype used to set the io field of any Module.
      * i.e. All concrete modules must have defined io in this form:
      * [lazy] val io[: io type] = IO(...[: io type])
      *
      * Items in [] are optional.
      *
      * The granted iodef must be a chisel type and not be bound to hardware.
      *
      * Also registers a Data as a port, also performing bindings. Cannot be called once ports are
      * requested (so that all calls to ports will return the same information).
      * Internal API.
      *
      * TODO(twigg): Specifically walk the Data definition to call out which nodes
      * are problematic.
      */
    protected def IO[T <: Data](iodef: T): T = chisel3.IO.apply(iodef)

    //
    // Internal Functions
    //

    /** Keep component for signal names */
    private[chisel3] var _component: Option[Component] = None

    /** Signal name (for simulation). */
    override def instanceName: String =
      if (_parent == None) name
      else
        _component match {
          case None    => getRef.name
          case Some(c) => getRef.fullName(c)
        }

  }
}
