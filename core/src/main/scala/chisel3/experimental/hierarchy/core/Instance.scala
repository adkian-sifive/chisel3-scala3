// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.hierarchy.core

import scala.language.experimental.macros
import chisel3._
import chisel3.experimental.hierarchy.{InstantiableClone, ModuleClone}
import chisel3.internal.{throwException, Builder}
import chisel3.experimental.BaseModule
import chisel3.internal.firrtl.{Component, DefModule, Port}
import firrtl.annotations.IsModule

import scala.annotation.nowarn

/** User-facing Instance type.
  * Represents a unique instance of type `A` which are marked as @instantiable
  * Can be created using Instance.apply method.
  *
  * @param underlying The internal representation of the instance, which may be either be directly the object, or a clone of an object
  */
final case class Instance[+A] private[chisel3] (private[chisel3] val underlying: Underlying[A])
    extends SealedHierarchy[A] {
  underlying match {
    case Proto(p: IsClone[_]) => chisel3.internal.throwException("Cannot have a Proto with a clone!")
    case other => //Ok
  }

  /** @return the context of any Data's return from inside the instance */
  private[chisel3] def getInnerDataContext: Option[BaseModule] = underlying match {
    case Proto(value: BaseModule) => Some(value)
    case Proto(value: IsInstantiable) => None
    case Clone(i: BaseModule) => Some(i)
    case Clone(i: InstantiableClone[_]) => i.getInnerContext
    case _ => throw new InternalErrorException("Match error: underlying=$underlying")
  }

  /** @return the context this instance. Note that for non-module clones, getInnerDataContext will be the same as getClonedParent */
  private[chisel3] def getClonedParent: Option[BaseModule] = underlying match {
    case Proto(value: BaseModule) => value._parent
    case Clone(i: BaseModule) => i._parent
    case Clone(i: InstantiableClone[_]) => i.getInnerContext
    case _ => throw new InternalErrorException("Match error: underlying=$underlying")
  }

  /** Used by Chisel's internal macros. DO NOT USE in your normal Chisel code!!!
    * Instead, mark the field you are accessing with [[public]]
    *
    * Given a selector function (that) which selects a member from the original, return the
    *   corresponding member from the instance.
    *
    * Our @instantiable and @public macros generate the calls to this apply method
    *
    * By calling this function, we summon the proper Lookupable typeclass from our implicit scope.
    *
    * @param that a user-specified lookup function
    * @param lookup typeclass which contains the correct lookup function, based on the types of A and B
    * @param macroGenerated a value created in the macro, to make it harder for users to use this API
    */
  def _lookup[B, C](
    that: A => B
  )(
    implicit lookup: Lookupable[B],
    macroGenerated:  chisel3.internal.MacroGenerated
  ): lookup.C = {
    lookup.instanceLookup(that, this)
  }

  /** Returns the definition of this Instance */
  override def toDefinition: Definition[A] = new Definition(Proto(proto))
  override def toInstance:   Instance[A] = this

}

/** Factory methods for constructing [[Instance]]s */
object Instance {
  implicit class InstanceBaseModuleExtensions[T <: BaseModule](i: Instance[T]) {

    /** If this is an instance of a Module, returns the toTarget of this instance
      * @return target of this instance
      */
    def toTarget: IsModule = i.underlying match {
      case Proto(x: BaseModule) => x.getTarget
      case Clone(x: (IsClone[_] & BaseModule)) => x.getTarget
      case _ => throw new InternalErrorException("Match error: i.underlying=${i.underlying}")
    }

    /** If this is an instance of a Module, returns the toAbsoluteTarget of this instance
      * @return absoluteTarget of this instance
      */
    def toAbsoluteTarget: IsModule = i.underlying match {
      case Proto(x) => x.toAbsoluteTarget
      case Clone(x: (IsClone[_] & BaseModule)) => x.toAbsoluteTarget
      case _ => throw new InternalErrorException("Match error: i.underlying=${i.underlying}")
    }

    /** Returns a FIRRTL ReferenceTarget that references this object, relative to an optional root.
      *
      * If `root` is defined, the target is a hierarchical path starting from `root`.
      *
      * If `root` is not defined, the target is a hierarchical path equivalent to `toAbsoluteTarget`.
      *
      * @note If `root` is defined, and has not finished elaboration, this must be called within `atModuleBodyEnd`.
      * @note The NamedComponent must be a descendant of `root`, if it is defined.
      * @note This doesn't have special handling for Views.
      */
    def toRelativeTarget(root: Option[BaseModule]): IsModule = i.underlying match {
      case Proto(x) => x.toRelativeTarget(root)
      case Clone(x: (IsClone[_] & BaseModule)) => x.toRelativeTarget(root)
      case _ => throw new InternalErrorException("Match error: i.underlying=${i.underlying}")
    }

    def toRelativeTargetToHierarchy(root: Option[Hierarchy[BaseModule]]): IsModule = i.underlying match {
      case Proto(x) => x.toRelativeTargetToHierarchy(root)
      case Clone(x: (IsClone[_] & BaseModule)) => x.toRelativeTargetToHierarchy(root)
      case _ => throw new InternalErrorException("Match error: i.underlying=${i.underlying}")
    }

    def suggestName(name: String): Unit = i.underlying match {
      case Clone(m: BaseModule) => m.suggestName(name)
      case Proto(m) => m.suggestName(name)
      case x        => Builder.exception(s"Cannot call .suggestName on $x")
    }
  }

  /** A constructs an [[Instance]] from a [[Definition]]
    *
    * @param definition the Module being created
    * @return an instance of the module definition
    */
  def apply[T <: BaseModule & IsInstantiable](definition: Definition[T]): Instance[T] = do_apply(definition)

  /** A constructs an [[Instance]] from a [[Definition]]
    *
    * @param definition the Module being created
    * @return an instance of the module definition
    */
  def do_apply[T <: BaseModule & IsInstantiable](
    definition: Definition[T]
  ): Instance[T] = {
    // Check to see if the module is already defined internally or externally
    val existingMod = Builder.definitions.view.map(_.proto).exists {
      case c: RawModule           => c == definition.proto
      case _ => false
    }

    // add after blackbox
    // if (!existingMod) {
    //   // Add a Definition that will get emitted as an ExtModule so that FIRRTL
    //   // does not complain about a missing element
    //   val extModName = Builder.importedDefinitionMap.getOrElse(
    //     definition.proto.name,
    //     throwException(
    //       s"Imported Definition information not found for ${definition.proto.name} - possibly forgot to add ImportDefinition annotation?"
    //     )
    //   )
    //   class EmptyExtModule extends ExtModule {
    //     override def desiredName: String = extModName
    //     override private[chisel3] def _isImportedDefinition = true
    //     override def generateComponent(): Option[Component] = {
    //       require(!_closed, s"Can't generate $desiredName module more than once")
    //       _closed = true
    //       val firrtlPorts = definition.proto.getModulePortsAndLocators.map {
    //         case (port, sourceInfo) =>
    //           Port(port, port.specifiedDirection, sourceInfo): @nowarn // Deprecated code allowed for internal use
    //       }
    //       // TODO undo after blackbox
    //       // val component = DefBlackBox(this, definition.proto.name, firrtlPorts, SpecifiedDirection.Unspecified, params)
    //       // Some(component)
    //       None
    //     }
    //   }
    //   Definition(new EmptyExtModule() {})
    // }

    val ports = experimental.CloneModuleAsRecord(definition.proto)
    val clone = ports._parent.get.asInstanceOf[ModuleClone[T]]
    clone._madeFromDefinition = true

    new Instance(Clone(clone))
  }

}
