// // SPDX-License-Identifier: Apache-2.0

// package chisel3

// trait CompileOptions {

//   /** Should Record connections require a strict match of fields.
//     *
//     * If true and the same fields aren't present in both source and sink, a MissingFieldException,
//     * MissingLeftFieldException, or MissingRightFieldException will be thrown.
//     */
//   val connectFieldsMustMatch: Boolean

//   /** When creating an object that takes a type argument, the argument must be unbound (a pure type). */
//   val declaredTypeMustBeUnbound: Boolean

//   /** If a connection operator fails, don't try the connection with the operands (source and sink) reversed. */
//   val dontTryConnectionsSwapped: Boolean

//   /** If connection directionality is not explicit, do not use heuristics to attempt to determine it. */
//   val dontAssumeDirectionality: Boolean

//   /** Check that referenced Data have actually been declared. */
//   val checkSynthesizable: Boolean

//   /** Require explicit assignment of DontCare to generate "x is invalid" */
//   val explicitInvalidate: Boolean

//   /** Should the reset type of Module be a Bool or a Reset */
//   val inferModuleReset: Boolean

//   /** If marked true, then any Module which consumes `inferModuleReset=false` must also mix in [[RequireSyncReset]] */
//   def migrateInferModuleReset: Boolean = false

//   /** Should connects emit as firrtl <= instead of <- */
//   def emitStrictConnects: Boolean = true
// }

// object CompileOptions {

// }

// object ExplicitCompileOptions {

//   case class CompileOptionsClass(
//     // Should Record connections require a strict match of fields.
//     // If true and the same fields aren't present in both source and sink, a MissingFieldException,
//     // MissingLeftFieldException, or MissingRightFieldException will be thrown.
//     val connectFieldsMustMatch: Boolean,
//     // When creating an object that takes a type argument, the argument must be unbound (a pure type).
//     val declaredTypeMustBeUnbound: Boolean,
//     // If a connection operator fails, don't try the connection with the operands (source and sink) reversed.
//     val dontTryConnectionsSwapped: Boolean,
//     // If connection directionality is not explicit, do not use heuristics to attempt to determine it.
//     val dontAssumeDirectionality: Boolean,
//     // Check that referenced Data have actually been declared.
//     val checkSynthesizable: Boolean,
//     // Require an explicit DontCare assignment to generate a firrtl DefInvalid
//     val explicitInvalidate: Boolean,
//     // Should the reset type of Module be a Bool or a Reset
//     val inferModuleReset: Boolean)
//       extends CompileOptions

//   // Collection of "not strict" connection compile options.
//   // These provide compatibility with existing code.
//   implicit val NotStrict: CompileOptionsClass = new CompileOptionsClass(
//     connectFieldsMustMatch = false,
//     declaredTypeMustBeUnbound = false,
//     dontTryConnectionsSwapped = false,
//     dontAssumeDirectionality = false,
//     checkSynthesizable = false,
//     explicitInvalidate = false,
//     inferModuleReset = false
//   ) {
//     override def migrateInferModuleReset = false
//     override def emitStrictConnects = false
//     override def copy(
//       connectFieldsMustMatch:    Boolean = false,
//       declaredTypeMustBeUnbound: Boolean = false,
//       dontTryConnectionsSwapped: Boolean = false,
//       dontAssumeDirectionality:  Boolean = false,
//       checkSynthesizable:        Boolean = false,
//       explicitInvalidate:        Boolean = false,
//       inferModuleReset:          Boolean = false
//     ) = new CompileOptionsClass(
//       connectFieldsMustMatch,
//       declaredTypeMustBeUnbound,
//       dontTryConnectionsSwapped,
//       dontAssumeDirectionality,
//       checkSynthesizable,
//       explicitInvalidate,
//       inferModuleReset
//     ) {
//       override def migrateInferModuleReset = false
//       override def emitStrictConnects = false
//     }
//   }

//   // Collection of "strict" connection compile options, preferred for new code.
//   implicit val Strict: CompileOptionsClass = new CompileOptionsClass(
//     connectFieldsMustMatch = true,
//     declaredTypeMustBeUnbound = true,
//     dontTryConnectionsSwapped = true,
//     dontAssumeDirectionality = true,
//     checkSynthesizable = true,
//     explicitInvalidate = true,
//     inferModuleReset = true
//   ) {

//     override def migrateInferModuleReset = false
//     override def emitStrictConnects = true

//     override def copy(
//       connectFieldsMustMatch:    Boolean = true,
//       declaredTypeMustBeUnbound: Boolean = true,
//       dontTryConnectionsSwapped: Boolean = true,
//       dontAssumeDirectionality:  Boolean = true,
//       checkSynthesizable:        Boolean = true,
//       explicitInvalidate:        Boolean = true,
//       inferModuleReset:          Boolean = true
//     ) = new CompileOptionsClass(
//       connectFieldsMustMatch,
//       declaredTypeMustBeUnbound,
//       dontTryConnectionsSwapped,
//       dontAssumeDirectionality,
//       checkSynthesizable,
//       explicitInvalidate,
//       inferModuleReset
//     ) {
//       override def migrateInferModuleReset = false
//       override def emitStrictConnects = true
//     }
//   }
// }
