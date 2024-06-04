// // SPDX-License-Identifier: Apache-2.0

// package chisel3.experimental

// import chisel3.Bool

// package object verification {

//   object assert {
//     @deprecated(
//       "Please use chisel3.assert instead. The chisel3.experimental.verification package will be removed.",
//       "Chisel 3.5"
//     )
//     def apply(
//       predicate: Bool,
//       msg:       String = ""
//     ): chisel3.assert.Assert = chisel3.assert(predicate, msg)
//   }

//   object assume {
//     @deprecated(
//       "Please use chisel3.assume instead. The chisel3.experimental.verification package will be removed.",
//       "Chisel 3.5"
//     )
//     def apply(
//       predicate: Bool,
//       msg:       String = ""
//     ): chisel3.assume.Assume = chisel3.assume(predicate, msg)
//   }

//   object cover {
//     @deprecated(
//       "Please use chisel3.cover instead. The chisel3.experimental.verification package will be removed.",
//       "Chisel 3.5"
//     )
//     def apply(
//       predicate: Bool,
//       msg:       String = ""
//     ): chisel3.cover.Cover = chisel3.cover(predicate, msg)
//   }
// }
