import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class DataBox[D <: Data](val a: D)

trait Monoid[D <: Data]:
  def unit(a: DataBox[D]): DataBox[D]
  def transform(a: DataBox[D]): DataBox[D]

given UIntMonoid: Monoid[UInt] with
  def unit(a: DataBox[UInt]): DataBox[UInt] = DataBox(a.a)
  def transform(a: DataBox[UInt]): DataBox[UInt] = DataBox(a.a+1.U)
  // def unit: DataBox[UInt] = DataBox(1.U)

given BoolMonoid: Monoid[Bool] with
  def unit(a: DataBox[Bool]): DataBox[Bool] = DataBox(a.a)
  def transform(a: DataBox[Bool]): DataBox[Bool] = DataBox(!a.a)

class AxiPorts[D <: Data](
  
)

// this typer should be part of the library code, there needs to be a
// way for users to get typers for their own datatypes in addition to
// being able to use built-in types
enum Typer[D <: Data](unit: DataBox[D] => DataBox[D], transform: DataBox[D] => DataBox[D]):
  case UIntTyper extends Typer(UIntMonoid.unit, UIntMonoid.transform)
  case BoolTyper extends Typer(BoolMonoid.unit, BoolMonoid.transform)
  def boxUnit(a: D): DataBox[D] = unit(DataBox(a))
  def boxTransform(a: D): DataBox[D] = transform(DataBox(a))

class Tree extends Module {
  import Typer._
  // ad-hoc typer
  given SIntTyper: Monoid[SInt] with
      def unit(a: DataBox[SInt]) = DataBox(a.a)
      def transform(a: DataBox[SInt]) = DataBox(a.a-1.S)

  val myData: UInt = 1.U
  val typeBoxedUInt = Typer.UIntTyper.boxUnit(myData) // myData.type
  val typeBoxedBool = Typer.BoolTyper.boxUnit(true.B)

  val a, b, c = IO(Input(typeBoxedBool.a))
  val foo, bar = IO(Input(UIntTyper.boxTransform(typeBoxedUInt.a).a))

  def sint_typer(using Monoid[SInt]) =
    val typeBoxedSInt = SIntTyper.unit(DataBox(-128.S))
    val fizz = IO(Input(typeBoxedSInt.a))
    fizz

  val fizz = sint_typer

  val out = IO(Output(UInt(8.W)))
  val myReg = RegInit(0.U(8.W))

  when(a && b && c) {
    myReg := foo
  }
  out := myReg
}


@main def runTree(): Unit = println(ChiselStage.emitChirrtl(gen = new Tree))

