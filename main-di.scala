import chisel3._
import chisel3.experimental.hierarchy.{Definition, Instance, IsInstantiable}
import chisel3.experimental.BaseModule
import chisel3.stage.ChiselStage

class AddOne(width: Int) extends Module with IsInstantiable {
  val in  = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))
  out := in + 1.U
}

class AddTwo(width: Int) extends Module {
  val in  = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))
  val addOneDef = Definition(new AddOne(width))
  val i0 = Instance(addOneDef)
  val i1 = Instance(addOneDef)
  // i0.in := in
  // i1.in := in
  i0.selectDynamic("in").asInstanceOf[UInt] := in
  i1.selectDynamic("in").asInstanceOf[UInt] := i0.selectDynamic("out").asInstanceOf[UInt]
  out   := i1.selectDynamic("out").asInstanceOf[UInt]
}


@main def runDI(): Unit = println(ChiselStage.emitChirrtl(gen = new AddTwo(4)))
