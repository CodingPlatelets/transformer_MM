package pe

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

class RWmem extends Module {

  val io = IO(new Bundle {
    val read = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(32.W))
    val dataIn = Input(UInt(32.W))
    val dataOut = Output(UInt(32.W))
  })

}

class RWmemFile(val bits: Int, val size: Int, memFile: String = "") extends Module {

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(10.W))
    val dataIn = Input(Vec(size, UInt(bits.W)))
    val dataOut = Output(Vec(size, UInt(bits.W)))
    // val dataIn = Input(UInt(bits.W))
    // val dataOut = Output(UInt(bits.W))
  })
  val cnt = RegInit(0.U(10.W))
  val mem = SyncReadMem(size * bits, Vec(size, UInt(bits.W)))
  // val mem = SyncReadMem(bits, UInt(bits.W))
  // Initialize memory
  if (memFile.trim().nonEmpty) {
    loadMemoryFromFile(mem, memFile)
  }
  io.dataOut := DontCare
  when(io.enable) {
    val rdwrPort = mem(io.addr)
    when(io.write) { rdwrPort := io.dataIn }.otherwise { io.dataOut := rdwrPort }
  }
}

class ForwardingMemory(val bits: Int, val size: Int) extends Module {
  val io = IO(new Bundle {
    val rdAddr = Input(UInt(12.W))
    val rdData = Output(Vec(size, UInt(bits.W)))
    val wrEna = Input(Bool())
    val wrData = Input(Vec(size, UInt(bits.W)))
    val wrAddr = Input(UInt(12.W))
  })
  val mem = SyncReadMem(size * bits * 2, Vec(size, UInt(bits.W)))
  val wrDataReg = RegNext(io.wrData)
  val doForwardReg = RegNext(
    io.wrAddr === io.rdAddr &&
      io.wrEna
  )
  val memData = mem.read(io.rdAddr)
  when(io.wrEna) {
    mem.write(io.wrAddr, io.wrData)
  }
  io.rdData := Mux(doForwardReg, wrDataReg, memData)
}
