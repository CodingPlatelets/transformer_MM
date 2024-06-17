package pe

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import dataclass.data

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

class ForwardingMemory[T <: Data](val size: Int, element: T) extends Module {
  val io = IO(new Bundle {
    val rdAddr = Input(UInt(16.W))
    val rdData = Output(element)
    val wrEna = Input(Bool())
    val wrData = Input(element)
    val wrAddr = Input(UInt(16.W))
  })
  val mem = SyncReadMem(size, element)
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

class ForwardingDelayMemory[T <: Data](element: T, val size: Int, val delay: Int) extends Module {
  val io = IO(new Bundle {
    val wrData = Flipped(Decoupled(element))
    val rdData = Decoupled(element)
  })
  val mem = Module(new ForwardingMemory(size, element))
  mem.io := DontCare
  io.rdData := DontCare

  object State extends ChiselEnum {
    val sIdle, sWrite, sRead = Value
  }

  // val state = RegInit(State.sIdle)
  val wCnt = Counter(delay + 1)
  val rCnt = Counter(delay + 1)

  val state = RegInit(State.sIdle)

  val dataReady = RegInit(true.B)
  io.wrData.ready := dataReady
  val tempData = Reg(element)
  val resValid = RegInit(false.B)
  io.rdData.valid := resValid

  mem.io.wrEna := state =/= State.sRead

  val readFinished = RegInit(false.B)

  // printf("state: %d\n", state.asUInt)
  // printf("wCnt: %d, rCnt: %d\n", wCnt.value, rCnt.value)
  // printf("readFinished: %d, resValid: %d\n", readFinished.asUInt, resValid)
  // printf("\n")

  switch(state) {
    is(State.sIdle) {
      when(io.wrData.valid && wCnt.value < delay.U) {
        dataReady := false.B
        tempData := io.wrData.bits
        state := State.sWrite
      }

      when(wCnt.value === delay.U) {
        state := State.sRead
      }
    }
    is(State.sWrite) {
      mem.io.wrData := tempData
      mem.io.wrAddr := wCnt.value
      wCnt.inc()
      dataReady := true.B
      state := State.sIdle
    }
    is(State.sRead) {
      when(rCnt.value === delay.U) {
        state := State.sIdle
      }.elsewhen(!readFinished && rCnt.value < delay.U) {
        mem.io.rdAddr := rCnt.value
        readFinished := true.B
      }.elsewhen(readFinished && rCnt.value < delay.U) {
        resValid := true.B
        io.rdData.bits := mem.io.rdData
        when(resValid && io.rdData.ready) {
          resValid := false.B
          readFinished := false.B
          rCnt.inc()
        }
      }
    }
  }
}
