package pe.utils

import chisel3._
import chisel3.util._

class QueueModule[T <: Data](ioType: T, entries: Int, useMem: Boolean, pipe: Boolean, flow: Boolean) extends Module {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries, pipe = pipe, flow = flow, useSyncReadMem = useMem)
}
