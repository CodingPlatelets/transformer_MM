package kernel.utils

import chisel3._
import chisel3.Printable

trait DebugLog {
  object LogLevel {
    val DEBUG = 3
    val INFO = 2
    val ERROR = 1
    val QUIET = 0
  }
  var logLevel = LogLevel.ERROR
  def debugLog(p: Printable, level: Int = logLevel) = {
    if (level >= LogLevel.DEBUG) {
      printf(p)
    }
  }
  def infoLog(p: Printable, level: Int = logLevel) = {
    if (level >= LogLevel.INFO) {
      printf(p)
    }
  }
  def errorLog(p: Printable, level: Int = logLevel) = {
    if (level >= LogLevel.ERROR) {
      printf(p)
    }
  }
}
