package org.linnando.mixemulator.vm

import scala.concurrent.Future
import scala.scalajs.js.annotation.JSExportTopLevel

trait VirtualMachine {
  def currentState: VirtualMachineState

  def breakpointAt(address: Short): Boolean

  def canMoveForward: Boolean

  def stepForward(): Future[Unit]

  def runForward(): Future[Unit]

  def toggleBreakpoint(address: Short): Unit

  def isModified(address: Short): Boolean
}

object VirtualMachine {
  @JSExportTopLevel("MEMORY_SIZE")
  val MEMORY_SIZE: Short = 4000

  val CHARACTERS: Array[Char] = Array(
    ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', '\u0394', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', '\u03a3', '\u03a0', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1',
    '2', '3', '4', '5', '6', '7', '8', '9',
    '.', ',', '(', ')', '+', '-', '*', '/',
    '=', '$', '<', '>', '@', ';', ':', '\''
  )

  val CODES: Map[Char, Byte] = CHARACTERS.zipWithIndex.map(c => (c._1, c._2.toByte)).toMap
}
