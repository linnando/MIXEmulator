package org.linnando.mixemulator.vm

trait VirtualMachine {
  def currentState: VirtualMachineState

  def breakpoints: Set[Short]

  def canMoveForward: Boolean

  def stepForward(): Unit

  def runForward(): Unit

  def toggleBreakpoint(address: Short): Unit
}

object VirtualMachine {
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
