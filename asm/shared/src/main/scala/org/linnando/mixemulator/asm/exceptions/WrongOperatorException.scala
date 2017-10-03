package org.linnando.mixemulator.asm.exceptions

class WrongOperatorException(val operator: String, val line: Int) extends Exception {
}
