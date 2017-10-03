package org.linnando.mixemulator.asm.exceptions

class WrongComputedMemoryAddressException(val address: Long, val line: Int) extends Exception {
}
