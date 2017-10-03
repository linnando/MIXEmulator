package org.linnando.mixemulator.asm.exceptions

class WrongAddressPartException(val addressPart: String, val line: Int) extends Exception {
}
