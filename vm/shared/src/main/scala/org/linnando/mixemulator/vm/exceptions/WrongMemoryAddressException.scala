package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("WrongMemoryAddressException")
class WrongMemoryAddressException(val address: Long) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName

  @JSExport("address")
  def addressJs: Double = address.toDouble
}
