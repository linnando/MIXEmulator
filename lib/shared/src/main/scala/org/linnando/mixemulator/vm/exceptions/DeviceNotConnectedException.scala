package org.linnando.mixemulator.vm.exceptions

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DeviceNotConnectedException")
class DeviceNotConnectedException(@JSExport val deviceNum: Int) extends Exception {
  @JSExport
  def tag: String = getClass.getSimpleName
}
