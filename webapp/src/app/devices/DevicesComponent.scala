package org.linnando.mixemulator.webapp

import angulate2.std._

import scala.scalajs.js

@Component(
  selector = "devices",
  templateUrl = "webapp/src/main/resources/devices.component.html",
  styleUrls = @@@("webapp/src/main/resources/devices.component.css")
)
class DevicesComponent(virtualMachineService: VirtualMachineService) extends OnInit {
  var deviceNumbers: js.Array[Int] = js.Array()
  var selectedDevice: Int = _

  override def ngOnInit(): Unit = {
    deviceNumbers = js.Array[Int]() ++ virtualMachineService.deviceNumbers
  }

  def deviceDescription(deviceNumber: Int): String = {
    val deviceName = virtualMachineService.deviceName(deviceNumber)
    s"$deviceNumber - $deviceName"
  }
}
