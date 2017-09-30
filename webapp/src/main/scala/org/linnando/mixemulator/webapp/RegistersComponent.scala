package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.io.data.IOWord

@Component(
  selector = "registers",
  templateUrl = "webapp/src/main/resources/registers.component.html",
  styleUrls = @@@("webapp/src/main/resources/registers.component.css")
)
class RegistersComponent(virtualMachineService: VirtualMachineService) {
  private def machineState = virtualMachineService.machineState

  def registerSign(register: String): String =
    registerContent(register).map(w => if (w.negative) "-" else "+").getOrElse("+")

  def registerByte(register: String, pos: Int): Byte =
    registerContent(register).map(_.bytes(pos)).getOrElse(0)

  def registerContent(register: String): Option[IOWord] = register match {
    case "A" => machineState.map(_.getA)
    case "X" => machineState.map(_.getX)
    case "I1" => machineState.map(_.getI1)
    case "I2" => machineState.map(_.getI2)
    case "I3" => machineState.map(_.getI3)
    case "I4" => machineState.map(_.getI4)
    case "I5" => machineState.map(_.getI5)
    case "I6" => machineState.map(_.getI6)
    case "J" => machineState.map(_.getJ)
  }

  def overflow: Boolean = machineState.exists(_.getOV)

  def comparison: Int = machineState.map(_.getCMP match {
    case Comparison.LESS => -1
    case Comparison.EQUAL => 0
    case Comparison.GREATER => 1
  }).getOrElse(0)

  def timeElapsed: Int = machineState.map(_.getTimeCounter).getOrElse(0)

  def isHalted: Boolean = machineState.exists(_.isHalted)
}
