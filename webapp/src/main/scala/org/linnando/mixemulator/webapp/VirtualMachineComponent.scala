package org.linnando.mixemulator.webapp

import angulate2.std._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

@Component(
  selector = "virtual-machine",
  templateUrl = "webapp/src/main/resources/virtual-machine.component.html",
  styleUrls = @@@("webapp/src/main/resources/virtual-machine.component.css")
)
class VirtualMachineComponent(virtualMachineService: VirtualMachineService) {
  def mode: String = virtualMachineService.mode

  def mode_=(value: String): Unit = virtualMachineService.mode = value

  def tracking: Boolean = virtualMachineService.tracking

  def tracking_=(value: Boolean): Unit = virtualMachineService.tracking = value

  def assemble(): Unit = virtualMachineService.assemble() onComplete {
    case Success(_) => ()
    case Failure(e) => ErrorPopup.show(e)
  }

  def switchOff(): Unit = virtualMachineService.switchOffMachine()

  def canMoveForward: Boolean = virtualMachineService.canMoveForward

  def runForward(): Unit = virtualMachineService.runForward() onComplete {
    case Success(_) => ()
    case Failure(e) => ErrorPopup.show(e)
  }

  def stepForward(): Unit = virtualMachineService.stepForward() onComplete {
    case Success(_) => ()
    case Failure(e) => ErrorPopup.show(e)
  }

  def canMoveBack: Boolean = virtualMachineService.canMoveBack

  def runBack(): Unit =
    try {
      virtualMachineService.runBack()
    }
    catch {
      case e: Throwable => ErrorPopup.show(e)
    }

  def stepBack(): Unit =
    try {
      virtualMachineService.stepBack()
    }
    catch {
      case e: Throwable => ErrorPopup.show(e)
    }

  def isActive: Boolean = virtualMachineService.isActive
}
