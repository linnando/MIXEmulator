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
  def assembleNonTracking(): Unit = virtualMachineService.assembleBinaryNonTracking() onComplete {
    case Success(_) =>
    case Failure(e) =>
      ErrorPopup.show(e)
  }

  def assembleTracking(): Unit = virtualMachineService.assembleBinaryTracking() onComplete {
    case Success(_) =>
    case Failure(e) =>
      ErrorPopup.show(e)
  }

  def canMoveForward: Boolean = virtualMachineService.canMoveForward

  def runForward(): Unit =
    try {
      virtualMachineService.runForward()
    }
    catch {
      case e: Throwable => ErrorPopup.show(e)
    }

  def stepForward(): Unit =
    try {
      virtualMachineService.stepForward()
    }
    catch {
      case e: Throwable => ErrorPopup.show(e)
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
