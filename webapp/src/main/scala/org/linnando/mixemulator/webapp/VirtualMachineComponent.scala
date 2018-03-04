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
  var mode = "binary"
  var tracking = false

  def assemble(): Unit = {
    val assembling = (mode, tracking) match {
      case ("binary", false) => virtualMachineService.assembleBinaryNonTracking()
      case ("binary", true) => virtualMachineService.assembleBinaryTracking()
      case ("decimal", false) => virtualMachineService.assembleDecimalNonTracking()
      case ("decimal", true) => virtualMachineService.assembleDecimalTracking()
      case (_, _) => throw new Error
    }
    assembling onComplete {
      case Success(_) => ()
      case Failure(e) => ErrorPopup.show(e)
    }
  }

  def go(): Unit = {
    val init = (mode, tracking) match {
      case ("binary", false) => virtualMachineService.goBinaryNonTracking()
      case ("binary", true) => virtualMachineService.goBinaryTracking()
      case ("decimal", false) => virtualMachineService.goDecimalNonTracking()
      case ("decimal", true) => virtualMachineService.goDecimalTracking()
      case (_, _) => throw new Error
    }
    init onComplete {
      case Success(_) => ()
      case Failure(e) => ErrorPopup.show(e)
    }
  }

  def switchOff(): Unit = virtualMachineService.switchOffMachine()

  def canMoveForward: Boolean = virtualMachineService.canMoveForward

  def runForward(): Unit =
    virtualMachineService.runForward() onComplete {
      case Success(_) => ()
      case Failure(e) => ErrorPopup.show(e)
    }

  def stepForward(): Unit =
    virtualMachineService.stepForward() onComplete {
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
