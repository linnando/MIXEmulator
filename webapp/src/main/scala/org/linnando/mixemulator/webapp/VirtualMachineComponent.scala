package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.vm.{TrackingVirtualMachine, VirtualMachine, VirtualMachineState}

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
      println(e)
      ???
  }

  def assembleTracking(): Unit = virtualMachineService.assembleBinaryTracking() onComplete {
    case Success(_) =>
    case Failure(e) =>
      println(e)
      ???
  }

  def canMoveForward: Boolean = virtualMachineService.canMoveForward

  def runForward(): Unit = virtualMachineService.runForward()

  def stepForward(): Unit = virtualMachineService.stepForward()

  def canMoveBack: Boolean = virtualMachineService.canMoveBack

  def runBack(): Unit = virtualMachineService.runBack()

  def stepBack(): Unit = virtualMachineService.stepBack()

  def isActive: Boolean = virtualMachineService.isActive
}
