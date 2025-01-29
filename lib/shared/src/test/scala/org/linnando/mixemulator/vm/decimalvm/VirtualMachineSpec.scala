package org.linnando.mixemulator.vm.decimalvm

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{BackFromInitialStateException, ForwardFromTerminalStateException}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class VirtualMachineSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  "decimal non-tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3010)
      machine.breakpointAt(3010) mustEqual true
      machine.breakpointAt(3020) mustEqual false
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpointAt(3010) mustEqual false
      machine.breakpointAt(3020) mustEqual true
    }

    "go one step forward" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.stepForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
        machine.currentState.getProgramCounter mustEqual 3001
      })
    }

    "stop when the current command is HLT" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.stepForward().map(_ => {
        machine.currentState.getProgramCounter mustEqual 3000
        machine.currentState.isHalted mustEqual true
      })
    }

    "go to the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
        machine.currentState.getProgramCounter mustEqual 3005
      })
    }

    "not execute the command at the breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005.toShort)
      machine.runForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        machine.currentState.getProgramCounter mustEqual 3005
      })
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward().map(_ => {
        machine.currentState.getProgramCounter mustEqual 3003
        machine.currentState.isHalted mustEqual true
      })
    }

    "report that it cannot move forward when halted" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.stepForward().map(_ => {
        machine.canMoveForward mustEqual false
      })
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      recoverToSucceededIf[ForwardFromTerminalStateException] {
        for {
          _ <- machine.stepForward()
          _ <- machine.stepForward()
        } yield ()
      }
    }
  }

  "decimal tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3010)
      machine.breakpointAt(3010) mustEqual true
      machine.breakpointAt(3020) mustEqual false
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpointAt(3010) mustEqual false
      machine.breakpointAt(3020) mustEqual true
    }

    "go one step forward" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
        machine.currentState.getProgramCounter mustEqual 3001
      })
    }

    "stop when the current command is HLT" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward().map(_ => {
        machine.currentState.getProgramCounter mustEqual 3000
        machine.currentState.isHalted mustEqual true
      })
    }

    "return back to the previous state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      for {
        _ <- machine.stepForward()
        _ = machine.stepBack()
      } yield machine.currentState mustEqual initialState
    }

    "go to the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
        machine.currentState.getProgramCounter mustEqual 3005
      })
    }

    "return back to the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      machine.toggleBreakpoint(3005)
      for {
        _ <- machine.runForward()
        _ = machine.runBack()
      } yield machine.currentState mustEqual initialState
    }

    "go to the halted state when replaying history" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      for {
        _ <- machine.runForward()
        _ = machine.runBack()
        _ <- machine.runForward()
      } yield {
        machine.currentState.getProgramCounter mustEqual 3000
        machine.currentState.isHalted mustEqual true
      }
    }

    "not execute the command at the breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward().map(_ => {
        machine.currentState.getA mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        machine.currentState.getProgramCounter mustEqual 3005
      })
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward().map(_ => {
        machine.currentState.getProgramCounter mustEqual 3003
        machine.currentState.isHalted mustEqual true
      })
    }

    "return back to a breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3005").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      for {
        _ <- machine.runForward()
        _ = machine.toggleBreakpoint(3003)
        _ = machine.runBack()
      } yield {
        machine.currentState.getProgramCounter mustEqual 3003
        machine.currentState.isHalted mustEqual false
      }
    }

    "report that it cannot move forward when halted" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward().map(_ => {
        machine.canMoveForward mustEqual false
      })
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      recoverToSucceededIf[ForwardFromTerminalStateException] {
        for {
          _ <- machine.stepForward()
          _ <- machine.stepForward()
        } yield ()
      }
    }

    "report that it cannot move back from the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward().map(_ => {
        machine.runBack()
        machine.canMoveBack mustEqual false
      })
    }

    "throw an exception on an attempt to step back from the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      a[BackFromInitialStateException] must be thrownBy {
        machine.stepBack()
      }
    }

    "throw an exception on an attempt to run back from the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      a[BackFromInitialStateException] must be thrownBy {
        machine.runBack()
      }
    }
  }
}
