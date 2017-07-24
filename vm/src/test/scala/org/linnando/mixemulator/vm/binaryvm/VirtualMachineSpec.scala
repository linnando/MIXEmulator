package org.linnando.mixemulator.vm.binaryvm

import org.linnando.mixemulator.vm.exceptions.{BackFromInitialStateException, ForwardFromTerminalStateException}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.binary
import org.specs2.mutable.Specification

class VirtualMachineSpec extends Specification {
  "binary non-tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = binary.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpoints must be equalTo Set(3010, 3020)
      machine.toggleBreakpoint(3010.toShort)
      machine.breakpoints must be equalTo Set(3020)
    }

    "go one step forward" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.stepForward()
      machine.currentState.getA must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.getProgramCounter must be equalTo 3001
    }

    "stop when the current command is HLT" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.stepForward()
      machine.currentState.getProgramCounter must be equalTo 3000
      machine.currentState.isHalted must beTrue
    }

    "go to the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.getA must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.getProgramCounter must be equalTo 3005
    }

    "not execute the command at the breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005.toShort)
      machine.runForward()
      machine.currentState.getA must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
      machine.currentState.getProgramCounter must be equalTo 3005
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.getProgramCounter must be equalTo 3003
      machine.currentState.isHalted must beTrue
    }

    "report that it cannot move forward when halted" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.canMoveForward must beTrue
      machine.stepForward()
      machine.canMoveForward must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.stepForward()
      machine.stepForward() must throwA[ForwardFromTerminalStateException]
    }
  }

  "binary tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = binary.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpoints must be equalTo Set(3010, 3020)
      machine.toggleBreakpoint(3010)
      machine.breakpoints must be equalTo Set(3020)
    }

    "go one step forward" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward()
      machine.currentState.getA must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.getProgramCounter must be equalTo 3001
    }

    "stop when the current command is HLT" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward()
      machine.currentState.getProgramCounter must be equalTo 3000
      machine.currentState.isHalted must beTrue
    }

    "return back to the previous state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      machine.stepForward()
      machine.stepBack()
      machine.currentState must be equalTo initialState
    }

    "go to the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.getA must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.getProgramCounter must be equalTo 3005
    }

    "return back to the initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.runBack()
      machine.currentState must be equalTo initialState
    }

    "not execute the command at the breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.getA must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
      machine.currentState.getProgramCounter must be equalTo 3005
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.getProgramCounter must be equalTo 3003
      machine.currentState.isHalted must beTrue
    }

    "return back to a breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3005").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.runForward()
      machine.toggleBreakpoint(3003)
      machine.runBack()
      machine.currentState.getProgramCounter must be equalTo 3003
      machine.currentState.isHalted must beFalse
    }

    "report that it cannot move forward when halted" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.canMoveForward must beTrue
      machine.stepForward()
      machine.canMoveForward must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepForward()
      machine.stepForward() must throwA[ForwardFromTerminalStateException]
    }

    "report that it cannot move back from the initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.canMoveBack must beFalse
      machine.stepForward()
      machine.canMoveBack must beTrue
      machine.runBack()
      machine.canMoveBack must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepBack() must throwA[BackFromInitialStateException]
      machine.runBack() must throwA[BackFromInitialStateException]
      machine.stepForward()
      machine.runBack()
      machine.stepBack() must throwA[BackFromInitialStateException]
      machine.runBack() must throwA[BackFromInitialStateException]
    }
  }
}
