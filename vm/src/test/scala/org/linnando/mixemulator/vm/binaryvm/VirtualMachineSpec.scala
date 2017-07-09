package org.linnando.mixemulator.vm.binaryvm

import org.linnando.mixemulator.vm.exceptions.{BackFromInitialStateException, ForwardFromTerminalStateException}
import org.linnando.mixemulator.vm.{UniWord, binary}
import org.specs2.mutable.Specification

class VirtualMachineSpec extends Specification {
  "binary non-tracking virtual machine" should {
    "load initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .build
      machine.currentState.memory.get(binary.MixIndex(2000)) must
        be equalTo binary.MixWord(0x41403144)
      machine.currentState.memory.get(binary.MixIndex(3000)) must
        be equalTo binary.MixWord(0x1f400148)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3000)
    }

    "allow setting and removing breakpoints" in {
      val machine = binary.createVirtualMachineBuilder()
        .updateProgramCounter(3000)
        .build
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpoints must be equalTo Set(3010, 3020)
      machine.toggleBreakpoint(3010)
      machine.breakpoints must be equalTo Set(3020)
    }

    "go one step forward" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .build
      machine.stepForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x41403144)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3001)
    }

    "stop when the current command is HLT" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .build
      machine.stepForward()
      machine.currentState.programCounter must be equalTo binary.MixIndex(3000)
      machine.currentState.isHalted must beTrue
    }

    "go to the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x41403144)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3005)
    }

    "not execute the command at the breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3005, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x00000000)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3005)
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3003, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .build
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.programCounter must be equalTo binary.MixIndex(3003)
      machine.currentState.isHalted must beTrue
    }

    "report that it cannot move forward when halted" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .build
      machine.canMoveForward must beTrue
      machine.stepForward()
      machine.canMoveForward must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .build
      machine.stepForward()
      machine.stepForward() must throwA[ForwardFromTerminalStateException]
    }
  }

  "binary tracking virtual machine" should {
    "load initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.currentState.memory.get(binary.MixIndex(2000)) must
        be equalTo binary.MixWord(0x41403144)
      machine.currentState.memory.get(binary.MixIndex(3000)) must
        be equalTo binary.MixWord(0x1f400148)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3000)
    }

    "allow setting and removing breakpoints" in {
      val machine = binary.createVirtualMachineBuilder()
        .updateProgramCounter(3000)
        .buildTracking
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpoints must be equalTo Set(3010, 3020)
      machine.toggleBreakpoint(3010)
      machine.breakpoints must be equalTo Set(3020)
    }

    "go one step forward" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.stepForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x41403144)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3001)
    }

    "stop when the current command is HLT" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.stepForward()
      machine.currentState.programCounter must be equalTo binary.MixIndex(3000)
      machine.currentState.isHalted must beTrue
    }

    "return back to the previous state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .buildTracking
      val initialState = machine.currentState
      machine.stepForward()
      machine.stepBack()
      machine.currentState must be equalTo initialState
    }

    "go to the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x41403144)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3005)
    }

    "return back to the initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .updateProgramCounter(3000)
        .buildTracking
      val initialState = machine.currentState
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.runBack()
      machine.currentState must be equalTo initialState
    }

    "not execute the command at the breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(2000, UniWord(negative = true,
          Map(9.toByte -> 1L, 18.toByte -> 16L, 27.toByte -> 3L, 36.toByte -> 5L, 45.toByte -> 4L)))
        .update(3005, UniWord(negative = false,
          Map(10.toByte -> 2000L, 27.toByte -> 0L, 36.toByte -> 5L, 45.toByte -> 8L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.registers.getA must be equalTo binary.MixWord(0x00000000)
      machine.currentState.programCounter must be equalTo binary.MixIndex(3005)
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3003, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.toggleBreakpoint(3005)
      machine.runForward()
      machine.currentState.programCounter must be equalTo binary.MixIndex(3003)
      machine.currentState.isHalted must beTrue
    }

    "return back to a breakpoint" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3005, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.runForward()
      machine.toggleBreakpoint(3003)
      machine.runBack()
      machine.currentState.programCounter must be equalTo binary.MixIndex(3003)
      machine.currentState.isHalted must beFalse
    }

    "report that it cannot move forward when halted" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.canMoveForward must beTrue
      machine.stepForward()
      machine.canMoveForward must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.stepForward()
      machine.stepForward() must throwA[ForwardFromTerminalStateException]
    }

    "report that it cannot move back from the initial state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
        .buildTracking
      machine.canMoveBack must beFalse
      machine.stepForward()
      machine.canMoveBack must beTrue
      machine.runBack()
      machine.canMoveBack must beFalse
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = binary.createVirtualMachineBuilder()
        .update(3000, UniWord(negative = false,
          Map(10.toByte -> 0L, 27.toByte -> 0L, 36.toByte -> 2L, 45.toByte -> 5L)))
        .updateProgramCounter(3000)
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
