package org.linnando.mixemulator.vm.decimalvm

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{BackFromInitialStateException, ForwardFromTerminalStateException}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class VirtualMachineSpec(implicit ee: ExecutionEnv) extends Specification {
  "decimal non-tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3010)
      machine.breakpointAt(3010) must beTrue
      machine.breakpointAt(3020) must beFalse
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpointAt(3010) must beFalse
      machine.breakpointAt(3020) must beTrue
    }

    "go one step forward" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      val state = machine.stepForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = true, Seq(1, 16, 3, 5, 4))).await
      state.map(_.getProgramCounter) must beEqualTo(3001).await
    }

    "stop when the current command is HLT" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      val state = machine.stepForward().map(_ => machine.currentState)
      state.map(_.getProgramCounter) must beEqualTo(3000).await
      state.map(_.isHalted) must beTrue.await
    }

    "go to the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = true, Seq(1, 16, 3, 5, 4))).await
      state.map(_.getProgramCounter) must beEqualTo(3005).await
    }

    "not execute the command at the breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005.toShort)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = false, Seq(0, 0, 0, 0, 0))).await
      state.map(_.getProgramCounter) must beEqualTo(3005).await
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.toggleBreakpoint(3005)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getProgramCounter) must beEqualTo(3003).await
      state.map(_.isHalted) must beTrue.await
    }

    "report that it cannot move forward when halted" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      machine.canMoveForward must beTrue
      machine.stepForward().map(_ => machine.canMoveForward) must beFalse.await
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .build
      val process = for {
        _ <- machine.stepForward()
        _ <- machine.stepForward()
      } yield ()
      process must throwA[ForwardFromTerminalStateException].await
    }
  }

  "decimal tracking virtual machine" should {
    "allow setting and removing breakpoints" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3010)
      machine.breakpointAt(3010) must beTrue
      machine.breakpointAt(3020) must beFalse
      machine.toggleBreakpoint(3010)
      machine.toggleBreakpoint(3020)
      machine.breakpointAt(3010) must beFalse
      machine.breakpointAt(3020) must beTrue
    }

    "go one step forward" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      val state = machine.stepForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = true, Seq(1, 16, 3, 5, 4))).await
      state.map(_.getProgramCounter) must beEqualTo(3001).await
    }

    "stop when the current command is HLT" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      val state = machine.stepForward().map(_ => machine.currentState)
      state.map(_.getProgramCounter) must beEqualTo(3000).await
      state.map(_.isHalted) must beTrue.await
    }

    "return back to the previous state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      val state = for {
        _ <- machine.stepForward()
        _ = machine.stepBack()
      } yield machine.currentState
      state must beEqualTo(initialState).await
    }

    "go to the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = true, Seq(1, 16, 3, 5, 4))).await
      state.map(_.getProgramCounter) must beEqualTo(3005).await
    }

    "return back to the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withFinalSection("", "3000")
        .buildTracking
      val initialState = machine.currentState
      machine.toggleBreakpoint(3005)
      val state = for {
        _ <- machine.runForward()
        _ = machine.runBack()
      } yield machine.currentState
      state must beEqualTo(initialState).await
    }

    "go to the halted state when replaying history" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      val state = for {
        _ <- machine.runForward()
        _ = machine.runBack()
        _ <- machine.runForward()
      } yield machine.currentState
      state.map(_.getProgramCounter) must beEqualTo(3000).await
      state.map(_.isHalted) must beTrue.await
    }

    "not execute the command at the breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3005").withCommand("2000", "", "", 8, 5)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getA) must beEqualTo(IOWord(negative = false, Seq(0, 0, 0, 0, 0))).await
      state.map(_.getProgramCounter) must beEqualTo(3005).await
    }

    "stop when HLT is encountered before the next breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3003").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.toggleBreakpoint(3005)
      val state = machine.runForward().map(_ => machine.currentState)
      state.map(_.getProgramCounter) must beEqualTo(3003).await
      state.map(_.isHalted) must beTrue.await
    }

    "return back to a breakpoint" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3005").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      val state = for {
        _ <- machine.runForward()
        _ = machine.toggleBreakpoint(3003)
        _ = machine.runBack()
      } yield machine.currentState
      state.map(_.getProgramCounter) must beEqualTo(3003).await
      state.map(_.isHalted) must beFalse.await
    }

    "report that it cannot move forward when halted" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.canMoveForward must beTrue
      machine.stepForward().map(_ => machine.canMoveForward) must beFalse.await
    }

    "throw an exception on attempt to move forward from a halted state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      val process = for {
        _ <- machine.stepForward()
        _ <- machine.stepForward()
      } yield ()
      process must throwA[ForwardFromTerminalStateException].await
    }

    "report that it cannot move back from the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.canMoveBack must beFalse
      machine.stepForward().map(_ => {
        val save = machine.canMoveBack
        machine.runBack()
        (save, machine.canMoveBack)
      }) must beEqualTo((true, false)).await
    }

    "throw an exception on an attempt to move back from the initial state" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("3000").withCommand("", "", "", 5, 2)
        .withFinalSection("", "3000")
        .buildTracking
      machine.stepBack() must throwA[BackFromInitialStateException]
      machine.runBack() must throwA[BackFromInitialStateException]
    }
  }
}
