package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryProcessingModel._
import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.exceptions.ForwardFromTerminalStateException
import org.specs2.mutable.Specification

class BinaryControlFlowSpec extends Specification {
  private val state = initialState.copy(
    programCounter = BinaryMixIndex(3000)
  )

  "control flow" should {
    "do nothing on NOP" in {
      // A = 0, I = 0, F = 0, C = 0 NOP
      val nextState = execute(state, BinaryMixWord(0x00000000))
      nextState.registers must be equalTo state.registers
      nextState.memory must be equalTo state.memory
    }

    "stop execution on HLT" in {
      // A = 0, I = 0, F = 2, C = 5 HLT
      val nextState = execute(state, BinaryMixWord(0x00000085))
      nextState.programCounter must be equalTo state.programCounter
      nextState.isHalted must beTrue
    }

    "perform unconditional jump" in {
      // A = 1000, I = 0, F = 0, C = 39 JMP
      val nextState = execute(state, BinaryMixWord(0x0fa00027))
      nextState.programCounter must be equalTo BinaryMixIndex(1000)
      nextState.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "perform unconditional jump saving J" in {
      // A = 1000, I = 0, F = 1, C = 39 JSJ
      val nextState = execute(state, BinaryMixWord(0x0fa00067))
      nextState.programCounter must be equalTo BinaryMixIndex(1000)
      nextState.registers.getJ must be equalTo BinaryMixIndex(0)
    }

    "advance according to the command in memory" in {
      val prevState = state.copy(
        memory = state.memory
          .updated(BinaryMixIndex(2000), BinaryMixWord(0x41403144)) // - 1 16 3 5 4
          .updated(BinaryMixIndex(3000), BinaryMixWord(0x1f400148)) // A = 2000, I = 0, F = 0:5, C = 8 LDA
      )
      val nextState = forward(prevState)
      nextState.registers.getA must be equalTo BinaryMixWord(0x41403144)
    }

    "throw an exception on attempt to advance a halted machine" in {
      val prevState = state.copy(isHalted = true)
      forward(prevState) must throwA[ForwardFromTerminalStateException]
    }
  }

  "overflow trigger" should {
    "trigger jump on overflow and not trigger jump on no overflow when set" in {
      val prevState = state.copy(registers = state.registers.updatedOV(true))
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(prevState, BinaryMixWord(0x0fa000a7))
      nextStateOV.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateOV.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateOV.registers.getOV must beFalse
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(prevState, BinaryMixWord(0x0fa000e7))
      nextStateNOV.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNOV.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateNOV.registers.getOV must beTrue
    }

    "not trigger jump on overflow and trigger jump on no overflow when not set" in {
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(state, BinaryMixWord(0x0fa000a7))
      nextStateOV.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateOV.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateOV.registers.getOV must beFalse
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(state, BinaryMixWord(0x0fa000e7))
      nextStateNOV.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNOV.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateOV.registers.getOV must beFalse
    }
  }

  "comparison flag" should {
    "trigger jumps accordingly when equals to LESS" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.LESS))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00127))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getCMP must be equalTo Comparison.LESS
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00167))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateE.registers.getCMP must be equalTo Comparison.LESS
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa001a7))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateG.registers.getCMP must be equalTo Comparison.LESS
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa001e7))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateGE.registers.getCMP must be equalTo Comparison.LESS
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00227))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getCMP must be equalTo Comparison.LESS
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00267))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getCMP must be equalTo Comparison.LESS
    }

    "trigger jumps accordingly when equals to EQUAL" in {
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(state, BinaryMixWord(0x0fa00127))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateL.registers.getCMP must be equalTo Comparison.EQUAL
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(state, BinaryMixWord(0x0fa00167))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getCMP must be equalTo Comparison.EQUAL
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(state, BinaryMixWord(0x0fa001a7))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateG.registers.getCMP must be equalTo Comparison.EQUAL
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(state, BinaryMixWord(0x0fa001e7))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getCMP must be equalTo Comparison.EQUAL
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(state, BinaryMixWord(0x0fa00227))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateNE.registers.getCMP must be equalTo Comparison.EQUAL
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(state, BinaryMixWord(0x0fa00267))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getCMP must be equalTo Comparison.EQUAL
    }

    "trigger jumps accordingly when equals to GREATER" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.GREATER))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00127))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateL.registers.getCMP must be equalTo Comparison.GREATER
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00167))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateE.registers.getCMP must be equalTo Comparison.GREATER
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa001a7))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getCMP must be equalTo Comparison.GREATER
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa001e7))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getCMP must be equalTo Comparison.GREATER
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00227))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getCMP must be equalTo Comparison.GREATER
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00267))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
      nextStateLE.registers.getCMP must be equalTo Comparison.GREATER
    }
  }

  "register A" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(BinaryMixWord(0x40000001)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00028))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00068))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a8))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e8))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00128))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00168))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(BinaryMixWord(0x40000000)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00028))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00068))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a8))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e8))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00128))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00168))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(state, BinaryMixWord(0x0fa00028))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(state, BinaryMixWord(0x0fa00068))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(state, BinaryMixWord(0x0fa000a8))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000e8))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa00128))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa00168))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(BinaryMixWord(0x00000001)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00028))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00068))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a8))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e8))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00128))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00168))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I1" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00029))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00069))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a9))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e9))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00129))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00169))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00029))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00069))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a9))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e9))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00129))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00169))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(state, BinaryMixWord(0x0fa00029))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa00069))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000a9))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000e9))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa00129))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa00169))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa00029))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa00069))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000a9))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000e9))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa00129))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa00169))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I2" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002a))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006a))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000aa))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ea))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012a))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016a))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002a))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006a))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000aa))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ea))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012a))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016a))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002a))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006a))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000aa))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000ea))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012a))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016a))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002a))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006a))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000aa))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ea))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012a))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016a))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I3" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002b))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006b))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ab))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000eb))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012b))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016b))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002b))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006b))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ab))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000eb))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012b))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016b))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002b))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006b))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000ab))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000eb))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012b))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016b))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002b))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006b))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ab))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000eb))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012b))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016b))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I4" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002c))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006c))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ac))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ec))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012c))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016c))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002c))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006c))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ac))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ec))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012c))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016c))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002c))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006c))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000ac))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000ec))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012c))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016c))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002c))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006c))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ac))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ec))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012c))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016c))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I5" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002d))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006d))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ad))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ed))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012d))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016d))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002d))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006d))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ad))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ed))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012d))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016d))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002d))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006d))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000ad))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000ed))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012d))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016d))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002d))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006d))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ad))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ed))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012d))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016d))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register I6" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, BinaryMixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002e))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006e))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ae))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ee))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012e))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016e))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, BinaryMixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002e))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006e))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ae))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ee))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012e))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016e))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002e))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006e))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(state, BinaryMixWord(0x0fa000ae))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000ee))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012e))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016e))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, BinaryMixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002e))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006e))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000ae))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ee))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012e))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016e))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }

  "register X" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(BinaryMixWord(0x40000001)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002f))
      nextStateL.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006f))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000af))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ef))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012f))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016f))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(BinaryMixWord(0x40000000)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002f))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006f))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000af))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ef))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012f))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016f))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(state, BinaryMixWord(0x0fa0002f))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(state, BinaryMixWord(0x0fa0006f))
      nextStateE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(state, BinaryMixWord(0x0fa000af))
      nextStateG.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(state, BinaryMixWord(0x0fa000ef))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(state, BinaryMixWord(0x0fa0012f))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(state, BinaryMixWord(0x0fa0016f))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(3001)
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(BinaryMixWord(0x00000001)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, BinaryMixWord(0x0fa0002f))
      nextStateL.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateL.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, BinaryMixWord(0x0fa0006f))
      nextStateE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateE.registers.getJ must be equalTo BinaryMixIndex(0)
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, BinaryMixWord(0x0fa000af))
      nextStateG.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateG.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, BinaryMixWord(0x0fa000ef))
      nextStateGE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateGE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, BinaryMixWord(0x0fa0012f))
      nextStateNE.programCounter must be equalTo BinaryMixIndex(1000)
      nextStateNE.registers.getJ must be equalTo BinaryMixIndex(3001)
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, BinaryMixWord(0x0fa0016f))
      nextStateLE.programCounter must be equalTo BinaryMixIndex(3001)
      nextStateLE.registers.getJ must be equalTo BinaryMixIndex(0)
    }
  }
}
