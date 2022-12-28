package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.{Comparison, binary}
import org.linnando.mixemulator.vm.exceptions.ForwardFromTerminalStateException
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ControlFlowSpec(implicit ee: ExecutionEnv) extends Specification {

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    programCounter = MixIndex(3000)
  )

  "control flow in the binary mode" should {
    "do nothing on NOP" in {
      // A = 0, I = 0, F = 0, C = 0 NOP
      val nextState = execute(state, MixWord(0x00000000))
      nextState.map(_.registers) must beEqualTo(state.registers).await
      nextState.map(_.memory) must beEqualTo(state.memory).await
    }

    "stop execution on HLT" in {
      // A = 0, I = 0, F = 2, C = 5 HLT
      val nextState = execute(state, MixWord(0x00000085))
      nextState.map(_.programCounter) must beEqualTo(state.programCounter).await
      nextState.map(_.isHalted) must beTrue.await
    }

    "perform unconditional jump" in {
      // A = 1000, I = 0, F = 0, C = 39 JMP
      val nextState = execute(state, MixWord(0x0fa00027))
      nextState.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextState.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "perform unconditional jump saving J" in {
      // A = 1000, I = 0, F = 1, C = 39 JSJ
      val nextState = execute(state, MixWord(0x0fa00067))
      nextState.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextState.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }

    "advance according to the command in memory" in {
      val prevState = state.copy(
        memory = state.memory
          .updated(MixIndex(2000), MixWord(0x41403144)) // - 1 16 3 5 4
          .updated(MixIndex(3000), MixWord(0x1f400148)) // A = 2000, I = 0, F = 0:5, C = 8 LDA
      )
      val nextState = forward(prevState)
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x41403144)).await
    }

    "throw an exception on attempt to advance a halted machine" in {
      val prevState = state.copy(isHalted = true)
      forward(prevState) must throwA[ForwardFromTerminalStateException].await
    }
  }

  "overflow trigger in the binary mode" should {
    "trigger jump on overflow and not trigger jump on no overflow when set" in {
      val prevState = state.copy(registers = state.registers.updatedOV(true))
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(prevState, MixWord(0x0fa000a7))
      nextStateOV.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateOV.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(prevState, MixWord(0x0fa000e7))
      nextStateNOV.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNOV.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateNOV.map(_.registers.getOV) must beTrue.await
    }

    "not trigger jump on overflow and trigger jump on no overflow when not set" in {
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(state, MixWord(0x0fa000a7))
      nextStateOV.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(state, MixWord(0x0fa000e7))
      nextStateNOV.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNOV.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
    }
  }

  "comparison flag in the binary mode" should {
    "trigger jumps accordingly when equals to LESS" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.LESS))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, MixWord(0x0fa00127))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, MixWord(0x0fa00167))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, MixWord(0x0fa001a7))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, MixWord(0x0fa001e7))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, MixWord(0x0fa00227))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, MixWord(0x0fa00267))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
    }

    "trigger jumps accordingly when equals to EQUAL" in {
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(state, MixWord(0x0fa00127))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(state, MixWord(0x0fa00167))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(state, MixWord(0x0fa001a7))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(state, MixWord(0x0fa001e7))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(state, MixWord(0x0fa00227))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(state, MixWord(0x0fa00267))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "trigger jumps accordingly when equals to GREATER" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.GREATER))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, MixWord(0x0fa00127))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, MixWord(0x0fa00167))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, MixWord(0x0fa001a7))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, MixWord(0x0fa001e7))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, MixWord(0x0fa00227))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, MixWord(0x0fa00267))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }
  }

  "register A in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x40000001)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(0x0fa00028))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(0x0fa00068))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(0x0fa000a8))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e8))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00128))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(0x0fa00168))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x40000000)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(0x0fa00028))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(0x0fa00068))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(0x0fa000a8))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e8))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00128))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(0x0fa00168))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(state, MixWord(0x0fa00028))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(state, MixWord(0x0fa00068))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(state, MixWord(0x0fa000a8))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(state, MixWord(0x0fa000e8))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(state, MixWord(0x0fa00128))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(state, MixWord(0x0fa00168))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x00000001)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(0x0fa00028))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(0x0fa00068))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(0x0fa000a8))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e8))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00128))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(0x0fa00168))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I1 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(0x0fa00029))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(0x0fa00069))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(0x0fa000a9))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e9))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00129))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(0x0fa00169))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(0x0fa00029))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(0x0fa00069))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(0x0fa000a9))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e9))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00129))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(0x0fa00169))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(state, MixWord(0x0fa00029))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(state, MixWord(0x0fa00069))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(state, MixWord(0x0fa000a9))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(state, MixWord(0x0fa000e9))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(state, MixWord(0x0fa00129))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(state, MixWord(0x0fa00169))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(0x0fa00029))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(0x0fa00069))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(0x0fa000a9))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000e9))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa00129))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(0x0fa00169))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I2 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(0x0fa0002a))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006a))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(0x0fa000aa))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ea))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012a))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016a))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(0x0fa0002a))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006a))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(0x0fa000aa))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ea))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012a))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016a))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(state, MixWord(0x0fa0002a))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(state, MixWord(0x0fa0006a))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(state, MixWord(0x0fa000aa))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(state, MixWord(0x0fa000ea))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(state, MixWord(0x0fa0012a))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(state, MixWord(0x0fa0016a))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(0x0fa0002a))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006a))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(0x0fa000aa))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ea))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012a))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016a))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I3 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(0x0fa0002b))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006b))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(0x0fa000ab))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000eb))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012b))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016b))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(0x0fa0002b))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006b))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(0x0fa000ab))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000eb))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012b))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016b))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(state, MixWord(0x0fa0002b))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(state, MixWord(0x0fa0006b))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(state, MixWord(0x0fa000ab))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(state, MixWord(0x0fa000eb))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(state, MixWord(0x0fa0012b))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(state, MixWord(0x0fa0016b))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(0x0fa0002b))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006b))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(0x0fa000ab))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000eb))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012b))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016b))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I4 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(0x0fa0002c))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006c))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(0x0fa000ac))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ec))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012c))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016c))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(0x0fa0002c))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006c))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(0x0fa000ac))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ec))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012c))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016c))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(state, MixWord(0x0fa0002c))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(state, MixWord(0x0fa0006c))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(state, MixWord(0x0fa000ac))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(state, MixWord(0x0fa000ec))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(state, MixWord(0x0fa0012c))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(state, MixWord(0x0fa0016c))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(0x0fa0002c))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006c))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(0x0fa000ac))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ec))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012c))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016c))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I5 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(0x0fa0002d))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006d))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(0x0fa000ad))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ed))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012d))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016d))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(0x0fa0002d))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006d))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(0x0fa000ad))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ed))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012d))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016d))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(state, MixWord(0x0fa0002d))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(state, MixWord(0x0fa0006d))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(state, MixWord(0x0fa000ad))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(state, MixWord(0x0fa000ed))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(state, MixWord(0x0fa0012d))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(state, MixWord(0x0fa0016d))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(0x0fa0002d))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006d))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(0x0fa000ad))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ed))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012d))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016d))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I6 in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x1001)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(0x0fa0002e))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006e))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(0x0fa000ae))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ee))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012e))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016e))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x1000)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(0x0fa0002e))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006e))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(0x0fa000ae))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ee))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012e))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016e))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(state, MixWord(0x0fa0002e))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(state, MixWord(0x0fa0006e))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(state, MixWord(0x0fa000ae))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(state, MixWord(0x0fa000ee))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(state, MixWord(0x0fa0012e))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(state, MixWord(0x0fa0016e))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x0001)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(0x0fa0002e))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(0x0fa0006e))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(0x0fa000ae))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ee))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012e))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016e))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register X in the binary mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x40000001)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(0x0fa0002f))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(0x0fa0006f))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(0x0fa000af))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ef))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012f))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016f))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x40000000)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(0x0fa0002f))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(0x0fa0006f))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(0x0fa000af))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ef))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012f))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016f))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(state, MixWord(0x0fa0002f))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(state, MixWord(0x0fa0006f))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(state, MixWord(0x0fa000af))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(state, MixWord(0x0fa000ef))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(state, MixWord(0x0fa0012f))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(state, MixWord(0x0fa0016f))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x00000001)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(0x0fa0002f))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(0x0fa0006f))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(0x0fa000af))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(0x0fa000ef))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(0x0fa0012f))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(0x0fa0016f))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }
}
