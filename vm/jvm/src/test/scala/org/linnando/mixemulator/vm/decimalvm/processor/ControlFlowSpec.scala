package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.exceptions.ForwardFromTerminalStateException
import org.linnando.mixemulator.vm.{Comparison, decimal}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ControlFlowSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    programCounter = MixIndex(3000)
  )

  "control flow in the decimal mode" should {
    "do nothing on NOP" in {
      // A = 0, I = 0, F = 0, C = 0 NOP
      val nextState = execute(state, MixWord(0L))
      nextState.map(_.registers) must beEqualTo(state.registers).await
      nextState.map(_.memory) must beEqualTo(state.memory).await
    }

    "stop execution on HLT" in {
      // A = 0, I = 0, F = 2, C = 5 HLT
      val nextState = execute(state, MixWord(205L))
      nextState.map(_.programCounter) must beEqualTo(state.programCounter).await
      nextState.map(_.isHalted) must beTrue.await
    }

    "perform unconditional jump" in {
      // A = 1000, I = 0, F = 0, C = 39 JMP
      val nextState = execute(state, MixWord(1000000039L))
      nextState.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextState.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "perform unconditional jump saving J" in {
      // A = 1000, I = 0, F = 1, C = 39 JSJ
      val nextState = execute(state, MixWord(1000000139L))
      nextState.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextState.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }

    "advance according to the command in memory" in {
      val prevState = state.copy(
        memory = state.memory
          .updated(MixIndex(2000), MixWord(0x400000000L | 116030504L)) // - 1 16 3 5 4
          .updated(MixIndex(3000), MixWord(2000000508L)) // A = 2000, I = 0, F = 0:5, C = 8 LDA
      )
      val nextState = forward(prevState)
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 116030504L)).await
    }

    "throw an exception on attempt to advance a halted machine" in {
      val prevState = state.copy(isHalted = true)
      forward(prevState) must throwA[ForwardFromTerminalStateException].await
    }
  }

  "overflow trigger in the decimal mode" should {
    "trigger jump on overflow and not trigger jump on no overflow when set" in {
      val prevState = state.copy(registers = state.registers.updatedOV(true))
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(prevState, MixWord(1000000239L))
      nextStateOV.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateOV.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(prevState, MixWord(1000000339L))
      nextStateNOV.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNOV.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateNOV.map(_.registers.getOV) must beTrue.await
    }

    "not trigger jump on overflow and trigger jump on no overflow when not set" in {
      // A = 1000, I = 0, F = 2, C = 39 JOV
      val nextStateOV = execute(state, MixWord(1000000239L))
      nextStateOV.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
      // A = 1000, I = 0, F = 3, C = 39 JNOV
      val nextStateNOV = execute(state, MixWord(1000000339L))
      nextStateNOV.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNOV.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateOV.map(_.registers.getOV) must beFalse.await
    }
  }

  "comparison flag in the decimal mode" should {
    "trigger jumps accordingly when equals to LESS" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.LESS))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, MixWord(1000000439L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, MixWord(1000000539L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, MixWord(1000000639L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, MixWord(1000000739L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, MixWord(1000000839L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, MixWord(1000000939L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
    }

    "trigger jumps accordingly when equals to EQUAL" in {
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(state, MixWord(1000000439L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(state, MixWord(1000000539L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(state, MixWord(1000000639L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(state, MixWord(1000000739L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(state, MixWord(1000000839L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(state, MixWord(1000000939L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "trigger jumps accordingly when equals to GREATER" in {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.GREATER))
      // A = 1000, I = 0, F = 4, C = 39 JL
      val nextStateL = execute(prevState, MixWord(1000000439L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateL.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 5, C = 39 JE
      val nextStateE = execute(prevState, MixWord(1000000539L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 6, C = 39 JG
      val nextStateG = execute(prevState, MixWord(1000000639L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 7, C = 39 JGE
      val nextStateGE = execute(prevState, MixWord(1000000739L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 8, C = 39 JNE
      val nextStateNE = execute(prevState, MixWord(1000000839L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 9, C = 39 JLE
      val nextStateLE = execute(prevState, MixWord(1000000939L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      nextStateLE.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }
  }

  "register A in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x400000001L)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(1000000040L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(1000000140L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(1000000240L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(1000000340L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(1000000440L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(1000000540L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x400000000L)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(1000000040L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(1000000140L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(1000000240L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(1000000340L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(1000000440L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(1000000540L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(state, MixWord(1000000040L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(state, MixWord(1000000140L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(state, MixWord(1000000240L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(state, MixWord(1000000340L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(state, MixWord(1000000440L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(state, MixWord(1000000540L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(1L)))
      // A = 1000, I = 0, F = 0, C = 40 JAN
      val nextStateL = execute(prevState, MixWord(1000000040L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 40 JAZ
      val nextStateE = execute(prevState, MixWord(1000000140L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 40 JAP
      val nextStateG = execute(prevState, MixWord(1000000240L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 40 JANN
      val nextStateGE = execute(prevState, MixWord(1000000340L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 40 JANZ
      val nextStateNE = execute(prevState, MixWord(1000000440L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 40 JANP
      val nextStateLE = execute(prevState, MixWord(1000000540L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I1 in the decimal node" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(1000000041L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(1000000141L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(1000000241L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(1000000341L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(1000000441L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(1000000541L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(1000000041L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(1000000141L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(1000000241L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(1000000341L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(1000000441L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(1000000541L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(state, MixWord(1000000041L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(state, MixWord(1000000141L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(state, MixWord(1000000241L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(state, MixWord(1000000341L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(state, MixWord(1000000441L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(state, MixWord(1000000541L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 41 J1N
      val nextStateL = execute(prevState, MixWord(1000000041L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 41 J1Z
      val nextStateE = execute(prevState, MixWord(1000000141L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 41 J1P
      val nextStateG = execute(prevState, MixWord(1000000241L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 41 J1NN
      val nextStateGE = execute(prevState, MixWord(1000000341L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 41 J1NZ
      val nextStateNE = execute(prevState, MixWord(1000000441L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 41 J1NP
      val nextStateLE = execute(prevState, MixWord(1000000541L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I2 in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(1000000042L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(1000000142L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(1000000242L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(1000000342L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(1000000442L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(1000000542L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(1000000042L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(1000000142L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(1000000242L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(1000000342L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(1000000442L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(1000000542L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(state, MixWord(1000000042L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(state, MixWord(1000000142L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(state, MixWord(1000000242L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(state, MixWord(1000000342L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(state, MixWord(1000000442L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(state, MixWord(1000000542L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 42 J2N
      val nextStateL = execute(prevState, MixWord(1000000042L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 42 J2Z
      val nextStateE = execute(prevState, MixWord(1000000142L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 42 J2P
      val nextStateG = execute(prevState, MixWord(1000000242L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 42 J2NN
      val nextStateGE = execute(prevState, MixWord(1000000342L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 42 J2NZ
      val nextStateNE = execute(prevState, MixWord(1000000442L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 42 J2NP
      val nextStateLE = execute(prevState, MixWord(1000000542L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I3 in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(1000000043L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(1000000143L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(1000000243L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(1000000343L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(1000000443L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(1000000543L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(1000000043L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(1000000143L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(1000000243L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(1000000343L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(1000000443L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(1000000543L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(state, MixWord(1000000043L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(state, MixWord(1000000143L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(state, MixWord(1000000243L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(state, MixWord(1000000343L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(state, MixWord(1000000443L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(state, MixWord(1000000543L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 43 J3N
      val nextStateL = execute(prevState, MixWord(1000000043L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 43 J3Z
      val nextStateE = execute(prevState, MixWord(1000000143L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 43 J3P
      val nextStateG = execute(prevState, MixWord(1000000243L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 43 J3NN
      val nextStateGE = execute(prevState, MixWord(1000000343L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 43 J3NZ
      val nextStateNE = execute(prevState, MixWord(1000000443L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 43 J3NP
      val nextStateLE = execute(prevState, MixWord(1000000543L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I4 in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(1000000044L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(1000000144L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(1000000244L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(1000000344L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(1000000444L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(1000000544L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(1000000044L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(1000000144L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(1000000244L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(1000000344L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(1000000444L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(1000000544L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(state, MixWord(1000000044L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(state, MixWord(1000000144L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(state, MixWord(1000000244L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(state, MixWord(1000000344L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(state, MixWord(1000000444L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(state, MixWord(1000000544L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 44 J4N
      val nextStateL = execute(prevState, MixWord(1000000044L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 44 J4Z
      val nextStateE = execute(prevState, MixWord(1000000144L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 44 J4P
      val nextStateG = execute(prevState, MixWord(1000000244L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 44 J4NN
      val nextStateGE = execute(prevState, MixWord(1000000344L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 44 J4NZ
      val nextStateNE = execute(prevState, MixWord(1000000444L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 44 J4NP
      val nextStateLE = execute(prevState, MixWord(1000000544L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I5 in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(1000000045L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(1000000145L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(1000000245L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(1000000345L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(1000000445L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(1000000545L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(1000000045L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(1000000145L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(1000000245L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(1000000345L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(1000000445L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(1000000545L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(state, MixWord(1000000045L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(state, MixWord(1000000145L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(state, MixWord(1000000245L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(state, MixWord(1000000345L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(state, MixWord(1000000445L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(state, MixWord(1000000545L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 45 J5N
      val nextStateL = execute(prevState, MixWord(1000000045L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 45 J5Z
      val nextStateE = execute(prevState, MixWord(1000000145L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 45 J5P
      val nextStateG = execute(prevState, MixWord(1000000245L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 45 J5NN
      val nextStateGE = execute(prevState, MixWord(1000000345L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 45 J5NZ
      val nextStateNE = execute(prevState, MixWord(1000000445L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 45 J5NP
      val nextStateLE = execute(prevState, MixWord(1000000545L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register I6 in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x4001)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(1000000046L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(1000000146L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(1000000246L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(1000000346L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(1000000446L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(1000000546L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x4000)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(1000000046L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(1000000146L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(1000000246L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(1000000346L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(1000000446L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(1000000546L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(state, MixWord(1000000046L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(state, MixWord(1000000146L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(state, MixWord(1000000246L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(state, MixWord(1000000346L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(state, MixWord(1000000446L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(state, MixWord(1000000546L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(1)))
      // A = 1000, I = 0, F = 0, C = 46 J6N
      val nextStateL = execute(prevState, MixWord(1000000046L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 46 J6Z
      val nextStateE = execute(prevState, MixWord(1000000146L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 46 J6P
      val nextStateG = execute(prevState, MixWord(1000000246L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 46 J6NN
      val nextStateGE = execute(prevState, MixWord(1000000346L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 46 J6NZ
      val nextStateNE = execute(prevState, MixWord(1000000446L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 46 J6NP
      val nextStateLE = execute(prevState, MixWord(1000000546L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }

  "register X in the decimal mode" should {
    "trigger jumps accordingly when is less than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x400000001L)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(1000000047L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(1000000147L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(1000000247L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(1000000347L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(1000000447L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(1000000547L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the negative zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x400000000L)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(1000000047L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(1000000147L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(1000000247L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(1000000347L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(1000000447L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(1000000547L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when equals to the positive zero" in {
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(state, MixWord(1000000047L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(state, MixWord(1000000147L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(state, MixWord(1000000247L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(state, MixWord(1000000347L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(state, MixWord(1000000447L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(state, MixWord(1000000547L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
    }

    "trigger jumps accordingly when is greater than zero" in {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(1L)))
      // A = 1000, I = 0, F = 0, C = 47 JXN
      val nextStateL = execute(prevState, MixWord(1000000047L))
      nextStateL.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateL.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 1, C = 47 JXZ
      val nextStateE = execute(prevState, MixWord(1000000147L))
      nextStateE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
      // A = 1000, I = 0, F = 2, C = 47 JXP
      val nextStateG = execute(prevState, MixWord(1000000247L))
      nextStateG.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateG.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 3, C = 47 JXNN
      val nextStateGE = execute(prevState, MixWord(1000000347L))
      nextStateGE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateGE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 4, C = 47 JXNZ
      val nextStateNE = execute(prevState, MixWord(1000000447L))
      nextStateNE.map(_.programCounter) must beEqualTo(MixIndex(1000)).await
      nextStateNE.map(_.registers.getJ) must beEqualTo(MixIndex(3001)).await
      // A = 1000, I = 0, F = 5, C = 47 JXNP
      val nextStateLE = execute(prevState, MixWord(1000000547L))
      nextStateLE.map(_.programCounter) must beEqualTo(MixIndex(3001)).await
      nextStateLE.map(_.registers.getJ) must beEqualTo(MixIndex(0)).await
    }
  }
}
