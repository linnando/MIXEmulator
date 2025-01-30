package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.exceptions.ForwardFromTerminalStateException
import org.linnando.mixemulator.vm.{Comparison, decimal}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ControlFlowSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    programCounter = MixIndex(3000)
  )

  "control flow in the decimal mode" should {
    "do nothing on NOP" in {
      // A = 0, I = 0, F = 0, C = 0 NOP
      decimal.execute(state, MixWord(0L)).map(s => {
        s.registers mustEqual state.registers
        s.memory mustEqual state.memory
      })
    }

    "stop execution on HLT" in {
      // A = 0, I = 0, F = 2, C = 5 HLT
      decimal.execute(state, MixWord(205L)).map(s => {
        s.programCounter mustEqual state.programCounter
        s.isHalted mustEqual true
      })
    }

    "perform unconditional jump" in {
      // A = 1000, I = 0, F = 0, C = 39 JMP
      decimal.execute(state, MixWord(1000000039L)).map(s => {
        s.programCounter mustEqual MixIndex(1000)
        s.registers.getJ mustEqual MixIndex(3001)
      })
    }

    "perform unconditional jump saving J" in {
      // A = 1000, I = 0, F = 1, C = 39 JSJ
      decimal.execute(state, MixWord(1000000139L)).map(s => {
        s.programCounter mustEqual MixIndex(1000)
        s.registers.getJ mustEqual MixIndex(0)
      })
    }

    "advance according to the command in memory" in {
      val prevState = state.copy(
        memory = state.memory
          .updated(MixIndex(2000), MixWord(0x400000000L | 116030504L)) // - 1 16 3 5 4
          .updated(MixIndex(3000), MixWord(2000000508L)) // A = 2000, I = 0, F = 0:5, C = 8 LDA
      )
      forward(prevState) map {
        _.registers.getA mustEqual MixWord(0x400000000L | 116030504L)
      }
    }

    "throw an exception on attempt to advance a halted machine" in {
      val prevState = state.copy(isHalted = true)
      recoverToSucceededIf[ForwardFromTerminalStateException] {
        forward(prevState)
      }
    }
  }

  "overflow trigger in the decimal mode" when {
    "set" should {
      "trigger conditional jump on overflow" in {
        val prevState = state.copy(registers = state.registers.updatedOV(true))
        // A = 1000, I = 0, F = 2, C = 39 JOV
        decimal.execute(prevState, MixWord(1000000239L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getOV mustEqual false
        })
      }

      "not trigger conditional jump on no overflow" in {
        val prevState = state.copy(registers = state.registers.updatedOV(true))
        // A = 1000, I = 0, F = 3, C = 39 JNOV
        decimal.execute(prevState, MixWord(1000000339L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getOV mustEqual true
        })
      }
    }

    "not set" should {
      "not trigger conditional jump on overflow" in {
        val prevState = state.copy(registers = state.registers.updatedOV(false))
        // A = 1000, I = 0, F = 2, C = 39 JOV
        decimal.execute(prevState, MixWord(1000000239L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getOV mustEqual false
        })
      }

      "trigger conditional jump on no overflow" in {
        val prevState = state.copy(registers = state.registers.updatedOV(false))
        // A = 1000, I = 0, F = 3, C = 39 JNOV
        decimal.execute(prevState, MixWord(1000000339L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getOV mustEqual false
        })
      }
    }
  }

  "comparison flag in the decimal mode" when {
    "set to LESS" should {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.LESS))

      "trigger conditional jump on less" in {
        // A = 1000, I = 0, F = 4, C = 39 JL
        decimal.execute(prevState, MixWord(1000000439L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        decimal.execute(prevState, MixWord(1000000539L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        decimal.execute(prevState, MixWord(1000000639L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        decimal.execute(prevState, MixWord(1000000739L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        decimal.execute(prevState, MixWord(1000000839L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        decimal.execute(prevState, MixWord(1000000939L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }
    }

    "set to EQUAL" should {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.EQUAL))

      "not trigger conditional jump on less" in {
        // A = 1000, I = 0, F = 4, C = 39 JL
        decimal.execute(prevState, MixWord(1000000439L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        decimal.execute(prevState, MixWord(1000000539L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "not trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        decimal.execute(prevState, MixWord(1000000639L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        decimal.execute(prevState, MixWord(1000000739L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "not trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        decimal.execute(prevState, MixWord(1000000839L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        decimal.execute(prevState, MixWord(1000000939L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }
    }

    "set to GREATER" should {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.GREATER))

      "not trigger conditional jump on less" in {
        // A = 1000, I = 0, F = 4, C = 39 JL
        decimal.execute(prevState, MixWord(1000000439L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "not trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        decimal.execute(prevState, MixWord(1000000539L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        decimal.execute(prevState, MixWord(1000000639L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        decimal.execute(prevState, MixWord(1000000739L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        decimal.execute(prevState, MixWord(1000000839L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "not trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        decimal.execute(prevState, MixWord(1000000939L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }
    }
  }

  "register A in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x400000001L)))

      "trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        decimal.execute(prevState, MixWord(1000000040L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        decimal.execute(prevState, MixWord(1000000140L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        decimal.execute(prevState, MixWord(1000000240L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        decimal.execute(prevState, MixWord(1000000340L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        decimal.execute(prevState, MixWord(1000000440L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        decimal.execute(prevState, MixWord(1000000540L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x400000000L)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        decimal.execute(prevState, MixWord(1000000040L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        decimal.execute(prevState, MixWord(1000000140L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        decimal.execute(prevState, MixWord(1000000240L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        decimal.execute(prevState, MixWord(1000000340L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        decimal.execute(prevState, MixWord(1000000440L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        decimal.execute(prevState, MixWord(1000000540L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        decimal.execute(prevState, MixWord(1000000040L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        decimal.execute(prevState, MixWord(1000000140L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        decimal.execute(prevState, MixWord(1000000240L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        decimal.execute(prevState, MixWord(1000000340L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        decimal.execute(prevState, MixWord(1000000440L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        decimal.execute(prevState, MixWord(1000000540L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(1L)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        decimal.execute(prevState, MixWord(1000000040L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        decimal.execute(prevState, MixWord(1000000140L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        decimal.execute(prevState, MixWord(1000000240L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        decimal.execute(prevState, MixWord(1000000340L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        decimal.execute(prevState, MixWord(1000000440L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        decimal.execute(prevState, MixWord(1000000540L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I1 in the decimal node" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x4001)))

      "trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        decimal.execute(prevState, MixWord(1000000041L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        decimal.execute(prevState, MixWord(1000000141L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        decimal.execute(prevState, MixWord(1000000241L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        decimal.execute(prevState, MixWord(1000000341L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        decimal.execute(prevState, MixWord(1000000441L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        decimal.execute(prevState, MixWord(1000000541L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x4000)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        decimal.execute(prevState, MixWord(1000000041L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        decimal.execute(prevState, MixWord(1000000141L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        decimal.execute(prevState, MixWord(1000000241L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        decimal.execute(prevState, MixWord(1000000341L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        decimal.execute(prevState, MixWord(1000000441L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        decimal.execute(prevState, MixWord(1000000541L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        decimal.execute(prevState, MixWord(1000000041L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        decimal.execute(prevState, MixWord(1000000141L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        decimal.execute(prevState, MixWord(1000000241L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        decimal.execute(prevState, MixWord(1000000341L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        decimal.execute(prevState, MixWord(1000000441L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        decimal.execute(prevState, MixWord(1000000541L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(1)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        decimal.execute(prevState, MixWord(1000000041L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        decimal.execute(prevState, MixWord(1000000141L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        decimal.execute(prevState, MixWord(1000000241L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        decimal.execute(prevState, MixWord(1000000341L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        decimal.execute(prevState, MixWord(1000000441L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        decimal.execute(prevState, MixWord(1000000541L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I2 in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x4001)))

      "trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        decimal.execute(prevState, MixWord(1000000042L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        decimal.execute(prevState, MixWord(1000000142L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        decimal.execute(prevState, MixWord(1000000242L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        decimal.execute(prevState, MixWord(1000000342L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        decimal.execute(prevState, MixWord(1000000442L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        decimal.execute(prevState, MixWord(1000000542L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x4000)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        decimal.execute(prevState, MixWord(1000000042L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        decimal.execute(prevState, MixWord(1000000142L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        decimal.execute(prevState, MixWord(1000000242L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        decimal.execute(prevState, MixWord(1000000342L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        decimal.execute(prevState, MixWord(1000000442L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        decimal.execute(prevState, MixWord(1000000542L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        decimal.execute(prevState, MixWord(1000000042L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        decimal.execute(prevState, MixWord(1000000142L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        decimal.execute(prevState, MixWord(1000000242L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        decimal.execute(prevState, MixWord(1000000342L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        decimal.execute(prevState, MixWord(1000000442L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        decimal.execute(prevState, MixWord(1000000542L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(1)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        decimal.execute(prevState, MixWord(1000000042L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        decimal.execute(prevState, MixWord(1000000142L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        decimal.execute(prevState, MixWord(1000000242L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        decimal.execute(prevState, MixWord(1000000342L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        decimal.execute(prevState, MixWord(1000000442L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        decimal.execute(prevState, MixWord(1000000542L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I3 in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x4001)))

      "trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        decimal.execute(prevState, MixWord(1000000043L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        decimal.execute(prevState, MixWord(1000000143L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        decimal.execute(prevState, MixWord(1000000243L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        decimal.execute(prevState, MixWord(1000000343L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        decimal.execute(prevState, MixWord(1000000443L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        decimal.execute(prevState, MixWord(1000000543L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x4000)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        decimal.execute(prevState, MixWord(1000000043L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        decimal.execute(prevState, MixWord(1000000143L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        decimal.execute(prevState, MixWord(1000000243L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        decimal.execute(prevState, MixWord(1000000343L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        decimal.execute(prevState, MixWord(1000000443L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        decimal.execute(prevState, MixWord(1000000543L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        decimal.execute(prevState, MixWord(1000000043L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        decimal.execute(prevState, MixWord(1000000143L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        decimal.execute(prevState, MixWord(1000000243L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        decimal.execute(prevState, MixWord(1000000343L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        decimal.execute(prevState, MixWord(1000000443L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        decimal.execute(prevState, MixWord(1000000543L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(1)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        decimal.execute(prevState, MixWord(1000000043L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        decimal.execute(prevState, MixWord(1000000143L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        decimal.execute(prevState, MixWord(1000000243L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        decimal.execute(prevState, MixWord(1000000343L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        decimal.execute(prevState, MixWord(1000000443L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        decimal.execute(prevState, MixWord(1000000543L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I4 in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x4001)))

      "trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        decimal.execute(prevState, MixWord(1000000044L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        decimal.execute(prevState, MixWord(1000000144L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        decimal.execute(prevState, MixWord(1000000244L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        decimal.execute(prevState, MixWord(1000000344L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        decimal.execute(prevState, MixWord(1000000444L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        decimal.execute(prevState, MixWord(1000000544L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x4000)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        decimal.execute(prevState, MixWord(1000000044L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        decimal.execute(prevState, MixWord(1000000144L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        decimal.execute(prevState, MixWord(1000000244L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        decimal.execute(prevState, MixWord(1000000344L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        decimal.execute(prevState, MixWord(1000000444L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        decimal.execute(prevState, MixWord(1000000544L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        decimal.execute(prevState, MixWord(1000000044L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        decimal.execute(prevState, MixWord(1000000144L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        decimal.execute(prevState, MixWord(1000000244L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        decimal.execute(prevState, MixWord(1000000344L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        decimal.execute(prevState, MixWord(1000000444L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        decimal.execute(prevState, MixWord(1000000544L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(1)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        decimal.execute(prevState, MixWord(1000000044L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        decimal.execute(prevState, MixWord(1000000144L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        decimal.execute(prevState, MixWord(1000000244L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        decimal.execute(prevState, MixWord(1000000344L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        decimal.execute(prevState, MixWord(1000000444L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        decimal.execute(prevState, MixWord(1000000544L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I5 in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x4001)))

      "trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        decimal.execute(prevState, MixWord(1000000045L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        decimal.execute(prevState, MixWord(1000000145L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        decimal.execute(prevState, MixWord(1000000245L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        decimal.execute(prevState, MixWord(1000000345L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        decimal.execute(prevState, MixWord(1000000445L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        decimal.execute(prevState, MixWord(1000000545L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x4000)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        decimal.execute(prevState, MixWord(1000000045L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        decimal.execute(prevState, MixWord(1000000145L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        decimal.execute(prevState, MixWord(1000000245L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        decimal.execute(prevState, MixWord(1000000345L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        decimal.execute(prevState, MixWord(1000000445L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        decimal.execute(prevState, MixWord(1000000545L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        decimal.execute(prevState, MixWord(1000000045L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        decimal.execute(prevState, MixWord(1000000145L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        decimal.execute(prevState, MixWord(1000000245L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        decimal.execute(prevState, MixWord(1000000345L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        decimal.execute(prevState, MixWord(1000000445L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        decimal.execute(prevState, MixWord(1000000545L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(1)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        decimal.execute(prevState, MixWord(1000000045L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        decimal.execute(prevState, MixWord(1000000145L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        decimal.execute(prevState, MixWord(1000000245L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        decimal.execute(prevState, MixWord(1000000345L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        decimal.execute(prevState, MixWord(1000000445L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        decimal.execute(prevState, MixWord(1000000545L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I6 in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x4001)))

      "trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        decimal.execute(prevState, MixWord(1000000046L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        decimal.execute(prevState, MixWord(1000000146L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        decimal.execute(prevState, MixWord(1000000246L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        decimal.execute(prevState, MixWord(1000000346L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        decimal.execute(prevState, MixWord(1000000446L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        decimal.execute(prevState, MixWord(1000000546L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x4000)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        decimal.execute(prevState, MixWord(1000000046L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        decimal.execute(prevState, MixWord(1000000146L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        decimal.execute(prevState, MixWord(1000000246L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        decimal.execute(prevState, MixWord(1000000346L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        decimal.execute(prevState, MixWord(1000000446L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        decimal.execute(prevState, MixWord(1000000546L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        decimal.execute(prevState, MixWord(1000000046L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        decimal.execute(prevState, MixWord(1000000146L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        decimal.execute(prevState, MixWord(1000000246L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        decimal.execute(prevState, MixWord(1000000346L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        decimal.execute(prevState, MixWord(1000000446L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        decimal.execute(prevState, MixWord(1000000546L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(1)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        decimal.execute(prevState, MixWord(1000000046L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        decimal.execute(prevState, MixWord(1000000146L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        decimal.execute(prevState, MixWord(1000000246L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        decimal.execute(prevState, MixWord(1000000346L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        decimal.execute(prevState, MixWord(1000000446L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        decimal.execute(prevState, MixWord(1000000546L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register X in the decimal mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x400000001L)))

      "trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        decimal.execute(prevState, MixWord(1000000047L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        decimal.execute(prevState, MixWord(1000000147L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        decimal.execute(prevState, MixWord(1000000247L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        decimal.execute(prevState, MixWord(1000000347L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        decimal.execute(prevState, MixWord(1000000447L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        decimal.execute(prevState, MixWord(1000000547L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x400000000L)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        decimal.execute(prevState, MixWord(1000000047L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        decimal.execute(prevState, MixWord(1000000147L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        decimal.execute(prevState, MixWord(1000000247L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        decimal.execute(prevState, MixWord(1000000347L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        decimal.execute(prevState, MixWord(1000000447L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        decimal.execute(prevState, MixWord(1000000547L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        decimal.execute(prevState, MixWord(1000000047L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        decimal.execute(prevState, MixWord(1000000147L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        decimal.execute(prevState, MixWord(1000000247L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        decimal.execute(prevState, MixWord(1000000347L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        decimal.execute(prevState, MixWord(1000000447L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        decimal.execute(prevState, MixWord(1000000547L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(1L)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        decimal.execute(prevState, MixWord(1000000047L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        decimal.execute(prevState, MixWord(1000000147L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        decimal.execute(prevState, MixWord(1000000247L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        decimal.execute(prevState, MixWord(1000000347L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        decimal.execute(prevState, MixWord(1000000447L)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        decimal.execute(prevState, MixWord(1000000547L)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }
}
