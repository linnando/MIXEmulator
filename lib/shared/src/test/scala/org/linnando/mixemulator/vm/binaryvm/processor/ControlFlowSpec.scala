package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.exceptions.ForwardFromTerminalStateException
import org.linnando.mixemulator.vm.{Comparison, binary}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ControlFlowSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    programCounter = MixIndex(3000)
  )

  "control flow in the binary mode" should {
    "do nothing on NOP" in {
      // A = 0, I = 0, F = 0, C = 0 NOP
      binary.execute(state, MixWord(0x00000000)).map(s => {
        s.registers mustEqual state.registers
        s.memory mustEqual state.memory
      })
    }

    "stop execution on HLT" in {
      // A = 0, I = 0, F = 2, C = 5 HLT
      binary.execute(state, MixWord(0x00000085)).map(s => {
        s.programCounter mustEqual state.programCounter
        s.isHalted mustEqual true
      })
    }

    "perform unconditional jump" in {
      // A = 1000, I = 0, F = 0, C = 39 JMP
      binary.execute(state, MixWord(0x0fa00027)).map(s => {
        s.programCounter mustEqual MixIndex(1000)
        s.registers.getJ mustEqual MixIndex(3001)
      })
    }

    "perform unconditional jump saving J" in {
      // A = 1000, I = 0, F = 1, C = 39 JSJ
      binary.execute(state, MixWord(0x0fa00067)).map(s => {
        s.programCounter mustEqual MixIndex(1000)
        s.registers.getJ mustEqual MixIndex(0)
      })
    }

    "advance according to the command in memory" in {
      val prevState = state.copy(
        memory = state.memory
          .updated(MixIndex(2000), MixWord(0x41403144)) // - 1 16 3 5 4
          .updated(MixIndex(3000), MixWord(0x1f400148)) // A = 2000, I = 0, F = 0:5, C = 8 LDA
      )
      forward(prevState) map {
        _.registers.getA mustEqual MixWord(0x41403144)
      }
    }

    "throw an exception on attempt to advance a halted machine" in {
      val prevState = state.copy(isHalted = true)
      recoverToSucceededIf[ForwardFromTerminalStateException] {
        forward(prevState)
      }
    }
  }

  "overflow trigger in the binary mode" when {
    "set" should {
      val prevState = state.copy(registers = state.registers.updatedOV(true))

      "trigger conditional jump on overflow" in {
        // A = 1000, I = 0, F = 2, C = 39 JOV
        binary.execute(prevState, MixWord(0x0fa000a7)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getOV mustEqual false
        })
      }

      "not trigger conditional jump on no overflow" in {
        // A = 1000, I = 0, F = 3, C = 39 JNOV
        binary.execute(prevState, MixWord(0x0fa000e7)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getOV mustEqual true
        })
      }
    }

    "not set" should {
      val prevState = state.copy(registers = state.registers.updatedOV(false))

      "not trigger conditional jump on overflow" in {
        // A = 1000, I = 0, F = 2, C = 39 JOV
        binary.execute(prevState, MixWord(0x0fa000a7)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getOV mustEqual false
        })
      }

      "trigger conditional jump on no overflow" in {
        // A = 1000, I = 0, F = 3, C = 39 JNOV
        binary.execute(prevState, MixWord(0x0fa000e7)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getOV mustEqual false
        })
      }
    }
  }

  "comparison flag in the binary mode" when {
    "set to LESS" should {
      val prevState = state.copy(registers = state.registers.updatedCMP(Comparison.LESS))

      "trigger conditional jump on less" in {
        // A = 1000, I = 0, F = 4, C = 39 JL
        binary.execute(prevState, MixWord(0x0fa00127)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        binary.execute(prevState, MixWord(0x0fa00167)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        binary.execute(prevState, MixWord(0x0fa001a7)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "not trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        binary.execute(prevState, MixWord(0x0fa001e7)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        binary.execute(prevState, MixWord(0x0fa00227)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.LESS
        })
      }

      "trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        binary.execute(prevState, MixWord(0x0fa00267)).map(s => {
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
        binary.execute(prevState, MixWord(0x0fa00127)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        binary.execute(prevState, MixWord(0x0fa00167)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "not trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        binary.execute(prevState, MixWord(0x0fa001a7)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        binary.execute(prevState, MixWord(0x0fa001e7)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "not trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        binary.execute(prevState, MixWord(0x0fa00227)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.EQUAL
        })
      }

      "trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        binary.execute(prevState, MixWord(0x0fa00267)).map(s => {
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
        binary.execute(prevState, MixWord(0x0fa00127)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "not trigger conditional jump on equal" in {
        // A = 1000, I = 0, F = 5, C = 39 JE
        binary.execute(prevState, MixWord(0x0fa00167)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on greater" in {
        // A = 1000, I = 0, F = 6, C = 39 JG
        binary.execute(prevState, MixWord(0x0fa001a7)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on greater or equal" in {
        // A = 1000, I = 0, F = 7, C = 39 JGE
        binary.execute(prevState, MixWord(0x0fa001e7)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "trigger conditional jump on not equal" in {
        // A = 1000, I = 0, F = 8, C = 39 JNE
        binary.execute(prevState, MixWord(0x0fa00227)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }

      "not trigger conditional jump on less or equal" in {
        // A = 1000, I = 0, F = 9, C = 39 JLE
        binary.execute(prevState, MixWord(0x0fa00267)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
          s.registers.getCMP mustEqual Comparison.GREATER
        })
      }
    }
  }

  "register A in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x40000001)))

      "trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        binary.execute(prevState, MixWord(0x0fa00028)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        binary.execute(prevState, MixWord(0x0fa00068)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        binary.execute(prevState, MixWord(0x0fa000a8)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        binary.execute(prevState, MixWord(0x0fa000e8)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        binary.execute(prevState, MixWord(0x0fa00128)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        binary.execute(prevState, MixWord(0x0fa00168)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x40000000)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        binary.execute(prevState, MixWord(0x0fa00028)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        binary.execute(prevState, MixWord(0x0fa00068)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        binary.execute(prevState, MixWord(0x0fa000a8)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        binary.execute(prevState, MixWord(0x0fa000e8)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        binary.execute(prevState, MixWord(0x0fa00128)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        binary.execute(prevState, MixWord(0x0fa00168)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x0)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        binary.execute(prevState, MixWord(0x0fa00028)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        binary.execute(prevState, MixWord(0x0fa00068)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        binary.execute(prevState, MixWord(0x0fa000a8)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        binary.execute(prevState, MixWord(0x0fa000e8)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        binary.execute(prevState, MixWord(0x0fa00128)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        binary.execute(prevState, MixWord(0x0fa00168)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedA(MixWord(0x00000001)))

      "not trigger conditional jump on negative A" in {
        // A = 1000, I = 0, F = 0, C = 40 JAN
        binary.execute(prevState, MixWord(0x0fa00028)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero A" in {
        // A = 1000, I = 0, F = 1, C = 40 JAZ
        binary.execute(prevState, MixWord(0x0fa00068)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive A" in {
        // A = 1000, I = 0, F = 2, C = 40 JAP
        binary.execute(prevState, MixWord(0x0fa000a8)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative A" in {
        // A = 1000, I = 0, F = 3, C = 40 JANN
        binary.execute(prevState, MixWord(0x0fa000e8)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero A" in {
        // A = 1000, I = 0, F = 4, C = 40 JANZ
        binary.execute(prevState, MixWord(0x0fa00128)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive A" in {
        // A = 1000, I = 0, F = 5, C = 40 JANP
        binary.execute(prevState, MixWord(0x0fa00168)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I1 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x1001)))

      "trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        binary.execute(prevState, MixWord(0x0fa00029)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        binary.execute(prevState, MixWord(0x0fa00069)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        binary.execute(prevState, MixWord(0x0fa000a9)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        binary.execute(prevState, MixWord(0x0fa000e9)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        binary.execute(prevState, MixWord(0x0fa00129)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        binary.execute(prevState, MixWord(0x0fa00169)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x1000)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        binary.execute(prevState, MixWord(0x0fa00029)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        binary.execute(prevState, MixWord(0x0fa00069)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        binary.execute(prevState, MixWord(0x0fa000a9)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        binary.execute(prevState, MixWord(0x0fa000e9)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        binary.execute(prevState, MixWord(0x0fa00129)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        binary.execute(prevState, MixWord(0x0fa00169)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x0)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        binary.execute(prevState, MixWord(0x0fa00029)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        binary.execute(prevState, MixWord(0x0fa00069)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        binary.execute(prevState, MixWord(0x0fa000a9)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        binary.execute(prevState, MixWord(0x0fa000e9)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        binary.execute(prevState, MixWord(0x0fa00129)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        binary.execute(prevState, MixWord(0x0fa00169)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(1, MixIndex(0x0001)))

      "not trigger conditional jump on negative I1" in {
        // A = 1000, I = 0, F = 0, C = 41 J1N
        binary.execute(prevState, MixWord(0x0fa00029)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I1" in {
        // A = 1000, I = 0, F = 1, C = 41 J1Z
        binary.execute(prevState, MixWord(0x0fa00069)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I1" in {
        // A = 1000, I = 0, F = 2, C = 41 J1P
        binary.execute(prevState, MixWord(0x0fa000a9)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I1" in {
        // A = 1000, I = 0, F = 3, C = 41 J1NN
        binary.execute(prevState, MixWord(0x0fa000e9)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I1" in {
        // A = 1000, I = 0, F = 4, C = 41 J1NZ
        binary.execute(prevState, MixWord(0x0fa00129)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I1" in {
        // A = 1000, I = 0, F = 5, C = 41 J1NP
        binary.execute(prevState, MixWord(0x0fa00169)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I2 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x1001)))

      "trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        binary.execute(prevState, MixWord(0x0fa0002a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        binary.execute(prevState, MixWord(0x0fa0006a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        binary.execute(prevState, MixWord(0x0fa000aa)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        binary.execute(prevState, MixWord(0x0fa000ea)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        binary.execute(prevState, MixWord(0x0fa0012a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        binary.execute(prevState, MixWord(0x0fa0016a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x1000)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        binary.execute(prevState, MixWord(0x0fa0002a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        binary.execute(prevState, MixWord(0x0fa0006a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        binary.execute(prevState, MixWord(0x0fa000aa)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        binary.execute(prevState, MixWord(0x0fa000ea)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        binary.execute(prevState, MixWord(0x0fa0012a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        binary.execute(prevState, MixWord(0x0fa0016a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x0)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        binary.execute(prevState, MixWord(0x0fa0002a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        binary.execute(prevState, MixWord(0x0fa0006a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        binary.execute(prevState, MixWord(0x0fa000aa)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        binary.execute(prevState, MixWord(0x0fa000ea)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        binary.execute(prevState, MixWord(0x0fa0012a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        binary.execute(prevState, MixWord(0x0fa0016a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(2, MixIndex(0x0001)))

      "not trigger conditional jump on negative I2" in {
        // A = 1000, I = 0, F = 0, C = 42 J2N
        binary.execute(prevState, MixWord(0x0fa0002a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I2" in {
        // A = 1000, I = 0, F = 1, C = 42 J2Z
        binary.execute(prevState, MixWord(0x0fa0006a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I2" in {
        // A = 1000, I = 0, F = 2, C = 42 J2P
        binary.execute(prevState, MixWord(0x0fa000aa)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I2" in {
        // A = 1000, I = 0, F = 3, C = 42 J2NN
        binary.execute(prevState, MixWord(0x0fa000ea)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I2" in {
        // A = 1000, I = 0, F = 4, C = 42 J2NZ
        binary.execute(prevState, MixWord(0x0fa0012a)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I2" in {
        // A = 1000, I = 0, F = 5, C = 42 J2NP
        binary.execute(prevState, MixWord(0x0fa0016a)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I3 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x1001)))

      "trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        binary.execute(prevState, MixWord(0x0fa0002b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        binary.execute(prevState, MixWord(0x0fa0006b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        binary.execute(prevState, MixWord(0x0fa000ab)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        binary.execute(prevState, MixWord(0x0fa000eb)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        binary.execute(prevState, MixWord(0x0fa0012b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        binary.execute(prevState, MixWord(0x0fa0016b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x1000)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        binary.execute(prevState, MixWord(0x0fa0002b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        binary.execute(prevState, MixWord(0x0fa0006b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        binary.execute(prevState, MixWord(0x0fa000ab)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        binary.execute(prevState, MixWord(0x0fa000eb)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        binary.execute(prevState, MixWord(0x0fa0012b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        binary.execute(prevState, MixWord(0x0fa0016b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x0)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        binary.execute(prevState, MixWord(0x0fa0002b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        binary.execute(prevState, MixWord(0x0fa0006b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        binary.execute(prevState, MixWord(0x0fa000ab)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        binary.execute(prevState, MixWord(0x0fa000eb)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        binary.execute(prevState, MixWord(0x0fa0012b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        binary.execute(prevState, MixWord(0x0fa0016b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(3, MixIndex(0x0001)))

      "not trigger conditional jump on negative I3" in {
        // A = 1000, I = 0, F = 0, C = 43 J3N
        binary.execute(prevState, MixWord(0x0fa0002b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I3" in {
        // A = 1000, I = 0, F = 1, C = 43 J3Z
        binary.execute(prevState, MixWord(0x0fa0006b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I3" in {
        // A = 1000, I = 0, F = 2, C = 43 J3P
        binary.execute(prevState, MixWord(0x0fa000ab)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I3" in {
        // A = 1000, I = 0, F = 3, C = 43 J3NN
        binary.execute(prevState, MixWord(0x0fa000eb)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I3" in {
        // A = 1000, I = 0, F = 4, C = 43 J3NZ
        binary.execute(prevState, MixWord(0x0fa0012b)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I3" in {
        // A = 1000, I = 0, F = 5, C = 43 J3NP
        binary.execute(prevState, MixWord(0x0fa0016b)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I4 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x1001)))

      "trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        binary.execute(prevState, MixWord(0x0fa0002c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        binary.execute(prevState, MixWord(0x0fa0006c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        binary.execute(prevState, MixWord(0x0fa000ac)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        binary.execute(prevState, MixWord(0x0fa000ec)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        binary.execute(prevState, MixWord(0x0fa0012c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        binary.execute(prevState, MixWord(0x0fa0016c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x1000)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        binary.execute(prevState, MixWord(0x0fa0002c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        binary.execute(prevState, MixWord(0x0fa0006c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        binary.execute(prevState, MixWord(0x0fa000ac)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        binary.execute(prevState, MixWord(0x0fa000ec)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        binary.execute(prevState, MixWord(0x0fa0012c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        binary.execute(prevState, MixWord(0x0fa0016c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x0)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        binary.execute(prevState, MixWord(0x0fa0002c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        binary.execute(prevState, MixWord(0x0fa0006c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        binary.execute(prevState, MixWord(0x0fa000ac)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        binary.execute(prevState, MixWord(0x0fa000ec)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        binary.execute(prevState, MixWord(0x0fa0012c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        binary.execute(prevState, MixWord(0x0fa0016c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(4, MixIndex(0x0001)))

      "not trigger conditional jump on negative I4" in {
        // A = 1000, I = 0, F = 0, C = 44 J4N
        binary.execute(prevState, MixWord(0x0fa0002c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I4" in {
        // A = 1000, I = 0, F = 1, C = 44 J4Z
        binary.execute(prevState, MixWord(0x0fa0006c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I4" in {
        // A = 1000, I = 0, F = 2, C = 44 J4P
        binary.execute(prevState, MixWord(0x0fa000ac)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I4" in {
        // A = 1000, I = 0, F = 3, C = 44 J4NN
        binary.execute(prevState, MixWord(0x0fa000ec)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I4" in {
        // A = 1000, I = 0, F = 4, C = 44 J4NZ
        binary.execute(prevState, MixWord(0x0fa0012c)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I4" in {
        // A = 1000, I = 0, F = 5, C = 44 J4NP
        binary.execute(prevState, MixWord(0x0fa0016c)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I5 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x1001)))

      "trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        binary.execute(prevState, MixWord(0x0fa0002d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        binary.execute(prevState, MixWord(0x0fa0006d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        binary.execute(prevState, MixWord(0x0fa000ad)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        binary.execute(prevState, MixWord(0x0fa000ed)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        binary.execute(prevState, MixWord(0x0fa0012d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        binary.execute(prevState, MixWord(0x0fa0016d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x1000)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        binary.execute(prevState, MixWord(0x0fa0002d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        binary.execute(prevState, MixWord(0x0fa0006d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        binary.execute(prevState, MixWord(0x0fa000ad)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        binary.execute(prevState, MixWord(0x0fa000ed)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        binary.execute(prevState, MixWord(0x0fa0012d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        binary.execute(prevState, MixWord(0x0fa0016d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x0)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        binary.execute(prevState, MixWord(0x0fa0002d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        binary.execute(prevState, MixWord(0x0fa0006d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        binary.execute(prevState, MixWord(0x0fa000ad)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        binary.execute(prevState, MixWord(0x0fa000ed)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        binary.execute(prevState, MixWord(0x0fa0012d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        binary.execute(prevState, MixWord(0x0fa0016d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(5, MixIndex(0x0001)))

      "not trigger conditional jump on negative I5" in {
        // A = 1000, I = 0, F = 0, C = 45 J5N
        binary.execute(prevState, MixWord(0x0fa0002d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I5" in {
        // A = 1000, I = 0, F = 1, C = 45 J5Z
        binary.execute(prevState, MixWord(0x0fa0006d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I5" in {
        // A = 1000, I = 0, F = 2, C = 45 J5P
        binary.execute(prevState, MixWord(0x0fa000ad)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I5" in {
        // A = 1000, I = 0, F = 3, C = 45 J5NN
        binary.execute(prevState, MixWord(0x0fa000ed)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I5" in {
        // A = 1000, I = 0, F = 4, C = 45 J5NZ
        binary.execute(prevState, MixWord(0x0fa0012d)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I5" in {
        // A = 1000, I = 0, F = 5, C = 45 J5NP
        binary.execute(prevState, MixWord(0x0fa0016d)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register I6 in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x1001)))

      "trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        binary.execute(prevState, MixWord(0x0fa0002e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        binary.execute(prevState, MixWord(0x0fa0006e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        binary.execute(prevState, MixWord(0x0fa000ae)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        binary.execute(prevState, MixWord(0x0fa000ee)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        binary.execute(prevState, MixWord(0x0fa0012e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        binary.execute(prevState, MixWord(0x0fa0016e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x1000)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        binary.execute(prevState, MixWord(0x0fa0002e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        binary.execute(prevState, MixWord(0x0fa0006e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        binary.execute(prevState, MixWord(0x0fa000ae)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        binary.execute(prevState, MixWord(0x0fa000ee)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        binary.execute(prevState, MixWord(0x0fa0012e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        binary.execute(prevState, MixWord(0x0fa0016e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x0)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        binary.execute(prevState, MixWord(0x0fa0002e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        binary.execute(prevState, MixWord(0x0fa0006e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        binary.execute(prevState, MixWord(0x0fa000ae)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        binary.execute(prevState, MixWord(0x0fa000ee)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        binary.execute(prevState, MixWord(0x0fa0012e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        binary.execute(prevState, MixWord(0x0fa0016e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedI(6, MixIndex(0x0001)))

      "not trigger conditional jump on negative I6" in {
        // A = 1000, I = 0, F = 0, C = 46 J6N
        binary.execute(prevState, MixWord(0x0fa0002e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero I6" in {
        // A = 1000, I = 0, F = 1, C = 46 J6Z
        binary.execute(prevState, MixWord(0x0fa0006e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive I6" in {
        // A = 1000, I = 0, F = 2, C = 46 J6P
        binary.execute(prevState, MixWord(0x0fa000ae)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative I6" in {
        // A = 1000, I = 0, F = 3, C = 46 J6NN
        binary.execute(prevState, MixWord(0x0fa000ee)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero I6" in {
        // A = 1000, I = 0, F = 4, C = 46 J6NZ
        binary.execute(prevState, MixWord(0x0fa0012e)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive I6" in {
        // A = 1000, I = 0, F = 5, C = 46 J6NP
        binary.execute(prevState, MixWord(0x0fa0016e)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }

  "register X in the binary mode" when {
    "contains a negative non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x40000001)))

      "trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        binary.execute(prevState, MixWord(0x0fa0002f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        binary.execute(prevState, MixWord(0x0fa0006f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        binary.execute(prevState, MixWord(0x0fa000af)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        binary.execute(prevState, MixWord(0x0fa000ef)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        binary.execute(prevState, MixWord(0x0fa0012f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        binary.execute(prevState, MixWord(0x0fa0016f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains negative zero" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x40000000)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        binary.execute(prevState, MixWord(0x0fa0002f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        binary.execute(prevState, MixWord(0x0fa0006f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        binary.execute(prevState, MixWord(0x0fa000af)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        binary.execute(prevState, MixWord(0x0fa000ef)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        binary.execute(prevState, MixWord(0x0fa0012f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        binary.execute(prevState, MixWord(0x0fa0016f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains positive zero" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x40000000)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        binary.execute(prevState, MixWord(0x0fa0002f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        binary.execute(prevState, MixWord(0x0fa0006f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        binary.execute(prevState, MixWord(0x0fa000af)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        binary.execute(prevState, MixWord(0x0fa000ef)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        binary.execute(prevState, MixWord(0x0fa0012f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        binary.execute(prevState, MixWord(0x0fa0016f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }
    }

    "contains a positive non-zero value" should {
      val prevState = state.copy(registers = state.registers.updatedX(MixWord(0x00000001)))

      "not trigger conditional jump on negative X" in {
        // A = 1000, I = 0, F = 0, C = 47 JXN
        binary.execute(prevState, MixWord(0x0fa0002f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "not trigger conditional jump on zero X" in {
        // A = 1000, I = 0, F = 1, C = 47 JXZ
        binary.execute(prevState, MixWord(0x0fa0006f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }

      "trigger conditional jump on positive X" in {
        // A = 1000, I = 0, F = 2, C = 47 JXP
        binary.execute(prevState, MixWord(0x0fa000af)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-negative X" in {
        // A = 1000, I = 0, F = 3, C = 47 JXNN
        binary.execute(prevState, MixWord(0x0fa000ef)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "trigger conditional jump on non-zero X" in {
        // A = 1000, I = 0, F = 4, C = 47 JXNZ
        binary.execute(prevState, MixWord(0x0fa0012f)).map(s => {
          s.programCounter mustEqual MixIndex(1000)
          s.registers.getJ mustEqual MixIndex(3001)
        })
      }

      "not trigger conditional jump on non-positive X" in {
        // A = 1000, I = 0, F = 5, C = 47 JXNP
        binary.execute(prevState, MixWord(0x0fa0016f)).map(s => {
          s.programCounter mustEqual MixIndex(3001)
          s.registers.getJ mustEqual MixIndex(0)
        })
      }
    }
  }
}
