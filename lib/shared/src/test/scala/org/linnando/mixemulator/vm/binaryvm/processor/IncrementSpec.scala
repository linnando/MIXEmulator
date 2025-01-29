package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class IncrementSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import binary._

  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x3ffffe30)) // + 63 63 63 56 48
      .updatedX(MixWord(0x7ffff51c)) // - 63 63 63 20 28
      .updatedI(1, MixIndex(0x07c3)) // + 31 3
      .updatedI(2, MixIndex(0x08c8)) // + 35 8
      .updatedI(3, MixIndex(0x0d29)) // + 52 41
      .updatedI(4, MixIndex(0x05bf)) // + 22 63
      .updatedI(5, MixIndex(0x00fd)) // + 3 61
      .updatedI(6, MixIndex(0x1de3)) // - 55 35
  )

  "binary increment module" when {
    "incrementing register A" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 48 INCA
        execute(state, MixWord(0x00040030)).map(s => {
          s.registers.getA mustEqual MixWord(0x3ffffe31) // + 63 63 63 56 49
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 48 INCA
        execute(state, MixWord(0x00045030)).map(s => {
          s.registers.getA mustEqual MixWord(0x3fffff2e) // + 63 63 63 60 46
          s.registers.getOV mustEqual false
        })
      }

      "increment with an overflow" in {
        // A = 0x1d1, I = 0, F = 0, C = 48 INCA
        execute(state, MixWord(0x07440030)).map(s => {
          s.registers.getA mustEqual MixWord(0x00000001) // + 0 0 0 0 1
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 48 INCA
        execute(stateWithOV, MixWord(0x00040030)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        val stateWithSmallA = state.copy(
          registers = state.registers.updatedA(MixWord(0x00000e30))
        )
        // A = -0xe30, I = 0, F = 0, C = 48 INCA
        execute(stateWithSmallA, MixWord(0x78c00030)) map {
          _.registers.getA mustEqual MixWord(0x00000000)
        }
      }
    }

    "incrementing register I1" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 49 INC1
        execute(state, MixWord(0x00040031)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(0x07c4) // + 31 4
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 49 INC1
        execute(state, MixWord(0x00045031)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(0x08c1) // + 35 1
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 0x83d, I = 0, F = 0, C = 49 INC1
          execute(state, MixWord(0x20f40031))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 49 INC1
        execute(stateWithOV, MixWord(0x00040031)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -0x7c3, I = 0, F = 0, C = 49 INC1
        execute(state, MixWord(0x5f0c0031)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0000)
        }
      }

      "incrementing register I2" should {
        "increment by a fixed number" in {
          // A = 1, I = 0, F = 0, C = 50 INC2
          execute(state, MixWord(0x00040032)).map(s => {
            s.registers.getI(2) mustEqual MixIndex(0x08c9) // + 35 9
            s.registers.getOV mustEqual false
          })
        }

        "increment by an indexed address" in {
          // A = 1, I = 5, F = 0, C = 50 INC2
          execute(state, MixWord(0x00045032)).map(s => {
            s.registers.getI(2) mustEqual MixIndex(0x09c6) // + 39 6
            s.registers.getOV mustEqual false
          })
        }

        "throw an exception on overflow" in {
          recoverToSucceededIf[OverflowException] {
            // A = 0x738, I = 0, F = 0, C = 50 INC2
            execute(state, MixWord(0x1ce00032))
          }
        }

        "not change the overflow flag if no overflow occurs" in {
          val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
          // A = 1, I = 0, F = 0, C = 50 INC2
          execute(stateWithOV, MixWord(0x00040032)) map {
            _.registers.getOV mustEqual true
          }
        }

        "increment by the current value's negation preserving the current sign" in {
          // A = -0x8c8, I = 0, F = 0, C = 50 INC2
          execute(state, MixWord(0x63200032)) map {
            _.registers.getI(2) mustEqual MixIndex(0x0000)
          }
        }
      }
    }

    "incrementing register I3" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 51 INC3
        execute(state, MixWord(0x00040033)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(0x0d2a) // + 52 42
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 51 INC3
        execute(state, MixWord(0x00045033)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(0x0e27) // + 56 39
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 0x2d7, I = 0, F = 0, C = 51 INC3
          execute(state, MixWord(0x0b5c0033))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 51 INC3
        execute(stateWithOV, MixWord(0x00040033)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -0xd29, I = 0, F = 0, C = 51 INC3
        execute(state, MixWord(0x74a40033)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0000)
        }
      }
    }

    "incrementing register I4" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 52 INC4
        execute(state, MixWord(0x00040034)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(0x05c0) // + 23 0
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 52 INC4
        execute(state, MixWord(0x00045034)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(0x06bd) // + 26 61
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 0xa41, I = 0, F = 0, C = 52 INC4
          execute(state, MixWord(0x39040034))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 52 INC4
        execute(stateWithOV, MixWord(0x00040034)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -0x5bf, I = 0, F = 0, C = 52 INC4
        execute(state, MixWord(0x56fc0034)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0000)
        }
      }
    }

    "incrementing register I5" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 53 INC5
        execute(state, MixWord(0x00040035)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(0x00fe) // + 3 62
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 53 INC5
        execute(state, MixWord(0x00045035)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(0x01fb) // + 7 59
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 0xf03, I = 0, F = 0, C = 53 INC5
          execute(state, MixWord(0x3c0c0035))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 53 INC5
        execute(stateWithOV, MixWord(0x00040035)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -0x0fd, I = 0, F = 0, C = 53 INC5
        execute(state, MixWord(0x43f40035)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0000)
        }
      }
    }

    "incrementing register I6" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 54 INC6
        execute(state, MixWord(0x00040036)).map(s => {
          s.registers.getI(6) mustEqual MixIndex(0x1de2) // - 55 34
          s.registers.getOV mustEqual false
        })
      }

      "increment registers by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 54 INC6
        execute(state, MixWord(0x00045036)).map(s => {
          s.registers.getI(6) mustEqual MixIndex(0x1ce5) // - 51 37
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0x21d, I = 0, F = 0, C = 54 INC6
          execute(state, MixWord(0x48740036))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 54 INC6
        execute(stateWithOV, MixWord(0x00040036)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = 0xde3, I = 0, F = 0, C = 54 INC6
        execute(state, MixWord(0x378c0036)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }
    }

    "incrementing register X" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 55 INCX
        execute(state, MixWord(0x00040037)).map(s => {
          s.registers.getX mustEqual MixWord(0x7ffff51b) // - 63 63 63 20 27
          s.registers.getOV mustEqual false
        })
      }

      "increment registers by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 55 INCX
        execute(state, MixWord(0x00045037)).map(s => {
          s.registers.getX mustEqual MixWord(0x7ffff41e) // - 63 63 63 16 30
          s.registers.getOV mustEqual false
        })
      }

      "increment with an overflow" in {
        // A = -0xae4, I = 0, F = 0, C = 55 INCX
        execute(state, MixWord(0x6b900037)).map(s => {
          s.registers.getX mustEqual MixWord(0x40000000) // - 0 0 0 0 0
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 55 INCX
        execute(stateWithOV, MixWord(0x00040037)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        val stateWithSmallX = state.copy(
          registers = state.registers.updatedX(MixWord(0x4000051c))
        )
        // A = 0x51c, I = 0, F = 0, C = 55 INCX
        execute(stateWithSmallX, MixWord(0x14700037)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }
    }
  }
}
