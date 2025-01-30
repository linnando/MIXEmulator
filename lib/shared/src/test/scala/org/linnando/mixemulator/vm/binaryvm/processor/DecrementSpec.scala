package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class DecrementSpec extends AsyncWordSpec with Matchers {
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

  "binary decrement module" when {
    "decrementing register A" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 48 DECA
        binary.execute(state, MixWord(0x00040070)).map(s => {
          s.registers.getA mustEqual MixWord(0x3ffffe2f) // + 63 63 63 56 47
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 48 DECA
        binary.execute(state, MixWord(0x00045070)).map(s => {
          s.registers.getA mustEqual MixWord(0x3ffffd32) // + 63 63 63 52 50
          s.registers.getOV mustEqual false
        })
      }

      "decrement with an overflow" in {
        // A = -0x1d1, I = 0, F = 1, C = 48 DECA
        binary.execute(state, MixWord(0x47440070)).map(s => {
          s.registers.getA mustEqual MixWord(0x00000001) // + 0 0 0 0 1
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 48 DECA
        binary.execute(stateWithOV, MixWord(0x00040070)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        val stateWithSmallA = state.copy(
          registers = state.registers.updatedA(MixWord(0x00000e30))
        )
        // A = 0xe30, I = 0, F = 1, C = 48 DECA
        binary.execute(stateWithSmallA, MixWord(0x38c00070)) map {
          _.registers.getA mustEqual MixWord(0x00000000)
        }
      }
    }

    "decrementing register I1" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 49 DEC1
        binary.execute(state, MixWord(0x00040071)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(0x07c2) // + 31 2
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 49 DEC1
        binary.execute(state, MixWord(0x00045071)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(0x06c5) // + 27 5
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0x83d, I = 0, F = 1, C = 49 DEC1
          binary.execute(state, MixWord(0x60f40071))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 49 DEC1
        binary.execute(stateWithOV, MixWord(0x00040071)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 0x7c3, I = 0, F = 1, C = 49 DEC1
        binary.execute(state, MixWord(0x1f0c0071)) map {
          _.registers.getI(1) mustEqual MixIndex(0x0000)
        }
      }
    }

    "decrementing register I2" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 50 DEC2
        binary.execute(state, MixWord(0x00040072)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(0x08c7) // + 35 7
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 50 DEC2
        binary.execute(state, MixWord(0x00045072)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(0x07ca) // + 31 10
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0x738, I = 0, F = 1, C = 50 DEC2
          binary.execute(state, MixWord(0x5ce00072))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 50 DEC2
        binary.execute(stateWithOV, MixWord(0x00040072)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 0x8c8, I = 0, F = 1, C = 50 DEC2
        binary.execute(state, MixWord(0x23200072)) map {
          _.registers.getI(2) mustEqual MixIndex(0x0000)
        }
      }
    }

    "decrementing register I3" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 51 DEC3
        binary.execute(state, MixWord(0x00040073)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(0x0d28) // + 52 40
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 51 DEC3
        binary.execute(state, MixWord(0x00045073)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(0x0c2b) // + 48 43
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0x2d7, I = 0, F = 1, C = 51 DEC3
          binary.execute(state, MixWord(0x4b5c0073))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 51 DEC3
        binary.execute(stateWithOV, MixWord(0x00040073)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 0xd29, I = 0, F = 1, C = 51 DEC3
        binary.execute(state, MixWord(0x34a40073)) map {
          _.registers.getI(3) mustEqual MixIndex(0x0000)
        }
      }
    }

    "decrementing register I4" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 52 DEC4
        binary.execute(state, MixWord(0x00040074)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(0x05be) // + 22 62
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 52 DEC4
        binary.execute(state, MixWord(0x00045074)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(0x04c1) // + 19 1
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0xa41, I = 0, F = 1, C = 52 DEC4
          binary.execute(state, MixWord(0x79040074))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 52 DEC4
        binary.execute(stateWithOV, MixWord(0x00040074)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 0x5bf, I = 0, F = 1, C = 52 DEC4
        binary.execute(state, MixWord(0x16fc0074)) map {
          _.registers.getI(4) mustEqual MixIndex(0x0000)
        }
      }
    }

    "decrementing register I5" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 53 DEC5
        binary.execute(state, MixWord(0x00040075)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(0x00fc) // + 3 60
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 53 DEC5
        binary.execute(state, MixWord(0x00045075)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(0x1001) // - 0 1
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -0xf03, I = 0, F = 1, C = 53 DEC5
          binary.execute(state, MixWord(0x7c0c0075))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 53 DEC5
        binary.execute(stateWithOV, MixWord(0x00040075)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 0x0fd, I = 0, F = 1, C = 53 DEC5
        binary.execute(state, MixWord(0x03f40075)) map {
          _.registers.getI(5) mustEqual MixIndex(0x0000)
        }
      }
    }

    "decrementing register I6" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 54 DEC6
        binary.execute(state, MixWord(0x00040076)).map(s => {
          s.registers.getI(6) mustEqual MixIndex(0x1de4) // - 55 36
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 54 DEC6
        binary.execute(state, MixWord(0x00045076)).map(s => {
          s.registers.getI(6) mustEqual MixIndex(0x1ee1) // - 59 33
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 0x21d, I = 0, F = 1, C = 54 DEC6
          binary.execute(state, MixWord(0x08740076))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 54 DEC6
        binary.execute(stateWithOV, MixWord(0x00040076)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = -0xde3, I = 0, F = 1, C = 54 DEC6
        binary.execute(state, MixWord(0x778c0076)) map {
          _.registers.getI(6) mustEqual MixIndex(0x1000)
        }
      }
    }

    "decrementing register X" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 55 DECX
        binary.execute(state, MixWord(0x00040077)).map(s => {
          s.registers.getX mustEqual MixWord(0x7ffff51d) // - 63 63 63 20 29
          s.registers.getOV mustEqual false
        })
      }

      "decrement an indexed address" in {
        // A = 1, I = 5, F = 1, C = 55 DECX
        binary.execute(state, MixWord(0x00045077)).map(s => {
          s.registers.getX mustEqual MixWord(0x7ffff61a) // - 63 63 63 24 26
          s.registers.getOV mustEqual false
        })
      }

      "decrement with an overflow" in {
        // A = 0xae4, I = 0, F = 1, C = 55 DECX
        binary.execute(state, MixWord(0x2b900077)).map(s => {
          s.registers.getX mustEqual MixWord(0x40000000) // - 0 0 0 0 0
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 55 DECX
        binary.execute(stateWithOV, MixWord(0x00040077)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        val stateWithSmallX = state.copy(
          registers = state.registers.updatedX(MixWord(0x4000051c))
        )
        // A = -0x51c, I = 0, F = 1, C = 55 DECX
        binary.execute(stateWithSmallX, MixWord(0x54700077)) map {
          _.registers.getX mustEqual MixWord(0x40000000)
        }
      }
    }
  }
}
