package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class DecrementSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(9999995648L)) // + 99 99 99 56 48
      .updatedX(MixWord(0x400000000L | 9999992028L)) // - 99 99 99 20 28
      .updatedI(1, MixIndex(3103)) // + 31 3
      .updatedI(2, MixIndex(3508)) // + 35 8
      .updatedI(3, MixIndex(5241)) // + 52 41
      .updatedI(4, MixIndex(2299)) // + 22 99
      .updatedI(5, MixIndex(361)) // + 3 61
      .updatedI(6, MixIndex((0x4000 | 5535).toShort)) // - 55 35
  )

  "decimal decrement module" when {
    "decrementing register A" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 48 DECA
        decimal.execute(state, MixWord(1000148L)).map(s => {
          s.registers.getA mustEqual MixWord(9999995647L) // + 99 99 99 56 47
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 48 DECA
        decimal.execute(state, MixWord(1050148L)).map(s => {
          s.registers.getA mustEqual MixWord(9999995286L) // + 99 99 99 52 86
          s.registers.getOV mustEqual false
        })
      }

      "decrement with an overflow" in {
        // A = -4353, I = 0, F = 1, C = 48 DECA
        decimal.execute(state, MixWord(0x400000000L | 4353000148L)).map(s => {
          s.registers.getA mustEqual MixWord(1L) // + 0 0 0 0 1
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 48 DECA
        decimal.execute(stateWithOV, MixWord(1000148L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        val stateWithSmallA = state.copy(
          registers = state.registers.updatedA(MixWord(3632L))
        )
        // A = 3632, I = 0, F = 1, C = 48 DECA
        decimal.execute(stateWithSmallA, MixWord(3632000148L)) map {
          _.registers.getA mustEqual MixWord(0L)
        }
      }
    }

    "decrementing register I1" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 49 DEC1
        decimal.execute(state, MixWord(1000149L)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(3102) // + 31 2
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 49 DEC1
        decimal.execute(state, MixWord(1050149L)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(2741) // + 27 41
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -6897, I = 0, F = 1, C = 49 DEC1
          decimal.execute(state, MixWord(0x400000000L | 6897000149L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 49 DEC1
        decimal.execute(stateWithOV, MixWord(1000149L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 3103, I = 0, F = 1, C = 49 DEC1
        decimal.execute(state, MixWord(3103000149L)) map {
          _.registers.getI(1) mustEqual MixIndex(0)
        }
      }
    }

    "decrementing register I2" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 50 DEC2
        decimal.execute(state, MixWord(1000150L)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(3507) // + 35 7
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 50 DEC2
        decimal.execute(state, MixWord(1050150L)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(3146) // + 31 46
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -6492, I = 0, F = 1, C = 50 DEC2
          decimal.execute(state, MixWord(0x400000000L | 6492000150L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 50 DEC2
        decimal.execute(stateWithOV, MixWord(1000150L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 3508, I = 0, F = 1, C = 50 DEC2
        decimal.execute(state, MixWord(3508000150L)) map {
          _.registers.getI(2) mustEqual MixIndex(0)
        }
      }
    }

    "decrementing register I3" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 51 DEC3
        decimal.execute(state, MixWord(1000151L)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(5240) // + 52 40
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 51 DEC3
        decimal.execute(state, MixWord(1050151L)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(4879) // + 48 79
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -4759, I = 0, F = 1, C = 51 DEC3
          decimal.execute(state, MixWord(0x400000000L | 4759000151L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 51 DEC3
        decimal.execute(stateWithOV, MixWord(1000151L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 5241, I = 0, F = 1, C = 51 DEC3
        decimal.execute(state, MixWord(5241000151L)) map {
          _.registers.getI(3) mustEqual MixIndex(0)
        }
      }
    }

    "decrementing register I4" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 52 DEC4
        decimal.execute(state, MixWord(1000152L)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(2298) // + 22 98
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 52 DEC4
        decimal.execute(state, MixWord(1050152L)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(1937) // + 19 37
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -7701, I = 0, F = 1, C = 52 DEC4
          decimal.execute(state, MixWord(0x400000000L | 7701000152L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 52 DEC4
        decimal.execute(stateWithOV, MixWord(1000152L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 2299, I = 0, F = 1, C = 52 DEC4
        decimal.execute(state, MixWord(2299000152L)) map {
          _.registers.getI(4) mustEqual MixIndex(0)
        }
      }
    }

    "decrementing register I5" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 53 DEC5
        decimal.execute(state, MixWord(1000153L)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(360) // + 3 60
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 53 DEC5
        decimal.execute(state, MixWord(1050153L)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(0x4001) // - 0 1
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -9639, I = 0, F = 1, C = 53 DEC5
          decimal.execute(state, MixWord(0x400000000L | 9639000153L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 53 DEC5
        decimal.execute(stateWithOV, MixWord(1000153L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = 361, I = 0, F = 1, C = 53 DEC5
        decimal.execute(state, MixWord(361000153L)) map {
          _.registers.getI(5) mustEqual MixIndex(0)
        }
      }
    }

    "decrementing register I6" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 54 DEC6
        decimal.execute(state, MixWord(1000154L)).map(s => {
          s.registers.getI(6) mustEqual MixIndex((0x4000 | 5536).toShort) // - 55 36
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 54 DEC6
        decimal.execute(state, MixWord(1050154L)).map(s => {
          s.registers.getI(6) mustEqual MixIndex((0x4000 | 5897).toShort) // - 58 97
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 4465, I = 0, F = 1, C = 54 DEC6
          decimal.execute(state, MixWord(4465000154L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 54 DEC6
        decimal.execute(stateWithOV, MixWord(1000154L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        // A = -5535, I = 0, F = 1, C = 54 DEC6
        decimal.execute(state, MixWord(0x400000000L | 5535000154L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }
    }

    "decrementing register X" should {
      "decrement by a fixed number" in {
        // A = 1, I = 0, F = 1, C = 55 DECX
        decimal.execute(state, MixWord(1000155L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L | 9999992029L) // - 99 99 99 20 29
          s.registers.getOV mustEqual false
        })
      }

      "decrement by an indexed address" in {
        // A = 1, I = 5, F = 1, C = 55 DECX
        decimal.execute(state, MixWord(1050155L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L | 9999992390L) // - 99 99 99 23 90
          s.registers.getOV mustEqual false
        })
      }

      "decrement with an overflow" in {
        // A = 7972, I = 0, F = 1, C = 55 DECX
        decimal.execute(state, MixWord(7972000155L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L) // - 0 0 0 0 0
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 1, C = 55 DECX
        decimal.execute(stateWithOV, MixWord(1000155L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "decrement by the current value preserving the sign" in {
        val stateWithSmallX = state.copy(
          registers = state.registers.updatedX(MixWord(0x400000000L | 1308L))
        )
        // A = -1308, I = 0, F = 1, C = 55 DECX
        decimal.execute(stateWithSmallX, MixWord(0x400000000L | 1308000155L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }
    }
  }
}
