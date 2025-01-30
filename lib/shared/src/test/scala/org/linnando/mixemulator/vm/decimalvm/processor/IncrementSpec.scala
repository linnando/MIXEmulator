package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class IncrementSpec extends AsyncWordSpec with Matchers {
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

  "decimal increment module" when {
    "incrementing register A" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 48 INCA
        decimal.execute(state, MixWord(1000048L)).map(s => {
          s.registers.getA mustEqual MixWord(9999995649L) // + 99 99 99 56 49
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 48 INCA
        decimal.execute(state, MixWord(1050048L)).map(s => {
          s.registers.getA mustEqual MixWord(9999996010L) // + 99 99 99 60 10
          s.registers.getOV mustEqual false
        })
      }

      "increment with an overflow" in {
        // A = 4353, I = 0, F = 0, C = 48 INCA
        decimal.execute(state, MixWord(4353000048L)).map(s => {
          s.registers.getA mustEqual MixWord(1L) // + 0 0 0 0 1
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 48 INCA
        decimal.execute(stateWithOV, MixWord(1000048L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        val stateWithSmallA = state.copy(
          registers = state.registers.updatedA(MixWord(3632L))
        )
        // A = -3632, I = 0, F = 0, C = 48 INCA
        decimal.execute(stateWithSmallA, MixWord(0x400000000L | 3632000048L)) map {
          _.registers.getA mustEqual MixWord(0L)
        }
      }
    }

    "incrementing register I1" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 49 INC1
        decimal.execute(state, MixWord(1000049L)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(3104) // + 31 4
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 49 INC1
        decimal.execute(state, MixWord(1050049L)).map(s => {
          s.registers.getI(1) mustEqual MixIndex(3465) // + 34 65
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 6897, I = 0, F = 0, C = 49 INC1
          decimal.execute(state, MixWord(6897000049L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 49 INC1
        decimal.execute(stateWithOV, MixWord(1000049L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -3103, I = 0, F = 0, C = 49 INC1
        decimal.execute(state, MixWord(0x400000000L | 3103000049L)) map {
          _.registers.getI(1) mustEqual MixIndex(0)
        }
      }
    }

    "incrementing register I2" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 50 INC2
        decimal.execute(state, MixWord(1000050L)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(3509) // + 35 9
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 50 INC2
        decimal.execute(state, MixWord(1050050L)).map(s => {
          s.registers.getI(2) mustEqual MixIndex(3870) // + 38 70
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 6492, I = 0, F = 0, C = 50 INC2
          decimal.execute(state, MixWord(6492000050L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 50 INC2
        decimal.execute(stateWithOV, MixWord(1000050L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -3508, I = 0, F = 0, C = 50 INC2
        decimal.execute(state, MixWord(0x400000000L | 3508000050L)) map {
          _.registers.getI(2) mustEqual MixIndex(0)
        }
      }
    }

    "incrementing register I3" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 51 INC3
        decimal.execute(state, MixWord(1000051L)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(5242) // + 52 42
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 51 INC3
        decimal.execute(state, MixWord(1050051L)).map(s => {
          s.registers.getI(3) mustEqual MixIndex(5603) // + 56 3
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 4759, I = 0, F = 0, C = 51 INC3
          decimal.execute(state, MixWord(4759000051L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 51 INC3
        decimal.execute(stateWithOV, MixWord(1000051L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -5241, I = 0, F = 0, C = 51 INC3
        decimal.execute(state, MixWord(0x400000000L | 5241000051L)) map {
          _.registers.getI(3) mustEqual MixIndex(0)
        }
      }
    }

    "incrementing register I4" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 52 INC4
        decimal.execute(state, MixWord(1000052L)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(2300) // + 23 0
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 52 INC4
        decimal.execute(state, MixWord(1050052L)).map(s => {
          s.registers.getI(4) mustEqual MixIndex(2661) // + 26 61
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 7701, I = 0, F = 0, C = 52 INC4
          decimal.execute(state, MixWord(7701000052L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 52 INC4
        decimal.execute(stateWithOV, MixWord(1000052L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -2299, I = 0, F = 0, C = 52 INC4
        decimal.execute(state, MixWord(0x400000000L | 2299000052L)) map {
          _.registers.getI(4) mustEqual MixIndex(0)
        }
      }
    }

    "incrementing register I5" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 53 INC5
        decimal.execute(state, MixWord(1000053L)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(362) // + 3 62
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 53 INC5
        decimal.execute(state, MixWord(1050053L)).map(s => {
          s.registers.getI(5) mustEqual MixIndex(723) // + 7 23
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = 9639, I = 0, F = 0, C = 53 INC5
          decimal.execute(state, MixWord(9639000053L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 53 INC5
        decimal.execute(stateWithOV, MixWord(1000053L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = -361, I = 0, F = 0, C = 53 INC5
        decimal.execute(state, MixWord(0x400000000L | 361000053L)) map {
          _.registers.getI(5) mustEqual MixIndex(0)
        }
      }
    }

    "incrementing register I6" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 54 INC6
        decimal.execute(state, MixWord(1000054L)).map(s => {
          s.registers.getI(6) mustEqual MixIndex((0x4000 | 5534).toShort) // - 55 34
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 54 INC6
        decimal.execute(state, MixWord(1050054L)).map(s => {
          s.registers.getI(6) mustEqual MixIndex((0x4000 | 5173).toShort) // - 51 73
          s.registers.getOV mustEqual false
        })
      }

      "throw an exception on overflow" in {
        recoverToSucceededIf[OverflowException] {
          // A = -4465, I = 0, F = 0, C = 54 INC6
          decimal.execute(state, MixWord(0x400000000L | 4465000054L))
        }
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 54 INC6
        decimal.execute(stateWithOV, MixWord(1000054L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        // A = 5535, I = 0, F = 0, C = 54 INC6
        decimal.execute(state, MixWord(5535000054L)) map {
          _.registers.getI(6) mustEqual MixIndex(0x4000)
        }
      }
    }

    "incrementing register X" should {
      "increment by a fixed number" in {
        // A = 1, I = 0, F = 0, C = 55 INCX
        decimal.execute(state, MixWord(1000055L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L | 9999992027L) // - 99 99 99 20 27
          s.registers.getOV mustEqual false
        })
      }

      "increment by an indexed address" in {
        // A = 1, I = 5, F = 0, C = 55 INCX
        decimal.execute(state, MixWord(1050055L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L | 9999991666L) // - 99 99 99 16 66
          s.registers.getOV mustEqual false
        })
      }

      "increment with an overflow" in {
        // A = -7972, I = 0, F = 0, C = 55 INCX
        decimal.execute(state, MixWord(0x400000000L | 7972000055L)).map(s => {
          s.registers.getX mustEqual MixWord(0x400000000L) // - 0 0 0 0 0
          s.registers.getOV mustEqual true
        })
      }

      "not change the overflow flag if no overflow occurs" in {
        val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
        // A = 1, I = 0, F = 0, C = 55 INCX
        decimal.execute(stateWithOV, MixWord(1000055L)) map {
          _.registers.getOV mustEqual true
        }
      }

      "increment by the current value's negation preserving the current sign" in {
        val stateWithSmallX = state.copy(
          registers = state.registers.updatedX(MixWord(0x400000000L | 1308L))
        )
        // A = 1308, I = 0, F = 0, C = 55 INCX
        decimal.execute(stateWithSmallX, MixWord(1308000055L)) map {
          _.registers.getX mustEqual MixWord(0x400000000L)
        }
      }
    }
  }
}
