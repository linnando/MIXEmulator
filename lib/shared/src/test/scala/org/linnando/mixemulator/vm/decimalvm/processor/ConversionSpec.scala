package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ConversionSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  import decimal._

  private val initialState = decimal.initialState

  "decimal conversion module" should {
    "convert characters to numeric" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L | 313239L)) // - 0 0 31 32 39
          .updatedX(MixWord(3757473030L)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      decimal.execute(state, MixWord(5L)).map(s => {
        s.registers.getA mustEqual MixWord(0x400000000L | 12977700L) // - 0 12 97 77 0
        s.registers.getX mustEqual MixWord(3757473030L) // + 37 57 47 30 30
        s.registers.getOV mustEqual false
      })
    }

    "convert number to characters" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L | 12977699L)) // - 0 12 97 76 99
          .updatedX(MixWord(3757473030L)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 1, C = 5 CHAR
      decimal.execute(state, MixWord(105L)).map(s => {
        s.registers.getA mustEqual MixWord(0x400000000L | 3030313239L) // - 30 30 31 32 39
        s.registers.getX mustEqual MixWord(3737363939L) // + 37 37 36 39 39
        s.registers.getOV mustEqual false
      })
    }
  }
}
