package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ShiftSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global
  import decimal._
  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(102030405L)) // + 1 2 3 4 5
      .updatedX(MixWord(0x400000000L | 607080910L)) // - 6 7 8 9 10
  )

  "decimal shift module" should {
    "shift A to the left" in {
      // A = 2, I = 0, F = 0, C = 6 SLA
      execute(state, MixWord(2000006L)).map(s=>{
      s.registers.getA mustEqual MixWord(304050000L) // + 3 4 5 0 0
      s.registers.getX mustEqual MixWord(0x400000000L | 607080910L) // - 6 7 8 9 10
      })
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      execute(state, MixWord(2000106L)).map(s=>{
      s.registers.getA mustEqual MixWord(10203L) // + 0 0 1 2 3
      s.registers.getX mustEqual MixWord(0x400000000L | 607080910L) // - 6 7 8 9 10
      })
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      execute(state, MixWord(1000206L)).map(s=>{
      s.registers.getA mustEqual MixWord(203040506L) // + 2 3 4 5 6
      s.registers.getX mustEqual MixWord(0x400000000L | 708091000L) // - 7 8 9 10 0
      })
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      execute(state, MixWord(1000306L)).map(s=>{
      s.registers.getA mustEqual MixWord(1020304L) // + 0 1 2 3 4
      s.registers.getX mustEqual MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
      })
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      execute(state, MixWord(1000406L)).map(s=>{
      s.registers.getA mustEqual MixWord(203040506L) // + 2 3 4 5 6
      s.registers.getX mustEqual MixWord(0x400000000L | 708091001L) // - 7 8 9 10 1
      })
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      execute(state, MixWord(1000506L)).map(s=>{
      s.registers.getA mustEqual MixWord(1001020304L) // + 10 1 2 3 4
      s.registers.getX mustEqual MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
      })
    }
  }
}
