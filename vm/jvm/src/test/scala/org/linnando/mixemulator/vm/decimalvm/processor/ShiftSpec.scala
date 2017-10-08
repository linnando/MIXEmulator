package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.specs2.mutable.Specification

class ShiftSpec extends Specification {
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
      val nextState = execute(state, MixWord(2000006L))
      nextState.registers.getA must be equalTo MixWord(304050000L) // + 3 4 5 0 0
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 607080910L) // - 6 7 8 9 10
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState = execute(state, MixWord(2000106L))
      nextState.registers.getA must be equalTo MixWord(10203L) // + 0 0 1 2 3
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 607080910L) // - 6 7 8 9 10
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      val nextState = execute(state, MixWord(1000206L))
      nextState.registers.getA must be equalTo MixWord(203040506L) // + 2 3 4 5 6
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 708091000L) // - 7 8 9 10 0
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState = execute(state, MixWord(1000306L))
      nextState.registers.getA must be equalTo MixWord(1020304L) // + 0 1 2 3 4
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState = execute(state, MixWord(1000406L))
      nextState.registers.getA must be equalTo MixWord(203040506L) // + 2 3 4 5 6
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 708091001L) // - 7 8 9 10 1
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      val nextState = execute(state, MixWord(1000506L))
      nextState.registers.getA must be equalTo MixWord(1001020304L) // + 10 1 2 3 4
      nextState.registers.getX must be equalTo MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
    }
  }

  "shift scenario" should {
    "perform consecutive shifts" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState0 = execute(state, MixWord(1000306L))
      nextState0.registers.getA must be equalTo MixWord(1020304L) // + 0 1 2 3 4
      nextState0.registers.getX must be equalTo MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState1 = execute(nextState0, MixWord(2000006L))
      nextState1.registers.getA must be equalTo MixWord(203040000L) // + 2 3 4 0 0
      nextState1.registers.getX must be equalTo MixWord(0x400000000L | 506070809L) // - 5 6 7 8 9
      // A = 4, I = 0, F = 5, C = 6 SRC
      val nextState2 = execute(nextState1, MixWord(4000506L))
      nextState2.registers.getA must be equalTo MixWord(607080902L) // + 6 7 8 9 2
      nextState2.registers.getX must be equalTo MixWord(0x400000000L | 304000005L) // - 3 4 0 0 5
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState3 = execute(nextState2, MixWord(2000106L))
      nextState3.registers.getA must be equalTo MixWord(60708L) // + 0 0 6 7 8
      nextState3.registers.getX must be equalTo MixWord(0x400000000L | 304000005L) // - 3 4 0 0 5
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState4 = execute(nextState3, MixWord(1000406L))
      nextState4.registers.getA must be equalTo MixWord(6070803L) // + 0 6 7 8 3
      nextState4.registers.getX must be equalTo MixWord(0x400000000L | 400000500L) // - 4 0 0 5 0
    }
  }
}
