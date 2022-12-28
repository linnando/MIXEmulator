package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ShiftSpec(implicit ee: ExecutionEnv) extends Specification {
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
      nextState.map(_.registers.getA) must beEqualTo(MixWord(304050000L)).await // + 3 4 5 0 0
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 607080910L)).await // - 6 7 8 9 10
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState = execute(state, MixWord(2000106L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(10203L)).await // + 0 0 1 2 3
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 607080910L)).await // - 6 7 8 9 10
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      val nextState = execute(state, MixWord(1000206L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(203040506L)).await // + 2 3 4 5 6
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 708091000L)).await // - 7 8 9 10 0
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState = execute(state, MixWord(1000306L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1020304L)).await // + 0 1 2 3 4
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 506070809L)).await // - 5 6 7 8 9
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState = execute(state, MixWord(1000406L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(203040506L)).await // + 2 3 4 5 6
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 708091001L)).await // - 7 8 9 10 1
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      val nextState = execute(state, MixWord(1000506L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1001020304L)).await // + 10 1 2 3 4
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 506070809L)).await // - 5 6 7 8 9
    }
  }

  "shift scenario" should {
    "perform consecutive shifts" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState0 = execute(state, MixWord(1000306L))
      nextState0.map(_.registers.getA) must beEqualTo(MixWord(1020304L)).await // + 0 1 2 3 4
      nextState0.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 506070809L)).await // - 5 6 7 8 9
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState1 = nextState0.flatMap(execute(_, MixWord(2000006L)))
      nextState1.map(_.registers.getA) must beEqualTo(MixWord(203040000L)).await // + 2 3 4 0 0
      nextState1.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 506070809L)).await // - 5 6 7 8 9
      // A = 4, I = 0, F = 5, C = 6 SRC
      val nextState2 = nextState1.flatMap(execute(_, MixWord(4000506L)))
      nextState2.map(_.registers.getA) must beEqualTo(MixWord(607080902L)).await // + 6 7 8 9 2
      nextState2.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 304000005L)).await // - 3 4 0 0 5
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState3 = nextState2.flatMap(execute(_, MixWord(2000106L)))
      nextState3.map(_.registers.getA) must beEqualTo(MixWord(60708L)).await // + 0 0 6 7 8
      nextState3.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 304000005L)).await // - 3 4 0 0 5
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState4 = nextState3.flatMap(execute(_, MixWord(1000406L)))
      nextState4.map(_.registers.getA) must beEqualTo(MixWord(6070803L)).await // + 0 6 7 8 3
      nextState4.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 400000500L)).await // - 4 0 0 5 0
    }
  }
}
