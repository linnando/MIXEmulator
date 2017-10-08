package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.mutable.Specification

class IncrementSpec extends Specification {
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

  "decimal increment module" should {
    "increment registers by a fixed number" in {
      // A = 1, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(1000048L))
      nextStateA.registers.getA must be equalTo MixWord(9999995649L) // + 99 99 99 56 49
      nextStateA.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(1000049L))
      nextState1.registers.getI(1) must be equalTo MixIndex(3104) // + 31 4
      nextState1.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(1000050L))
      nextState2.registers.getI(2) must be equalTo MixIndex(3509) // + 35 9
      nextState2.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(1000051L))
      nextState3.registers.getI(3) must be equalTo MixIndex(5242) // + 52 42
      nextState3.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(1000052L))
      nextState4.registers.getI(4) must be equalTo MixIndex(2300) // + 23 0
      nextState4.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(1000053L))
      nextState5.registers.getI(5) must be equalTo MixIndex(362) // + 3 62
      nextState5.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(1000054L))
      nextState6.registers.getI(6) must be equalTo MixIndex((0x4000 | 5534).toShort) // - 55 34
      nextState6.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(1000055L))
      nextStateX.registers.getX must be equalTo MixWord(0x400000000L | 9999992027L) // - 99 99 99 20 27
      nextStateX.registers.getOV must beFalse
    }

    "increment registers by an indexed address" in {
      // A = 1, I = 5, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(1050048L))
      nextStateA.registers.getA must be equalTo MixWord(9999996010L) // + 99 99 99 60 10
      nextStateA.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(1050049L))
      nextState1.registers.getI(1) must be equalTo MixIndex(3465) // + 34 65
      nextState1.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(1050050L))
      nextState2.registers.getI(2) must be equalTo MixIndex(3870) // + 38 70
      nextState2.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(1050051L))
      nextState3.registers.getI(3) must be equalTo MixIndex(5603) // + 56 3
      nextState3.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(1050052L))
      nextState4.registers.getI(4) must be equalTo MixIndex(2661) // + 26 61
      nextState4.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(1050053L))
      nextState5.registers.getI(5) must be equalTo MixIndex(723) // + 7 23
      nextState5.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(1050054L))
      nextState6.registers.getI(6) must be equalTo MixIndex((0x4000 | 5173).toShort) // - 51 73
      nextState6.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(1050055L))
      nextStateX.registers.getX must be equalTo MixWord(0x400000000L | 9999991666L) // - 99 99 99 16 66
      nextStateX.registers.getOV must beFalse
    }

    "increment registers A and X with an overflow" in {
      // A = 4353, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(4353000048L))
      nextStateA.registers.getA must be equalTo MixWord(1L) // + 0 0 0 0 1
      nextStateA.registers.getOV must beTrue
      // A = -7972, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(0x400000000L | 7972000055L))
      nextStateX.registers.getX must be equalTo MixWord(0x400000000L) // - 0 0 0 0 0
      nextStateX.registers.getOV must beTrue
    }

    "throw an exception for index registers overflow" in {
      // A = 6897, I = 0, F = 0, C = 49 INC1
      execute(state, MixWord(6897000049L)) must throwAn[OverflowException]
      // A = 6492, I = 0, F = 0, C = 50 INC2
      execute(state, MixWord(6492000050L)) must throwAn[OverflowException]
      // A = 4759, I = 0, F = 0, C = 51 INC3
      execute(state, MixWord(4759000051L)) must throwAn[OverflowException]
      // A = 7701, I = 0, F = 0, C = 52 INC4
      execute(state, MixWord(7701000052L)) must throwAn[OverflowException]
      // A = 9639, I = 0, F = 0, C = 53 INC5
      execute(state, MixWord(9639000053L)) must throwAn[OverflowException]
      // A = -4465, I = 0, F = 0, C = 54 INC6
      execute(state, MixWord(0x400000000L | 4465000054L)) must throwAn[OverflowException]
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 0, C = 48 INCA
      execute(stateWithOV, MixWord(1000048L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 49 INC1
      execute(stateWithOV, MixWord(1000049L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 50 INC2
      execute(stateWithOV, MixWord(1000050L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 51 INC3
      execute(stateWithOV, MixWord(1000051L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 52 INC4
      execute(stateWithOV, MixWord(1000052L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 53 INC5
      execute(stateWithOV, MixWord(1000053L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 54 INC6
      execute(stateWithOV, MixWord(1000054L)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 55 INCX
      execute(stateWithOV, MixWord(1000055L)).registers.getOV must beTrue
    }

    "increment a register by its negation preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(MixWord(3632L)).updatedX(MixWord(0x400000000L | 1308L))
      )
      // A = -3632, I = 0, F = 0, C = 48 INCA
      execute(stateWithSmallAX, MixWord(0x400000000L | 3632000048L)).registers.getA must be equalTo MixWord(0L)
      // A = -3103, I = 0, F = 0, C = 49 INC1
      execute(stateWithSmallAX, MixWord(0x400000000L | 3103000049L)).registers.getI(1) must be equalTo MixIndex(0)
      // A = -3508, I = 0, F = 0, C = 50 INC2
      execute(stateWithSmallAX, MixWord(0x400000000L | 3508000050L)).registers.getI(2) must be equalTo MixIndex(0)
      // A = -5241, I = 0, F = 0, C = 51 INC3
      execute(stateWithSmallAX, MixWord(0x400000000L | 5241000051L)).registers.getI(3) must be equalTo MixIndex(0)
      // A = -2299, I = 0, F = 0, C = 52 INC4
      execute(stateWithSmallAX, MixWord(0x400000000L | 2299000052L)).registers.getI(4) must be equalTo MixIndex(0)
      // A = -361, I = 0, F = 0, C = 53 INC5
      execute(stateWithSmallAX, MixWord(0x400000000L | 361000053L)).registers.getI(5) must be equalTo MixIndex(0)
      // A = 5535, I = 0, F = 0, C = 54 INC6
      execute(stateWithSmallAX, MixWord(5535000054L)).registers.getI(6) must be equalTo MixIndex(0x4000)
      // A = 1308, I = 0, F = 0, C = 55 INCX
      execute(stateWithSmallAX, MixWord(1308000055L)).registers.getX must be equalTo MixWord(0x400000000L)
    }
  }
}
