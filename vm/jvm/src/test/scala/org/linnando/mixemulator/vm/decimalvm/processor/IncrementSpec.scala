package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class IncrementSpec(implicit ee: ExecutionEnv) extends Specification {

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
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(9999995649L)).await // + 99 99 99 56 49
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(1000049L))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(3104)).await // + 31 4
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(1000050L))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(3509)).await // + 35 9
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(1000051L))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(5242)).await // + 52 42
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(1000052L))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(2300)).await // + 23 0
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(1000053L))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(362)).await // + 3 62
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(1000054L))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 5534).toShort)).await // - 55 34
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(1000055L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 9999992027L)).await // - 99 99 99 20 27
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "increment registers by an indexed address" in {
      // A = 1, I = 5, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(1050048L))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(9999996010L)).await // + 99 99 99 60 10
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(1050049L))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(3465)).await // + 34 65
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(1050050L))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(3870)).await // + 38 70
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(1050051L))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(5603)).await // + 56 3
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(1050052L))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(2661)).await // + 26 61
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(1050053L))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(723)).await // + 7 23
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(1050054L))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 5173).toShort)).await // - 51 73
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(1050055L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 9999991666L)).await // - 99 99 99 16 66
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "increment registers A and X with an overflow" in {
      // A = 4353, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(4353000048L))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(1L)).await // + 0 0 0 0 1
      nextStateA.map(_.registers.getOV) must beTrue.await
      // A = -7972, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(0x400000000L | 7972000055L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await // - 0 0 0 0 0
      nextStateX.map(_.registers.getOV) must beTrue.await
    }

    "throw an exception for index registers overflow" in {
      // A = 6897, I = 0, F = 0, C = 49 INC1
      execute(state, MixWord(6897000049L)) must throwAn[OverflowException].await
      // A = 6492, I = 0, F = 0, C = 50 INC2
      execute(state, MixWord(6492000050L)) must throwAn[OverflowException].await
      // A = 4759, I = 0, F = 0, C = 51 INC3
      execute(state, MixWord(4759000051L)) must throwAn[OverflowException].await
      // A = 7701, I = 0, F = 0, C = 52 INC4
      execute(state, MixWord(7701000052L)) must throwAn[OverflowException].await
      // A = 9639, I = 0, F = 0, C = 53 INC5
      execute(state, MixWord(9639000053L)) must throwAn[OverflowException].await
      // A = -4465, I = 0, F = 0, C = 54 INC6
      execute(state, MixWord(0x400000000L | 4465000054L)) must throwAn[OverflowException].await
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 0, C = 48 INCA
      execute(stateWithOV, MixWord(1000048L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 49 INC1
      execute(stateWithOV, MixWord(1000049L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 50 INC2
      execute(stateWithOV, MixWord(1000050L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 51 INC3
      execute(stateWithOV, MixWord(1000051L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 52 INC4
      execute(stateWithOV, MixWord(1000052L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 53 INC5
      execute(stateWithOV, MixWord(1000053L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 54 INC6
      execute(stateWithOV, MixWord(1000054L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 55 INCX
      execute(stateWithOV, MixWord(1000055L)).map(_.registers.getOV) must beTrue.await
    }

    "increment a register by its negation preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(MixWord(3632L)).updatedX(MixWord(0x400000000L | 1308L))
      )
      // A = -3632, I = 0, F = 0, C = 48 INCA
      execute(stateWithSmallAX, MixWord(0x400000000L | 3632000048L)).map(_.registers.getA) must beEqualTo(MixWord(0L)).await
      // A = -3103, I = 0, F = 0, C = 49 INC1
      execute(stateWithSmallAX, MixWord(0x400000000L | 3103000049L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0)).await
      // A = -3508, I = 0, F = 0, C = 50 INC2
      execute(stateWithSmallAX, MixWord(0x400000000L | 3508000050L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0)).await
      // A = -5241, I = 0, F = 0, C = 51 INC3
      execute(stateWithSmallAX, MixWord(0x400000000L | 5241000051L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0)).await
      // A = -2299, I = 0, F = 0, C = 52 INC4
      execute(stateWithSmallAX, MixWord(0x400000000L | 2299000052L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0)).await
      // A = -361, I = 0, F = 0, C = 53 INC5
      execute(stateWithSmallAX, MixWord(0x400000000L | 361000053L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0)).await
      // A = 5535, I = 0, F = 0, C = 54 INC6
      execute(stateWithSmallAX, MixWord(5535000054L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4000)).await
      // A = 1308, I = 0, F = 0, C = 55 INCX
      execute(stateWithSmallAX, MixWord(1308000055L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await
    }
  }
}
