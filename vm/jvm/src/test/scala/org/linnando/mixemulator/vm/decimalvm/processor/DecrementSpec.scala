package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class DecrementSpec(implicit ee: ExecutionEnv) extends Specification {

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

  "decimal decrement module" should {
    "decrement registers by a fixed number" in {
      // A = 1, I = 0, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(1000148L))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(9999995647L)).await // + 99 99 99 56 47
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 49 DEC1
      val nextState1 = execute(state, MixWord(1000149L))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(3102)).await // + 31 2
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 50 DEC2
      val nextState2 = execute(state, MixWord(1000150L))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(3507)).await // + 35 7
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 51 DEC3
      val nextState3 = execute(state, MixWord(1000151L))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(5240)).await // + 52 40
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 52 DEC4
      val nextState4 = execute(state, MixWord(1000152L))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(2298)).await // + 22 98
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 53 DEC5
      val nextState5 = execute(state, MixWord(1000153L))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(360)).await // + 3 60
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 54 DEC6
      val nextState6 = execute(state, MixWord(1000154L))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 5536).toShort)).await // - 55 36
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(1000155L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 9999992029L)).await // - 99 99 99 20 29
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "decrement registers by an indexed address" in {
      // A = 1, I = 5, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(1050148L))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(9999995286L)).await // + 99 99 99 52 86
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 49 DEC1
      val nextState1 = execute(state, MixWord(1050149L))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(2741)).await // + 27 41
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 50 DEC2
      val nextState2 = execute(state, MixWord(1050150L))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(3146)).await // + 31 46
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 51 DEC3
      val nextState3 = execute(state, MixWord(1050151L))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(4879)).await // + 48 79
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 52 DEC4
      val nextState4 = execute(state, MixWord(1050152L))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(1937)).await // + 19 37
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 53 DEC5
      val nextState5 = execute(state, MixWord(1050153L))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(0x4001)).await // - 0 1
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 54 DEC6
      val nextState6 = execute(state, MixWord(1050154L))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex((0x4000 | 5897).toShort)).await // - 58 97
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(1050155L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 9999992390L)).await // - 99 99 99 23 90
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "decrement registers A and X with an overflow" in {
      // A = -4353, I = 0, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(0x400000000L | 4353000148L))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(1L)).await // + 0 0 0 0 1
      nextStateA.map(_.registers.getOV) must beTrue.await
      // A = 7972, I = 0, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(7972000155L))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await // - 0 0 0 0 0
      nextStateX.map(_.registers.getOV) must beTrue.await
    }

    "throw an exception for index registers overflow" in {
      // A = -6897, I = 0, F = 1, C = 49 DEC1
      execute(state, MixWord(0x400000000L | 6897000149L)) must throwAn[OverflowException].await
      // A = -6492, I = 0, F = 1, C = 50 DEC2
      execute(state, MixWord(0x400000000L | 6492000150L)) must throwAn[OverflowException].await
      // A = -4759, I = 0, F = 1, C = 51 DEC3
      execute(state, MixWord(0x400000000L | 4759000151L)) must throwAn[OverflowException].await
      // A = -7701, I = 0, F = 1, C = 52 DEC4
      execute(state, MixWord(0x400000000L | 7701000152L)) must throwAn[OverflowException].await
      // A = -9639, I = 0, F = 1, C = 53 DEC5
      execute(state, MixWord(0x400000000L | 9639000153L)) must throwAn[OverflowException].await
      // A = 4465, I = 0, F = 1, C = 54 DEC6
      execute(state, MixWord(4465000154L)) must throwAn[OverflowException].await
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 1, C = 48 DECA
      execute(stateWithOV, MixWord(1000148L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 49 DEC1
      execute(stateWithOV, MixWord(1000149L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 50 DEC2
      execute(stateWithOV, MixWord(1000150L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 51 DEC3
      execute(stateWithOV, MixWord(1000151L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 52 DEC4
      execute(stateWithOV, MixWord(1000152L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 53 DEC5
      execute(stateWithOV, MixWord(1000153L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 54 DEC6
      execute(stateWithOV, MixWord(1000154L)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 55 DECX
      execute(stateWithOV, MixWord(1000155L)).map(_.registers.getOV) must beTrue.await
    }

    "decrement a register by itself preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(MixWord(3632L)).updatedX(MixWord(0x400000000L | 1308L))
      )
      // A = 3632, I = 0, F = 1, C = 48 DECA
      execute(stateWithSmallAX, MixWord(3632000148L)).map(_.registers.getA) must beEqualTo(MixWord(0L)).await
      // A = 3103, I = 0, F = 1, C = 49 DEC1
      execute(stateWithSmallAX, MixWord(3103000149L)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0)).await
      // A = 3508, I = 0, F = 1, C = 50 DEC2
      execute(stateWithSmallAX, MixWord(3508000150L)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0)).await
      // A = 5241, I = 0, F = 1, C = 51 DEC3
      execute(stateWithSmallAX, MixWord(5241000151L)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0)).await
      // A = 2299, I = 0, F = 1, C = 52 DEC4
      execute(stateWithSmallAX, MixWord(2299000152L)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0)).await
      // A = 361, I = 0, F = 1, C = 53 DEC5
      execute(stateWithSmallAX, MixWord(361000153L)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0)).await
      // A = -5535, I = 0, F = 1, C = 54 DEC6
      execute(stateWithSmallAX, MixWord(0x400000000L | 5535000154L)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x4000)).await
      // A = -1308, I = 0, F = 1, C = 55 DECX
      execute(stateWithSmallAX, MixWord(0x400000000L | 1308000155L)).map(_.registers.getX) must beEqualTo(MixWord(0x400000000L)).await
    }
  }
}
