package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class IncrementSpec(implicit ee: ExecutionEnv) extends Specification {

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

  "binary increment module" should {
    "increment registers by a fixed number" in {
      // A = 1, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(0x00040030))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x3ffffe31)).await // + 63 63 63 56 49
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(0x00040031))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(0x07c4)).await // + 31 4
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(0x00040032))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(0x08c9)).await // + 35 9
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(0x00040033))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0d2a)).await // + 52 42
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(0x00040034))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(0x05c0)).await // + 23 0
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(0x00040035))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(0x00fe)).await // + 3 62
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(0x00040036))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1de2)).await // - 55 34
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(0x00040037))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x7ffff51b)).await // - 63 63 63 20 27
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "increment registers by an indexed address" in {
      // A = 1, I = 5, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(0x00045030))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x3fffff2e)).await // + 63 63 63 60 46
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 49 INC1
      val nextState1 = execute(state, MixWord(0x00045031))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(0x08c1)).await // + 35 1
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 50 INC2
      val nextState2 = execute(state, MixWord(0x00045032))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(0x09c6)).await // + 39 6
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 51 INC3
      val nextState3 = execute(state, MixWord(0x00045033))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0e27)).await // + 56 39
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 52 INC4
      val nextState4 = execute(state, MixWord(0x00045034))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(0x06bd)).await // + 26 61
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 53 INC5
      val nextState5 = execute(state, MixWord(0x00045035))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(0x01fb)).await // + 7 59
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 54 INC6
      val nextState6 = execute(state, MixWord(0x00045036))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1ce5)).await // - 51 37
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(0x00045037))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x7ffff41e)).await // - 63 63 63 16 30
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "increment registers A and X with an overflow" in {
      // A = 0x1d1, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, MixWord(0x07440030))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x00000001)).await // + 0 0 0 0 1
      nextStateA.map(_.registers.getOV) must beTrue.await
      // A = -0xae4, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, MixWord(0x6b900037))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await // - 0 0 0 0 0
      nextStateX.map(_.registers.getOV) must beTrue.await
    }

    "throw an exception for index registers overflow" in {
      // A = 0x83d, I = 0, F = 0, C = 49 INC1
      execute(state, MixWord(0x20f40031)) must throwAn[OverflowException].await
      // A = 0x738, I = 0, F = 0, C = 50 INC2
      execute(state, MixWord(0x1ce00032)) must throwAn[OverflowException].await
      // A = 0x2d7, I = 0, F = 0, C = 51 INC3
      execute(state, MixWord(0x0b5c0033)) must throwAn[OverflowException].await
      // A = 0xa41, I = 0, F = 0, C = 52 INC4
      execute(state, MixWord(0x39040034)) must throwAn[OverflowException].await
      // A = 0xf03, I = 0, F = 0, C = 53 INC5
      execute(state, MixWord(0x3c0c0035)) must throwAn[OverflowException].await
      // A = -0x21d, I = 0, F = 0, C = 54 INC6
      execute(state, MixWord(0x48740036)) must throwAn[OverflowException].await
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 0, C = 48 INCA
      execute(stateWithOV, MixWord(0x00040030)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 49 INC1
      execute(stateWithOV, MixWord(0x00040031)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 50 INC2
      execute(stateWithOV, MixWord(0x00040032)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 51 INC3
      execute(stateWithOV, MixWord(0x00040033)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 52 INC4
      execute(stateWithOV, MixWord(0x00040034)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 53 INC5
      execute(stateWithOV, MixWord(0x00040035)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 54 INC6
      execute(stateWithOV, MixWord(0x00040036)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 0, C = 55 INCX
      execute(stateWithOV, MixWord(0x00040037)).map(_.registers.getOV) must beTrue.await
    }

    "increment a register by its negation preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(MixWord(0x00000e30)).updatedX(MixWord(0x4000051c))
      )
      // A = -0xe30, I = 0, F = 0, C = 48 INCA
      execute(stateWithSmallAX, MixWord(0x78c00030)).map(_.registers.getA) must beEqualTo(MixWord(0x00000000)).await
      // A = -0x7c3, I = 0, F = 0, C = 49 INC1
      execute(stateWithSmallAX, MixWord(0x5f0c0031)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0x8c8, I = 0, F = 0, C = 50 INC2
      execute(stateWithSmallAX, MixWord(0x63200032)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0xd29, I = 0, F = 0, C = 51 INC3
      execute(stateWithSmallAX, MixWord(0x74a40033)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0x5bf, I = 0, F = 0, C = 52 INC4
      execute(stateWithSmallAX, MixWord(0x56fc0034)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0x0fd, I = 0, F = 0, C = 53 INC5
      execute(stateWithSmallAX, MixWord(0x43f40035)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0000)).await
      // A = 0xde3, I = 0, F = 0, C = 54 INC6
      execute(stateWithSmallAX, MixWord(0x378c0036)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1000)).await
      // A = 0x51c, I = 0, F = 0, C = 55 INCX
      execute(stateWithSmallAX, MixWord(0x14700037)).map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await
    }
  }
}
