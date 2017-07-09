package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryProcessingModel.{BinaryMixIndex, BinaryMixWord, execute, initialState}
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.mutable.Specification

class BinaryIncrementSpec extends Specification {
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(BinaryMixWord(0x3ffffe30)) // + 63 63 63 56 48
      .updatedX(BinaryMixWord(0x7ffff51c)) // - 63 63 63 20 28
      .updatedI(1, BinaryMixIndex(0x07c3)) // + 31 3
      .updatedI(2, BinaryMixIndex(0x08c8)) // + 35 8
      .updatedI(3, BinaryMixIndex(0x0d29)) // + 52 41
      .updatedI(4, BinaryMixIndex(0x05bf)) // + 32 63
      .updatedI(5, BinaryMixIndex(0x00fd)) // + 3 61
      .updatedI(6, BinaryMixIndex(0x1de3)) // - 55 35
  )

  "binary increment module" should {
    "increment registers by a fixed number" in {
      // A = 1, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, BinaryMixWord(0x00040030))
      nextStateA.registers.getA must be equalTo BinaryMixWord(0x3ffffe31) // + 63 63 63 56 49
      nextStateA.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 49 INC1
      val nextState1 = execute(state, BinaryMixWord(0x00040031))
      nextState1.registers.getI(1) must be equalTo BinaryMixIndex(0x07c4) // + 31 4
      nextState1.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 50 INC2
      val nextState2 = execute(state, BinaryMixWord(0x00040032))
      nextState2.registers.getI(2) must be equalTo BinaryMixIndex(0x08c9) // + 35 9
      nextState2.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 51 INC3
      val nextState3 = execute(state, BinaryMixWord(0x00040033))
      nextState3.registers.getI(3) must be equalTo BinaryMixIndex(0x0d2a) // + 52 42
      nextState3.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 52 INC4
      val nextState4 = execute(state, BinaryMixWord(0x00040034))
      nextState4.registers.getI(4) must be equalTo BinaryMixIndex(0x05c0) // + 23 0
      nextState4.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 53 INC5
      val nextState5 = execute(state, BinaryMixWord(0x00040035))
      nextState5.registers.getI(5) must be equalTo BinaryMixIndex(0x00fe) // + 3 62
      nextState5.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 54 INC6
      val nextState6 = execute(state, BinaryMixWord(0x00040036))
      nextState6.registers.getI(6) must be equalTo BinaryMixIndex(0x1de2) // - 55 34
      nextState6.registers.getOV must beFalse
      // A = 1, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, BinaryMixWord(0x00040037))
      nextStateX.registers.getX must be equalTo BinaryMixWord(0x7ffff51b) // - 63 63 63 20 27
      nextStateX.registers.getOV must beFalse
    }

    "increment registers by an indexed address" in {
      // A = 1, I = 5, F = 0, C = 48 INCA
      val nextStateA = execute(state, BinaryMixWord(0x00045030))
      nextStateA.registers.getA must be equalTo BinaryMixWord(0x3fffff2e) // + 63 63 63 60 46
      nextStateA.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 49 INC1
      val nextState1 = execute(state, BinaryMixWord(0x00045031))
      nextState1.registers.getI(1) must be equalTo BinaryMixIndex(0x08c1) // + 35 1
      nextState1.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 50 INC2
      val nextState2 = execute(state, BinaryMixWord(0x00045032))
      nextState2.registers.getI(2) must be equalTo BinaryMixIndex(0x09c6) // + 39 6
      nextState2.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 51 INC3
      val nextState3 = execute(state, BinaryMixWord(0x00045033))
      nextState3.registers.getI(3) must be equalTo BinaryMixIndex(0x0e27) // + 56 39
      nextState3.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 52 INC4
      val nextState4 = execute(state, BinaryMixWord(0x00045034))
      nextState4.registers.getI(4) must be equalTo BinaryMixIndex(0x06bd) // + 26 61
      nextState4.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 53 INC5
      val nextState5 = execute(state, BinaryMixWord(0x00045035))
      nextState5.registers.getI(5) must be equalTo BinaryMixIndex(0x01fb) // + 7 59
      nextState5.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 54 INC6
      val nextState6 = execute(state, BinaryMixWord(0x00045036))
      nextState6.registers.getI(6) must be equalTo BinaryMixIndex(0x1ce5) // - 51 37
      nextState6.registers.getOV must beFalse
      // A = 1, I = 5, F = 0, C = 55 INCX
      val nextStateX = execute(state, BinaryMixWord(0x00045037))
      nextStateX.registers.getX must be equalTo BinaryMixWord(0x7ffff41e) // - 63 63 63 16 30
      nextStateX.registers.getOV must beFalse
    }

    "increment registers A and X with an overflow" in {
      // A = 0x1d1, I = 0, F = 0, C = 48 INCA
      val nextStateA = execute(state, BinaryMixWord(0x07440030))
      nextStateA.registers.getA must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      nextStateA.registers.getOV must beTrue
      // A = -0xae4, I = 0, F = 0, C = 55 INCX
      val nextStateX = execute(state, BinaryMixWord(0x6b900037))
      nextStateX.registers.getX must be equalTo BinaryMixWord(0x40000000) // - 0 0 0 0 0
      nextStateX.registers.getOV must beTrue
    }

    "throw an exception for index registers overflow" in {
      // A = 0x83d, I = 0, F = 0, C = 49 INC1
      execute(state, BinaryMixWord(0x20f40031)) must throwAn[OverflowException]
      // A = 0x738, I = 0, F = 0, C = 50 INC2
      execute(state, BinaryMixWord(0x1ce00032)) must throwAn[OverflowException]
      // A = 0x2d7, I = 0, F = 0, C = 51 INC3
      execute(state, BinaryMixWord(0x0b5c0033)) must throwAn[OverflowException]
      // A = 0xa41, I = 0, F = 0, C = 52 INC4
      execute(state, BinaryMixWord(0x39040034)) must throwAn[OverflowException]
      // A = 0xf03, I = 0, F = 0, C = 53 INC5
      execute(state, BinaryMixWord(0x3c0c0035)) must throwAn[OverflowException]
      // A = -0x21d, I = 0, F = 0, C = 54 INC6
      execute(state, BinaryMixWord(0x48740036)) must throwAn[OverflowException]
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 0, C = 48 INCA
      execute(stateWithOV, BinaryMixWord(0x00040030)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 49 INC1
      execute(stateWithOV, BinaryMixWord(0x00040031)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 50 INC2
      execute(stateWithOV, BinaryMixWord(0x00040032)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 51 INC3
      execute(stateWithOV, BinaryMixWord(0x00040033)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 52 INC4
      execute(stateWithOV, BinaryMixWord(0x00040034)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 53 INC5
      execute(stateWithOV, BinaryMixWord(0x00040035)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 54 INC6
      execute(stateWithOV, BinaryMixWord(0x00040036)).registers.getOV must beTrue
      // A = 1, I = 0, F = 0, C = 55 INCX
      execute(stateWithOV, BinaryMixWord(0x00040037)).registers.getOV must beTrue
    }

    "increment a register by its negation preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(BinaryMixWord(0x00000e30)).updatedX(BinaryMixWord(0x4000051c))
      )
      // A = -0xe30, I = 0, F = 0, C = 48 INCA
      execute(stateWithSmallAX, BinaryMixWord(0x78c00030)).registers.getA must be equalTo BinaryMixWord(0x00000000)
      // A = -0x7c3, I = 0, F = 0, C = 49 INC1
      execute(stateWithSmallAX, BinaryMixWord(0x5f0c0031)).registers.getI(1) must be equalTo BinaryMixIndex(0x0000)
      // A = -0x8c8, I = 0, F = 0, C = 50 INC2
      execute(stateWithSmallAX, BinaryMixWord(0x63200032)).registers.getI(2) must be equalTo BinaryMixIndex(0x0000)
      // A = -0xd29, I = 0, F = 0, C = 51 INC3
      execute(stateWithSmallAX, BinaryMixWord(0x74a40033)).registers.getI(3) must be equalTo BinaryMixIndex(0x0000)
      // A = -0x5bf, I = 0, F = 0, C = 52 INC4
      execute(stateWithSmallAX, BinaryMixWord(0x56fc0034)).registers.getI(4) must be equalTo BinaryMixIndex(0x0000)
      // A = -0x0fd, I = 0, F = 0, C = 53 INC5
      execute(stateWithSmallAX, BinaryMixWord(0x43f40035)).registers.getI(5) must be equalTo BinaryMixIndex(0x0000)
      // A = 0xde3, I = 0, F = 0, C = 54 INC6
      execute(stateWithSmallAX, BinaryMixWord(0x378c0036)).registers.getI(6) must be equalTo BinaryMixIndex(0x1000)
      // A = 0x51c, I = 0, F = 0, C = 55 INCX
      execute(stateWithSmallAX, BinaryMixWord(0x14700037)).registers.getX must be equalTo BinaryMixWord(0x40000000)
    }
  }
}
