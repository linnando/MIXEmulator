package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class DecrementSpec(implicit ee: ExecutionEnv) extends Specification {

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

  "binary decrement module" should {
    "decrement registers by a fixed number" in {
      // A = 1, I = 0, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(0x00040070))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x3ffffe2f)).await // + 63 63 63 56 47
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 49 DEC1
      val nextState1 = execute(state, MixWord(0x00040071))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(0x07c2)).await // + 31 2
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 50 DEC2
      val nextState2 = execute(state, MixWord(0x00040072))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(0x08c7)).await // + 35 7
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 51 DEC3
      val nextState3 = execute(state, MixWord(0x00040073))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0d28)).await // + 52 40
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 52 DEC4
      val nextState4 = execute(state, MixWord(0x00040074))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(0x05be)).await // + 22 62
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 53 DEC5
      val nextState5 = execute(state, MixWord(0x00040075))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(0x00fc)).await // + 3 60
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 54 DEC6
      val nextState6 = execute(state, MixWord(0x00040076))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1de4)).await // - 55 36
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 0, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(0x00040077))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x7ffff51d)).await // - 63 63 63 20 29
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "decrement registers by an indexed address" in {
      // A = 1, I = 5, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(0x00045070))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x3ffffd32)).await // + 63 63 63 52 50
      nextStateA.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 49 DEC1
      val nextState1 = execute(state, MixWord(0x00045071))
      nextState1.map(_.registers.getI(1)) must beEqualTo(MixIndex(0x06c5)).await // + 27 5
      nextState1.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 50 DEC2
      val nextState2 = execute(state, MixWord(0x00045072))
      nextState2.map(_.registers.getI(2)) must beEqualTo(MixIndex(0x07ca)).await // + 31 10
      nextState2.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 51 DEC3
      val nextState3 = execute(state, MixWord(0x00045073))
      nextState3.map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0c2b)).await // + 48 43
      nextState3.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 52 DEC4
      val nextState4 = execute(state, MixWord(0x00045074))
      nextState4.map(_.registers.getI(4)) must beEqualTo(MixIndex(0x04c1)).await // + 19 1
      nextState4.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 53 DEC5
      val nextState5 = execute(state, MixWord(0x00045075))
      nextState5.map(_.registers.getI(5)) must beEqualTo(MixIndex(0x1001)).await // - 0 1
      nextState5.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 54 DEC6
      val nextState6 = execute(state, MixWord(0x00045076))
      nextState6.map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1ee1)).await // - 59 33
      nextState6.map(_.registers.getOV) must beFalse.await
      // A = 1, I = 5, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(0x00045077))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x7ffff61a)).await // - 63 63 63 24 26
      nextStateX.map(_.registers.getOV) must beFalse.await
    }

    "decrement registers A and X with an overflow" in {
      // A = -0x1d1, I = 0, F = 1, C = 48 DECA
      val nextStateA = execute(state, MixWord(0x47440070))
      nextStateA.map(_.registers.getA) must beEqualTo(MixWord(0x00000001)).await // + 0 0 0 0 1
      nextStateA.map(_.registers.getOV) must beTrue.await
      // A = 0xae4, I = 0, F = 1, C = 55 DECX
      val nextStateX = execute(state, MixWord(0x2b900077))
      nextStateX.map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await // - 0 0 0 0 0
      nextStateX.map(_.registers.getOV) must beTrue.await
    }

    "throw an exception for index registers overflow" in {
      // A = -0x83d, I = 0, F = 1, C = 49 DEC1
      execute(state, MixWord(0x60f40071)) must throwAn[OverflowException].await
      // A = -0x738, I = 0, F = 1, C = 50 DEC2
      execute(state, MixWord(0x5ce00072)) must throwAn[OverflowException].await
      // A = -0x2d7, I = 0, F = 1, C = 51 DEC3
      execute(state, MixWord(0x4b5c0073)) must throwAn[OverflowException].await
      // A = -0xa41, I = 0, F = 1, C = 52 DEC4
      execute(state, MixWord(0x79040074)) must throwAn[OverflowException].await
      // A = -0xf03, I = 0, F = 1, C = 53 DEC5
      execute(state, MixWord(0x7c0c0075)) must throwAn[OverflowException].await
      // A = 0x21d, I = 0, F = 1, C = 54 DEC6
      execute(state, MixWord(0x08740076)) must throwAn[OverflowException].await
    }

    "not change the overflow flag if no overflow occurs" in {
      val stateWithOV = state.copy(registers = state.registers.updatedOV(true))
      // A = 1, I = 0, F = 1, C = 48 DECA
      execute(stateWithOV, MixWord(0x00040070)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 49 DEC1
      execute(stateWithOV, MixWord(0x00040071)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 50 DEC2
      execute(stateWithOV, MixWord(0x00040072)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 51 DEC3
      execute(stateWithOV, MixWord(0x00040073)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 52 DEC4
      execute(stateWithOV, MixWord(0x00040074)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 53 DEC5
      execute(stateWithOV, MixWord(0x00040075)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 54 DEC6
      execute(stateWithOV, MixWord(0x00040076)).map(_.registers.getOV) must beTrue.await
      // A = 1, I = 0, F = 1, C = 55 DECX
      execute(stateWithOV, MixWord(0x00040077)).map(_.registers.getOV) must beTrue.await
    }

    "decrement a register by itself preserving register's sign" in {
      val stateWithSmallAX = state.copy(
        registers = state.registers.updatedA(MixWord(0x00000e30)).updatedX(MixWord(0x4000051c))
      )
      // A = 0xe30, I = 0, F = 1, C = 48 DECA
      execute(stateWithSmallAX, MixWord(0x38c00070)).map(_.registers.getA) must beEqualTo(MixWord(0x00000000)).await
      // A = 0x7c3, I = 0, F = 1, C = 49 DEC1
      execute(stateWithSmallAX, MixWord(0x1f0c0071)).map(_.registers.getI(1)) must beEqualTo(MixIndex(0x0000)).await
      // A = 0x8c8, I = 0, F = 1, C = 50 DEC2
      execute(stateWithSmallAX, MixWord(0x23200072)).map(_.registers.getI(2)) must beEqualTo(MixIndex(0x0000)).await
      // A = 0xd29, I = 0, F = 1, C = 51 DEC3
      execute(stateWithSmallAX, MixWord(0x34a40073)).map(_.registers.getI(3)) must beEqualTo(MixIndex(0x0000)).await
      // A = 0x5bf, I = 0, F = 1, C = 52 DEC4
      execute(stateWithSmallAX, MixWord(0x16fc0074)).map(_.registers.getI(4)) must beEqualTo(MixIndex(0x0000)).await
      // A = 0x0fd, I = 0, F = 1, C = 53 DEC5
      execute(stateWithSmallAX, MixWord(0x03f40075)).map(_.registers.getI(5)) must beEqualTo(MixIndex(0x0000)).await
      // A = -0xde3, I = 0, F = 1, C = 54 DEC6
      execute(stateWithSmallAX, MixWord(0x778c0076)).map(_.registers.getI(6)) must beEqualTo(MixIndex(0x1000)).await
      // A = -0x51c, I = 0, F = 1, C = 55 DECX
      execute(stateWithSmallAX, MixWord(0x54700077)).map(_.registers.getX) must beEqualTo(MixWord(0x40000000)).await
    }
  }
}
