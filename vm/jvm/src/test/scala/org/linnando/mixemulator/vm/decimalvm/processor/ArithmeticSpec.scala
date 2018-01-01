package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.{DivisionByZeroException, OverflowException}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ArithmeticSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState

  "decimal summation module" should {
    "sum two numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(1918010222L)), // + 19 18 1 2 22
        memory = initialState.memory.updated(MixIndex(1000), MixWord(136050050L)) // + 1 36 5 0 50
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      val nextState = execute(state, MixWord(1000000501L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(2054060272L)).await // + 20 54 6 2 72
      nextState.map(_.registers.getOV) must beFalse.await
    }

    "sum two numbers with an overflow" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(5000000000L)), // + 50 0 0 0 0
        memory = initialState.memory.updated(MixIndex(1000), MixWord(5000000001L)) // + 50 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      val nextState = execute(state, MixWord(1000000501L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1L)).await // + 0 0 0 0 1
      nextState.map(_.registers.getOV) must beTrue.await
    }

    "not change the overflow flag if no overflow occurs" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedOV(true)
          .updatedA(MixWord(1L)), // + 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(2L)) // + 0 0 0 0 2
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      execute(state, MixWord(1000000501L)).map(_.registers.getOV) must beTrue.await
    }

    "sum a positive number and its negation to the positive zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(1L)), // + 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000001L)) // - 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      execute(state, MixWord(1000000501L)).map(_.registers.getA) must beEqualTo(MixWord(0L)).await // + 0 0 0 0 0
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(0x400000001L)), // - 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(1L)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      execute(state, MixWord(1000000501L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await // - 0 0 0 0 0
    }
  }

  "decimal subtraction module" should {
    "subtract two numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(0x400000000L | 1918000009L)), // - 19 18 0 0 9
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000000L | 3116022200L)) // - 31 16 2 22 0
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, MixWord(1000000502L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1198022191L)).await // + 11 98 2 21 91
      nextState.map(_.registers.getOV) must beFalse.await
    }

    "subtract two numbers with an overflow" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(5000000000L)), // + 50 0 0 0 0
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000000L | 5000000001L)) // - 50 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, MixWord(1000000502L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1L)).await // + 0 0 0 0 1
      nextState.map(_.registers.getOV) must beTrue.await
    }

    "not change the overflow flag if no overflow occurs" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedOV(true)
          .updatedA(MixWord(1L)), // + 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(2L)) // + 0 0 0 0 2
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, MixWord(1000000502L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000001L)).await // - 0 0 0 0 1
      nextState.map(_.registers.getOV) must beTrue.await
    }

    "subtract a positive number from itself to the positive zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(1L)), // + 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(1L)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      execute(state, MixWord(1000000502L)).map(_.registers.getA) must beEqualTo(MixWord(0L)).await // + 0 0 0 0 0
    }

    "subtract a negative number from itself to the negative zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(0x400000001L)), // - 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000001L)) // - 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      execute(state, MixWord(1000000502L)).map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await // - 0 0 0 0 0
    }
  }

  "decimal multiplication module" should {
    "multiply positive numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(101010101)), // + 1 1 1 1 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(101010101)) // + 1 1 1 1 1
      )
      // A = 1000, I = 0, F = 0:5, C = 3 MUL
      val nextState = execute(state, MixWord(1000000503L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(1020304L)).await // + 0 1 2 3 4
      nextState.map(_.registers.getX) must beEqualTo(MixWord(504030201L)).await // + 5 4 3 2 1
    }

    "multiply negative numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(0x400000000L | 5000011204L)), // - 50 0 112 4
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000000L | 200000000L)) // - 2 0 0 0 0
      )
      // A = 1000, I = 0, F = 0:5, C = 3 MUL
      val nextState = execute(state, MixWord(1000000503L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(100000224L)).await // + 1 0 0 2 24
      nextState.map(_.registers.getX) must beEqualTo(MixWord(800000000L)).await // + 8 0 0 0 0
    }

    "multiply numbers of different signs" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(MixWord(0x400000000L | 148L)), // - 0 0 0 1 48
        memory = initialState.memory.updated(MixIndex(1000), MixWord(200000000L)) // + 2 0 0 0 0
      )
      // A = 1000, I = 0, F = 1:1, C = 3 MUL
      val nextState = execute(state, MixWord(1000000903L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000000L)).await // - 0 0 0 0 0
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 296L)).await // - 0 0 0 2 96
    }
  }

  "decimal division module" should {
    "divide positive numbers" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0L)) // + 0 0 0 0 0
          .updatedX(MixWord(17L)), // + 0 0 0 0 17
        memory = initialState.memory.updated(MixIndex(1000), MixWord(3L)) // + 0 0 0 0 3
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, MixWord(1000000504L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(5L)).await // + 0 0 0 0 5
      nextState.map(_.registers.getX) must beEqualTo(MixWord(2L)).await // + 0 0 0 0 2
    }

    "divide negative numbers" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L)) // - 0 0 0 0 0
          .updatedX(MixWord(1919000301L)), // + 19 19 0 3 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0x400000000L | 200L)) // - 0 0 0 2 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, MixWord(1000000504L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(9595001L)).await // + 0 9 59 50 1
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 101L)).await // - 0 0 0 1 1
    }

    "divide a negative number by a positive one" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x400000000L)) // - 0 0 0 0 0
          .updatedX(MixWord(1919000301L)), // + 19 19 0 3 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(200L)) // + 0 0 0 2 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, MixWord(1000000504L))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x400000000L | 9595001L)).await // - 0 9 59 50 1
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x400000000L | 101L)).await // - 0 0 0 1 1
    }

    "throw an exception on division by zero" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0L)) // + 0 0 0 0 0
          .updatedX(MixWord(1L)), // + 0 0 0 0 1
        memory = initialState.memory.updated(MixIndex(1000), MixWord(0L)) // + 0 0 0 0 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      execute(state, MixWord(1000000504L)) must throwA[DivisionByZeroException].await
    }

    "throw an exception if the dividend is too big" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(1L)) // + 0 0 0 0 1
          .updatedX(MixWord(0L)), // + 0 0 0 0 0
        memory = initialState.memory.updated(MixIndex(1000), MixWord(1L)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      execute(state, MixWord(1000000504L)) must throwAn[OverflowException].await
    }
  }
}
