package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryVirtualMachine._
import org.linnando.mixemulator.vm.exceptions.{DivisionByZeroException, OverflowException}
import org.specs2.mutable.Specification

class BinaryArithmeticSpec extends Specification {
  "binary summation module" should {
    "sum two numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x13481096)), // + 19 18 1 2 22
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x01905032)) // + 1 36 5 0 50
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      val nextState = execute(state, BinaryMixWord(0x0fa00141))
      nextState.registers.getA must be equalTo BinaryMixWord(0x14d860c8) // + 20 54 6 3 8
      nextState.registers.getOV must beFalse
    }

    "sum two numbers with an overflow" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x20000000)), // + 32 0 0 0 0
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x20000001)) // + 32 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      val nextState = execute(state, BinaryMixWord(0x0fa00141))
      nextState.registers.getA must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      nextState.registers.getOV must beTrue
    }

    "not change the overflow flag if no overflow occurs" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedOV(true)
          .updatedA(BinaryMixWord(0x00000001)), // + 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000002)) // + 0 0 0 0 2
      )
      execute(state, BinaryMixWord(0x0fa00141)).registers.getOV must beTrue
    }

    "sum a positive number and its negation to the positive zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x00000001)), // + 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x40000001)) // - 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      execute(state, BinaryMixWord(0x0fa00141)).registers.getA must be equalTo BinaryMixWord(0x00000000) // + 0 0 0 0 0
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x40000001)), // - 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000001)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 1 ADD
      execute(state, BinaryMixWord(0x0fa00141)).registers.getA must be equalTo BinaryMixWord(0x40000000) // - 0 0 0 0 0
    }
  }

  "binary subtraction module" should {
    "subtract two numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x53480009)), // - 19 18 0 0 9
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x5f402580)) // - 31 16 2 22 0
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, BinaryMixWord(0x0fa00142))
      nextState.registers.getA must be equalTo BinaryMixWord(0x0bf82577) // + 11 62 2 21 55
      nextState.registers.getOV must beFalse
    }

    "subtract two numbers with an overflow" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x20000000)), // + 32 0 0 0 0
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x60000001)) // - 32 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, BinaryMixWord(0x0fa00142))
      nextState.registers.getA must be equalTo BinaryMixWord(0x00000001) // + 0 0 0 0 1
      nextState.registers.getOV must beTrue
    }

    "not change the overflow flag if no overflow occurs" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedOV(true)
          .updatedA(BinaryMixWord(0x00000001)), // + 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000002)) // + 0 0 0 0 2
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      val nextState = execute(state, BinaryMixWord(0x0fa00142))
      nextState.registers.getA must be equalTo BinaryMixWord(0x40000001) // - 0 0 0 0 1
      nextState.registers.getOV must beTrue
    }

    "subtract a positive number from itself to the positive zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x00000001)), // + 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000001)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      execute(state, BinaryMixWord(0x0fa00142)).registers.getA must be equalTo BinaryMixWord(0x00000000) // + 0 0 0 0 0
    }

    "subtract a negative number from itself to the negative zero" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x40000001)), // - 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x40000001)) // - 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 2 SUB
      execute(state, BinaryMixWord(0x0fa00142)).registers.getA must be equalTo BinaryMixWord(0x40000000) // - 0 0 0 0 0
    }
  }

  "binary multiplication module" should {
    "multiply positive numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x01041041)), // + 1 1 1 1 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x01041041)) // + 1 1 1 1 1
      )
      // A = 1000, I = 0, F = 0:5, C = 3 MUL
      val nextState = execute(state, BinaryMixWord(0x0fa00143))
      nextState.registers.getA must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      nextState.registers.getX must be equalTo BinaryMixWord(0x05103081) // + 5 4 3 2 1
    }

    "multiply negative numbers" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x72001c04)), // - 50 0 112 4
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x42000000)) // - 2 0 0 0 0
      )
      // A = 1000, I = 0, F = 0:5, C = 3 MUL
      val nextState = execute(state, BinaryMixWord(0x0fa00143))
      nextState.registers.getA must be equalTo BinaryMixWord(0x019000e0) // + 1 36 0 3 32
      nextState.registers.getX must be equalTo BinaryMixWord(0x08000000) // + 8 0 0 0 0
    }

    "multiply numbers of different signs" in {
      val state = initialState.copy(
        registers = initialState.registers.updatedA(BinaryMixWord(0x40000070)), // - 0 0 0 1 48
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x02000000)) // + 2 0 0 0 0
      )
      // A = 1000, I = 0, F = 1:1, C = 3 MUL
      val nextState = execute(state, BinaryMixWord(0x0fa00243))
      nextState.registers.getA must be equalTo BinaryMixWord(0x40000000) // - 0 0 0 0 0
      nextState.registers.getX must be equalTo BinaryMixWord(0x400000e0) // - 0 0 0 3 32
    }
  }

  "binary division module" should {
    "divide positive numbers" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x00000000))  // + 0 0 0 0 0
          .updatedX(BinaryMixWord(0x00000011)), // + 0 0 0 0 17
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000003)) // + 0 0 0 0 3
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, BinaryMixWord(0x0fa00144))
      nextState.registers.getA must be equalTo BinaryMixWord(0x00000005) // + 0 0 0 0 5
      nextState.registers.getX must be equalTo BinaryMixWord(0x00000002) // + 0 0 0 0 2
    }

    "divide negative numbers" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x40000000))  // - 0 0 0 0 0
          .updatedX(BinaryMixWord(0x134c00c1)), // + 19 19 0 3 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x40000080)) // - 0 0 0 2 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, BinaryMixWord(0x0fa00144))
      nextState.registers.getA must be equalTo BinaryMixWord(0x00269801) // + 0 9 41 32 1
      nextState.registers.getX must be equalTo BinaryMixWord(0x40000041) // - 0 0 0 1 1
    }

    "divide a negative number by a positive one" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x40000000))  // - 0 0 0 0 0
          .updatedX(BinaryMixWord(0x134c00c1)), // + 19 19 0 3 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000080)) // + 0 0 0 2 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      val nextState = execute(state, BinaryMixWord(0x0fa00144))
      nextState.registers.getA must be equalTo BinaryMixWord(0x40269801) // - 0 9 41 32 1
      nextState.registers.getX must be equalTo BinaryMixWord(0x40000041) // - 0 0 0 1 1
    }

    "throw an exception on division by zero" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x00000000))  // + 0 0 0 0 0
          .updatedX(BinaryMixWord(0x00000001)), // + 0 0 0 0 1
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000000)) // + 0 0 0 0 0
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      execute(state, BinaryMixWord(0x0fa00144)) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x00000001))  // + 0 0 0 0 1
          .updatedX(BinaryMixWord(0x00000000)), // + 0 0 0 0 0
        memory = initialState.memory.updated(BinaryMixIndex(1000), BinaryMixWord(0x00000001)) // + 0 0 0 0 1
      )
      // A = 1000, I = 0, F = 0:5, C = 4 DIV
      execute(state, BinaryMixWord(0x0fa00144)) must throwAn[OverflowException]
    }
  }
}
