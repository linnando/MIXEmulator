package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryVirtualMachine._
import org.specs2.mutable.Specification

class BinaryConversionSpec extends Specification {
  "binary conversion module" should {
    "convert characters to numeric" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x4001f827)) // - 0 0 31 32 39
          .updatedX(BinaryMixWord(0x25e6f79e)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      val nextState = execute(state, BinaryMixWord(0x00000005))
      nextState.registers.getA must be equalTo BinaryMixWord(0x40c60624) // - 00 49 32 24 36
      nextState.registers.getX must be equalTo BinaryMixWord(0x25e6f79e) // + 37 57 47 30 30
      nextState.registers.getOV must beFalse
    }

    "process numeric overflow" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x5f7a5865)) // - 31 30 37 33 37
          .updatedX(BinaryMixWord(0x227e6822)) // + 34 31 38 32 34
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      val nextState = execute(state, BinaryMixWord(0x00000005))
      nextState.registers.getA must be equalTo BinaryMixWord(0x40000000) // - 00 00 00 00 00
      nextState.registers.getX must be equalTo BinaryMixWord(0x227e6822) // + 34 31 38 32 34
      nextState.registers.getOV must beFalse
    }

    "convert number to characters" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(BinaryMixWord(0x40c60623)) // - 00 49 32 24 35
          .updatedX(BinaryMixWord(0x25e6f79e)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 1, C = 5 CHAR
      val nextState = execute(state, BinaryMixWord(0x00000045))
      nextState.registers.getA must be equalTo BinaryMixWord(0x5e79f827) // - 30 30 31 32 39
      nextState.registers.getX must be equalTo BinaryMixWord(0x259649e7) // + 37 37 36 39 39
      nextState.registers.getOV must beFalse
    }
  }
}
