package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ConversionSpec(implicit ee: ExecutionEnv) extends Specification {
  import binary._
  private val initialState = binary.initialState

  "binary conversion module" should {
    "convert characters to numeric" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x4001f827)) // - 0 0 31 32 39
          .updatedX(MixWord(0x25e6f79e)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      val nextState = execute(state, MixWord(0x00000005))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x40c60624)).await // - 00 49 32 24 36
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x25e6f79e)).await // + 37 57 47 30 30
      nextState.map(_.registers.getOV) must beFalse.await
    }

    "process numeric overflow" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x5f7a5865)) // - 31 30 37 33 37
          .updatedX(MixWord(0x227e6822)) // + 34 31 38 32 34
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      val nextState = execute(state, MixWord(0x00000005))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x40000000)).await // - 00 00 00 00 00
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x227e6822)).await // + 34 31 38 32 34
      nextState.map(_.registers.getOV) must beFalse.await
    }

    "convert number to characters" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x40c60623)) // - 00 49 32 24 35
          .updatedX(MixWord(0x25e6f79e)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 1, C = 5 CHAR
      val nextState = execute(state, MixWord(0x00000045))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x5e79f827)).await // - 30 30 31 32 39
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x259649e7)).await // + 37 37 36 39 39
      nextState.map(_.registers.getOV) must beFalse.await
    }
  }
}
