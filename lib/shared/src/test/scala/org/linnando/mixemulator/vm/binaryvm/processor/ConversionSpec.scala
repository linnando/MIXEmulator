package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ConversionSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

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
      binary.execute(state, MixWord(0x00000005)).map(s => {
        s.registers.getA mustEqual MixWord(0x40c60624) // - 00 49 32 24 36
        s.registers.getX mustEqual MixWord(0x25e6f79e) // + 37 57 47 30 30
        s.registers.getOV mustEqual false
      })
    }

    "process numeric overflow" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x5f7a5865)) // - 31 30 37 33 37
          .updatedX(MixWord(0x227e6822)) // + 34 31 38 32 34
      )
      // A = 0, I = 0, F = 0, C = 5 NUM
      binary.execute(state, MixWord(0x00000005)).map(s => {
        s.registers.getA mustEqual MixWord(0x40000000) // - 00 00 00 00 00
        s.registers.getX mustEqual MixWord(0x227e6822) // + 34 31 38 32 34
        s.registers.getOV mustEqual false
      })
    }

    "convert number to characters" in {
      val state = initialState.copy(
        registers = initialState.registers
          .updatedA(MixWord(0x40c60623)) // - 00 49 32 24 35
          .updatedX(MixWord(0x25e6f79e)) // + 37 57 47 30 30
      )
      // A = 0, I = 0, F = 1, C = 5 CHAR
      binary.execute(state, MixWord(0x00000045)).map(s => {
        s.registers.getA mustEqual MixWord(0x5e79f827) // - 30 30 31 32 39
        s.registers.getX mustEqual MixWord(0x259649e7) // + 37 37 36 39 39
        s.registers.getOV mustEqual false
      })
    }
  }
}
