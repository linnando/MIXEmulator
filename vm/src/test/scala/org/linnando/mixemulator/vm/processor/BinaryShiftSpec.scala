package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryProcessingModel._
import org.specs2.mutable.Specification

class BinaryShiftSpec extends Specification {
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(BinaryMixWord(0x01083105)) // + 1 2 3 4 5
      .updatedX(BinaryMixWord(0x461c824a)) // - 6 7 8 9 10
  )
  "binary shift module" should {
    "shift A to the left" in {
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState = execute(state, BinaryMixWord(0x00080006))
      nextState.registers.getA must be equalTo BinaryMixWord(0x03105000) // + 3 4 5 0 0
      nextState.registers.getX must be equalTo BinaryMixWord(0x461c824a) // - 6 7 8 9 10
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState = execute(state, BinaryMixWord(0x00080046))
      nextState.registers.getA must be equalTo BinaryMixWord(0x00001083) // + 0 0 1 2 3
      nextState.registers.getX must be equalTo BinaryMixWord(0x461c824a) // - 6 7 8 9 10
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      val nextState = execute(state, BinaryMixWord(0x00040086))
      nextState.registers.getA must be equalTo BinaryMixWord(0x020c4146) // + 2 3 4 5 6
      nextState.registers.getX must be equalTo BinaryMixWord(0x47209280) // - 7 8 9 10 0
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState = execute(state, BinaryMixWord(0x000400c6))
      nextState.registers.getA must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      nextState.registers.getX must be equalTo BinaryMixWord(0x45187209) // - 5 6 7 8 9
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState = execute(state, BinaryMixWord(0x00040106))
      nextState.registers.getA must be equalTo BinaryMixWord(0x020c4146) // + 2 3 4 5 6
      nextState.registers.getX must be equalTo BinaryMixWord(0x47209281) // - 7 8 9 10 1
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      val nextState = execute(state, BinaryMixWord(0x00040146))
      nextState.registers.getA must be equalTo BinaryMixWord(0x0a0420c4) // + 10 1 2 3 4
      nextState.registers.getX must be equalTo BinaryMixWord(0x45187209) // - 5 6 7 8 9
    }
  }

  "shift scenario" should {
    "perform consecutive shifts" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState0 = execute(state, BinaryMixWord(0x000400c6))
      nextState0.registers.getA must be equalTo BinaryMixWord(0x000420c4) // + 0 1 2 3 4
      nextState0.registers.getX must be equalTo BinaryMixWord(0x45187209) // - 5 6 7 8 9
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState1 = execute(nextState0, BinaryMixWord(0x00080006))
      nextState1.registers.getA must be equalTo BinaryMixWord(0x020c4000) // + 2 3 4 0 0
      nextState1.registers.getX must be equalTo BinaryMixWord(0x45187209) // - 5 6 7 8 9
      // A = 4, I = 0, F = 5, C = 6 SRC
      val nextState2 = execute(nextState1, BinaryMixWord(0x00100146))
      nextState2.registers.getA must be equalTo BinaryMixWord(0x061c8242) // + 6 7 8 9 2
      nextState2.registers.getX must be equalTo BinaryMixWord(0x43100005) // - 3 4 0 0 5
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState3 = execute(nextState2, BinaryMixWord(0x00080046))
      nextState3.registers.getA must be equalTo BinaryMixWord(0x000061c8) // + 0 0 6 7 8
      nextState3.registers.getX must be equalTo BinaryMixWord(0x43100005) // - 3 4 0 0 5
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState4 = execute(nextState3, BinaryMixWord(0x00040106))
      nextState4.registers.getA must be equalTo BinaryMixWord(0x00187203) // + 0 6 7 8 3
      nextState4.registers.getX must be equalTo BinaryMixWord(0x44000140) // - 4 0 0 5 0
    }
  }
}
