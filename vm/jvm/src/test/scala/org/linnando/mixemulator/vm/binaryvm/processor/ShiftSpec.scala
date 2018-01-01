package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ShiftSpec(implicit ee: ExecutionEnv) extends Specification {
  import binary._
  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x01083105)) // + 1 2 3 4 5
      .updatedX(MixWord(0x461c824a)) // - 6 7 8 9 10
  )
  "binary shift module" should {
    "shift A to the left" in {
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState = execute(state, MixWord(0x00080006))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x03105000)).await // + 3 4 5 0 0
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x461c824a)).await // - 6 7 8 9 10
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState = execute(state, MixWord(0x00080046))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x00001083)).await // + 0 0 1 2 3
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x461c824a)).await // - 6 7 8 9 10
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      val nextState = execute(state, MixWord(0x00040086))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x020c4146)).await // + 2 3 4 5 6
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x47209280)).await // - 7 8 9 10 0
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState = execute(state, MixWord(0x000400c6))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x000420c4)).await // + 0 1 2 3 4
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x45187209)).await // - 5 6 7 8 9
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState = execute(state, MixWord(0x00040106))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x020c4146)).await // + 2 3 4 5 6
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x47209281)).await // - 7 8 9 10 1
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      val nextState = execute(state, MixWord(0x00040146))
      nextState.map(_.registers.getA) must beEqualTo(MixWord(0x0a0420c4)).await // + 10 1 2 3 4
      nextState.map(_.registers.getX) must beEqualTo(MixWord(0x45187209)).await // - 5 6 7 8 9
    }
  }

  "shift scenario" should {
    "perform consecutive shifts" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      val nextState0 = execute(state, MixWord(0x000400c6))
      nextState0.map(_.registers.getA) must beEqualTo(MixWord(0x000420c4)).await // + 0 1 2 3 4
      nextState0.map(_.registers.getX) must beEqualTo(MixWord(0x45187209)).await // - 5 6 7 8 9
      // A = 2, I = 0, F = 0, C = 6 SLA
      val nextState1 = nextState0.flatMap(execute(_, MixWord(0x00080006)))
      nextState1.map(_.registers.getA) must beEqualTo(MixWord(0x020c4000)).await // + 2 3 4 0 0
      nextState1.map(_.registers.getX) must beEqualTo(MixWord(0x45187209)).await // - 5 6 7 8 9
      // A = 4, I = 0, F = 5, C = 6 SRC
      val nextState2 = nextState1.flatMap(execute(_, MixWord(0x00100146)))
      nextState2.map(_.registers.getA) must beEqualTo(MixWord(0x061c8242)).await // + 6 7 8 9 2
      nextState2.map(_.registers.getX) must beEqualTo(MixWord(0x43100005)).await // - 3 4 0 0 5
      // A = 2, I = 0, F = 1, C = 6 SRA
      val nextState3 = nextState2.flatMap(execute(_, MixWord(0x00080046)))
      nextState3.map(_.registers.getA) must beEqualTo(MixWord(0x000061c8)).await // + 0 0 6 7 8
      nextState3.map(_.registers.getX) must beEqualTo(MixWord(0x43100005)).await // - 3 4 0 0 5
      // A = 1, I = 0, F = 4, C = 6 SLC
      val nextState4 = nextState3.flatMap(execute(_, MixWord(0x00040106)))
      nextState4.map(_.registers.getA) must beEqualTo(MixWord(0x00187203)).await // + 0 6 7 8 3
      nextState4.map(_.registers.getX) must beEqualTo(MixWord(0x44000140)).await // - 4 0 0 5 0
    }
  }
}
