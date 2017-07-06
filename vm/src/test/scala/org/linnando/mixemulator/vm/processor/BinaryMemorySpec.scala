package org.linnando.mixemulator.vm.processor

import org.linnando.mixemulator.vm.BinaryVirtualMachine._
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.mutable.Specification

class BinaryMemorySpec extends Specification {
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(BinaryMixWord(0x061c8240))  // + 6 7 8 9 0
      .updatedX(BinaryMixWord(0x461c8240))  // - 6 7 8 9 0
      .updatedI(1, BinaryMixIndex(0x0240))  // + 9 0
      .updatedI(2, BinaryMixIndex(0x1240))  // - 9 0
      .updatedI(3, BinaryMixIndex(0x0240))  // + 9 0
      .updatedI(4, BinaryMixIndex(0x0240))  // + 9 0
      .updatedI(5, BinaryMixIndex(0x0240))  // + 9 0
      .updatedI(6, BinaryMixIndex(0x0240))  // + 9 0
      .updatedJ(BinaryMixIndex(0x0bb8)),
    memory = initialState.memory
      .updated(BinaryMixIndex(2000), BinaryMixWord(0x41403144)) // - 1 16 3 5 4
      .updated(BinaryMixIndex(2001), BinaryMixWord(0x41083105)) // - 1 2 3 4 5
  )

  "binary memory module" should {
    "load register A from memory" in {
      // A = 2000, I = 0, F = 0:5, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400148)).registers.getA must be equalTo BinaryMixWord(0x41403144)
      // A = 2000, I = 0, F = 1:5, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400348)).registers.getA must be equalTo BinaryMixWord(0x01403144)
      // A = 2000, I = 0, F = 3:5, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400748)).registers.getA must be equalTo BinaryMixWord(0x00003144)
      // A = 2000, I = 0, F = 0:3, C = 8 LDA
      execute(state, BinaryMixWord(0x1F4000c8)).registers.getA must be equalTo BinaryMixWord(0x40001403)
      // A = 2000, I = 0, F = 4:4, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400908)).registers.getA must be equalTo BinaryMixWord(0x00000005)
      // A = 2000, I = 0, F = 0:0, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400008)).registers.getA must be equalTo BinaryMixWord(0x40000000)
      // A = 2000, I = 0, F = 1:1, C = 8 LDA
      execute(state, BinaryMixWord(0x1F400248)).registers.getA must be equalTo BinaryMixWord(0x00000001)
    }

    "load register I1 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 9 LD1
      execute(state, BinaryMixWord(0x1F400089)).registers.getI(1) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 9 LD1
      execute(state, BinaryMixWord(0x1F400909)).registers.getI(1) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 9 LD1
      execute(state, BinaryMixWord(0x1F400009)).registers.getI(1) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 9 LD1
      execute(state, BinaryMixWord(0x1F400249)).registers.getI(1) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 9 LD1
      execute(state, BinaryMixWord(0x1F400149)) must throwAn[OverflowException]
    }

    "load register I2 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 10 LD2
      execute(state, BinaryMixWord(0x1F40008a)).registers.getI(2) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 10 LD2
      execute(state, BinaryMixWord(0x1F40090a)).registers.getI(2) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 10 LD2
      execute(state, BinaryMixWord(0x1F40000a)).registers.getI(2) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 10 LD2
      execute(state, BinaryMixWord(0x1F40024a)).registers.getI(2) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 10 LD2
      execute(state, BinaryMixWord(0x1F40014a)) must throwAn[OverflowException]
    }

    "load register I3 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 11 LD3
      execute(state, BinaryMixWord(0x1F40008b)).registers.getI(3) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 11 LD3
      execute(state, BinaryMixWord(0x1F40090b)).registers.getI(3) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 11 LD3
      execute(state, BinaryMixWord(0x1F40000b)).registers.getI(3) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 11 LD3
      execute(state, BinaryMixWord(0x1F40024b)).registers.getI(3) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 11 LD3
      execute(state, BinaryMixWord(0x1F40014b)) must throwAn[OverflowException]
    }

    "load register I4 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 12 LD4
      execute(state, BinaryMixWord(0x1F40008c)).registers.getI(4) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 12 LD4
      execute(state, BinaryMixWord(0x1F40090c)).registers.getI(4) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 12 LD4
      execute(state, BinaryMixWord(0x1F40000c)).registers.getI(4) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 12 LD4
      execute(state, BinaryMixWord(0x1F40024c)).registers.getI(4) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 12 LD4
      execute(state, BinaryMixWord(0x1F40014c)) must throwAn[OverflowException]
    }

    "load register I5 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 13 LD5
      execute(state, BinaryMixWord(0x1F40008d)).registers.getI(5) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 13 LD5
      execute(state, BinaryMixWord(0x1F40090d)).registers.getI(5) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 13 LD5
      execute(state, BinaryMixWord(0x1F40000d)).registers.getI(5) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 13 LD5
      execute(state, BinaryMixWord(0x1F40024d)).registers.getI(5) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 13 LD5
      execute(state, BinaryMixWord(0x1F40014d)) must throwAn[OverflowException]
    }

    "load register I6 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 14 LD6
      execute(state, BinaryMixWord(0x1F40008e)).registers.getI(6) must be equalTo BinaryMixIndex(0x1050)
      // A = 2000, I = 0, F = 4:4, C = 14 LD6
      execute(state, BinaryMixWord(0x1F40090e)).registers.getI(6) must be equalTo BinaryMixIndex(0x0005)
      // A = 2000, I = 0, F = 0:0, C = 14 LD6
      execute(state, BinaryMixWord(0x1F40000e)).registers.getI(6) must be equalTo BinaryMixIndex(0x1000)
      // A = 2000, I = 0, F = 1:1, C = 14 LD6
      execute(state, BinaryMixWord(0x1F40024e)).registers.getI(6) must be equalTo BinaryMixIndex(0x0001)
      // A = 2000, I = 0, F = 0:5, C = 14 LD6
      execute(state, BinaryMixWord(0x1F40014e)) must throwAn[OverflowException]
    }

    "load register X from memory" in {
      // A = 2000, I = 0, F = 0:5, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40014f)).registers.getX must be equalTo BinaryMixWord(0x41403144)
      // A = 2000, I = 0, F = 1:5, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40034f)).registers.getX must be equalTo BinaryMixWord(0x01403144)
      // A = 2000, I = 0, F = 3:5, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40074f)).registers.getX must be equalTo BinaryMixWord(0x00003144)
      // A = 2000, I = 0, F = 0:3, C = 15 LDX
      execute(state, BinaryMixWord(0x1F4000cf)).registers.getX must be equalTo BinaryMixWord(0x40001403)
      // A = 2000, I = 0, F = 4:4, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40090f)).registers.getX must be equalTo BinaryMixWord(0x00000005)
      // A = 2000, I = 0, F = 0:0, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40000f)).registers.getX must be equalTo BinaryMixWord(0x40000000)
      // A = 2000, I = 0, F = 1:1, C = 15 LDX
      execute(state, BinaryMixWord(0x1F40024f)).registers.getX must be equalTo BinaryMixWord(0x00000001)
    }

    "load register A from memory with negation" in {
      // A = 2000, I = 0, F = 0:5, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400150)).registers.getA must be equalTo BinaryMixWord(0x01403144)
      // A = 2000, I = 0, F = 1:5, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400350)).registers.getA must be equalTo BinaryMixWord(0x41403144)
      // A = 2000, I = 0, F = 3:5, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400750)).registers.getA must be equalTo BinaryMixWord(0x40003144)
      // A = 2000, I = 0, F = 0:3, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F4000d0)).registers.getA must be equalTo BinaryMixWord(0x00001403)
      // A = 2000, I = 0, F = 4:4, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400910)).registers.getA must be equalTo BinaryMixWord(0x40000005)
      // A = 2000, I = 0, F = 0:0, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400010)).registers.getA must be equalTo BinaryMixWord(0x00000000)
      // A = 2000, I = 0, F = 1:1, C = 16 LDAN
      execute(state, BinaryMixWord(0x1F400250)).registers.getA must be equalTo BinaryMixWord(0x40000001)
    }

    "load register I1 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 17 LD1N
      execute(state, BinaryMixWord(0x1F400091)).registers.getI(1) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 17 LD1N
      execute(state, BinaryMixWord(0x1F400911)).registers.getI(1) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 17 LD1N
      execute(state, BinaryMixWord(0x1F400011)).registers.getI(1) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 17 LD1N
      execute(state, BinaryMixWord(0x1F400251)).registers.getI(1) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 17 LD1N
      execute(state, BinaryMixWord(0x1F400151)) must throwAn[OverflowException]
    }

    "load register I2 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 18 LD2N
      execute(state, BinaryMixWord(0x1F400092)).registers.getI(2) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 18 LD2N
      execute(state, BinaryMixWord(0x1F400912)).registers.getI(2) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 18 LD2N
      execute(state, BinaryMixWord(0x1F400012)).registers.getI(2) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 18 LD2N
      execute(state, BinaryMixWord(0x1F400252)).registers.getI(2) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 18 LD2N
      execute(state, BinaryMixWord(0x1F400152)) must throwAn[OverflowException]
    }

    "load register I3 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 19 LD3N
      execute(state, BinaryMixWord(0x1F400093)).registers.getI(3) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 19 LD3N
      execute(state, BinaryMixWord(0x1F400913)).registers.getI(3) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 19 LD3N
      execute(state, BinaryMixWord(0x1F400013)).registers.getI(3) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 19 LD3N
      execute(state, BinaryMixWord(0x1F400253)).registers.getI(3) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 19 LD3N
      execute(state, BinaryMixWord(0x1F400153)) must throwAn[OverflowException]
    }

    "load register I4 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 20 LD4N
      execute(state, BinaryMixWord(0x1F400094)).registers.getI(4) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 20 LD4N
      execute(state, BinaryMixWord(0x1F400914)).registers.getI(4) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 20 LD4N
      execute(state, BinaryMixWord(0x1F400014)).registers.getI(4) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 20 LD4N
      execute(state, BinaryMixWord(0x1F400254)).registers.getI(4) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 20 LD4N
      execute(state, BinaryMixWord(0x1F400154)) must throwAn[OverflowException]
    }

    "load register I5 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 21 LD5N
      execute(state, BinaryMixWord(0x1F400095)).registers.getI(5) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 21 LD5N
      execute(state, BinaryMixWord(0x1F400915)).registers.getI(5) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 21 LD5N
      execute(state, BinaryMixWord(0x1F400015)).registers.getI(5) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 21 LD5N
      execute(state, BinaryMixWord(0x1F400255)).registers.getI(5) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 21 LD5N
      execute(state, BinaryMixWord(0x1F400155)) must throwAn[OverflowException]
    }

    "load register I6 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 22 LD6N
      execute(state, BinaryMixWord(0x1F400096)).registers.getI(6) must be equalTo BinaryMixIndex(0x0050)
      // A = 2000, I = 0, F = 4:4, C = 22 LD6N
      execute(state, BinaryMixWord(0x1F400916)).registers.getI(6) must be equalTo BinaryMixIndex(0x1005)
      // A = 2000, I = 0, F = 0:0, C = 22 LD6N
      execute(state, BinaryMixWord(0x1F400016)).registers.getI(6) must be equalTo BinaryMixIndex(0x0000)
      // A = 2000, I = 0, F = 1:1, C = 22 LD6N
      execute(state, BinaryMixWord(0x1F400256)).registers.getI(6) must be equalTo BinaryMixIndex(0x1001)
      // A = 2000, I = 0, F = 0:5, C = 22 LD6N
      execute(state, BinaryMixWord(0x1F400156)) must throwAn[OverflowException]
    }

    "load register X from memory with negation" in {
      // A = 2000, I = 0, F = 0:5, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400157)).registers.getX must be equalTo BinaryMixWord(0x01403144)
      // A = 2000, I = 0, F = 1:5, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400357)).registers.getX must be equalTo BinaryMixWord(0x41403144)
      // A = 2000, I = 0, F = 3:5, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400757)).registers.getX must be equalTo BinaryMixWord(0x40003144)
      // A = 2000, I = 0, F = 0:3, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F4000d7)).registers.getX must be equalTo BinaryMixWord(0x00001403)
      // A = 2000, I = 0, F = 4:4, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400917)).registers.getX must be equalTo BinaryMixWord(0x40000005)
      // A = 2000, I = 0, F = 0:0, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400017)).registers.getX must be equalTo BinaryMixWord(0x00000000)
      // A = 2000, I = 0, F = 1:1, C = 23 LDXN
      execute(state, BinaryMixWord(0x1F400257)).registers.getX must be equalTo BinaryMixWord(0x40000001)
    }

    "store register A to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 24 STA
      execute(state, BinaryMixWord(0x1F440158)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x061c8240)
      // A = 2001, I = 0, F = 1:5, C = 24 STA
      execute(state, BinaryMixWord(0x1F440358)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x461c8240)
      // A = 2001, I = 0, F = 5:5, C = 24 STA
      execute(state, BinaryMixWord(0x1F440b58)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 24 STA
      execute(state, BinaryMixWord(0x1F440498)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 24 STA
      execute(state, BinaryMixWord(0x1F4404d8)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 24 STA
      execute(state, BinaryMixWord(0x1F440058)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register I1 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 25 ST1
      execute(state, BinaryMixWord(0x1F440159)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000240)
      // A = 2001, I = 0, F = 1:5, C = 25 ST1
      execute(state, BinaryMixWord(0x1F440359)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 25 ST1
      execute(state, BinaryMixWord(0x1F440b59)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 25 ST1
      execute(state, BinaryMixWord(0x1F440499)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 25 ST1
      execute(state, BinaryMixWord(0x1F4404d9)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 25 ST1
      execute(state, BinaryMixWord(0x1F440059)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register I2 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 26 ST2
      execute(state, BinaryMixWord(0x1F44015a)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 1:5, C = 26 ST2
      execute(state, BinaryMixWord(0x1F44035a)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 26 ST2
      execute(state, BinaryMixWord(0x1F440b5a)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 26 ST2
      execute(state, BinaryMixWord(0x1F44049a)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 26 ST2
      execute(state, BinaryMixWord(0x1F4404da)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 26 ST2
      execute(state, BinaryMixWord(0x1F44005a)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40083105)
    }

    "store register I3 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 27 ST3
      execute(state, BinaryMixWord(0x1F44015b)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000240)
      // A = 2001, I = 0, F = 1:5, C = 27 ST3
      execute(state, BinaryMixWord(0x1F44035b)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 27 ST3
      execute(state, BinaryMixWord(0x1F440b5b)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 27 ST3
      execute(state, BinaryMixWord(0x1F44049b)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 27 ST3
      execute(state, BinaryMixWord(0x1F4404db)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 27 ST3
      execute(state, BinaryMixWord(0x1F44005b)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register I4 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 28 ST4
      execute(state, BinaryMixWord(0x1F44015c)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000240)
      // A = 2001, I = 0, F = 1:5, C = 28 ST4
      execute(state, BinaryMixWord(0x1F44035c)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 28 ST4
      execute(state, BinaryMixWord(0x1F440b5c)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 28 ST4
      execute(state, BinaryMixWord(0x1F44049c)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 28 ST4
      execute(state, BinaryMixWord(0x1F4404dc)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 28 ST4
      execute(state, BinaryMixWord(0x1F44005c)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register I5 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 29 ST5
      execute(state, BinaryMixWord(0x1F44015d)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000240)
      // A = 2001, I = 0, F = 1:5, C = 29 ST5
      execute(state, BinaryMixWord(0x1F44035d)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 29 ST5
      execute(state, BinaryMixWord(0x1F440b5d)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 29 ST5
      execute(state, BinaryMixWord(0x1F44049d)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 29 ST5
      execute(state, BinaryMixWord(0x1F4404dd)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 29 ST5
      execute(state, BinaryMixWord(0x1F44005d)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register I6 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 30 ST6
      execute(state, BinaryMixWord(0x1F44015e)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000240)
      // A = 2001, I = 0, F = 1:5, C = 30 ST6
      execute(state, BinaryMixWord(0x1F44035e)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000240)
      // A = 2001, I = 0, F = 5:5, C = 30 ST6
      execute(state, BinaryMixWord(0x1F440b5e)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 30 ST6
      execute(state, BinaryMixWord(0x1F44049e)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 30 ST6
      execute(state, BinaryMixWord(0x1F4404de)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 30 ST6
      execute(state, BinaryMixWord(0x1F44005e)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "store register X to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 31 STX
      execute(state, BinaryMixWord(0x1F44015f)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x461c8240)
      // A = 2001, I = 0, F = 1:5, C = 31 STX
      execute(state, BinaryMixWord(0x1F44035f)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x461c8240)
      // A = 2001, I = 0, F = 5:5, C = 31 STX
      execute(state, BinaryMixWord(0x1F440b5f)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 31 STX
      execute(state, BinaryMixWord(0x1F44049f)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 31 STX
      execute(state, BinaryMixWord(0x1F4404df)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41240105)
      // A = 2001, I = 0, F = 0:1, C = 31 STX
      execute(state, BinaryMixWord(0x1F44005f)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40083105)
    }

    "store register J to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 32 STJ
      execute(state, BinaryMixWord(0x1F440160)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000bb8)
      // A = 2001, I = 0, F = 1:5, C = 32 STJ
      execute(state, BinaryMixWord(0x1F440360)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000bb8)
      // A = 2001, I = 0, F = 5:5, C = 32 STJ
      execute(state, BinaryMixWord(0x1F440b60)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083138)
      // A = 2001, I = 0, F = 2:2, C = 32 STJ
      execute(state, BinaryMixWord(0x1F4404a0)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41e03105)
      // A = 2001, I = 0, F = 2:3, C = 32 STJ
      execute(state, BinaryMixWord(0x1F4404e0)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41bb8105)
      // A = 2001, I = 0, F = 0:1, C = 32 STJ
      execute(state, BinaryMixWord(0x1F440060)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x38083105)
    }

    "store zero to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 33 STZ
      execute(state, BinaryMixWord(0x1F440161)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000000)
      // A = 2001, I = 0, F = 1:5, C = 33 STZ
      execute(state, BinaryMixWord(0x1F440361)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x40000000)
      // A = 2001, I = 0, F = 5:5, C = 33 STZ
      execute(state, BinaryMixWord(0x1F440b61)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41083100)
      // A = 2001, I = 0, F = 2:2, C = 33 STZ
      execute(state, BinaryMixWord(0x1F4404a1)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41003105)
      // A = 2001, I = 0, F = 2:3, C = 33 STZ
      execute(state, BinaryMixWord(0x1F4404e1)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x41000105)
      // A = 2001, I = 0, F = 0:1, C = 33 STZ
      execute(state, BinaryMixWord(0x1F440061)).memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00083105)
    }

    "move memory words" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, BinaryMixIndex(3000)),
        memory = initialState.memory
          .updated(BinaryMixIndex(2000), BinaryMixWord(0x00000001))
          .updated(BinaryMixIndex(2001), BinaryMixWord(0x00000002))
          .updated(BinaryMixIndex(2002), BinaryMixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, BinaryMixWord(0x1f4000c7))
      nextState.memory.get(BinaryMixIndex(3000)) must be equalTo BinaryMixWord(0x00000001)
      nextState.memory.get(BinaryMixIndex(3001)) must be equalTo BinaryMixWord(0x00000002)
      nextState.memory.get(BinaryMixIndex(3002)) must be equalTo BinaryMixWord(0x00000003)
    }

    "do nothing when the number of words to move is zero" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, BinaryMixIndex(3000)),
        memory = initialState.memory
          .updated(BinaryMixIndex(2000), BinaryMixWord(0x00000001))
          .updated(BinaryMixIndex(2001), BinaryMixWord(0x00000002))
          .updated(BinaryMixIndex(2002), BinaryMixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 0, C = 7 MOVE
      val nextState = execute(prevState, BinaryMixWord(0x1f400007))
      nextState.memory.get(BinaryMixIndex(3000)) must be equalTo BinaryMixWord(0x00000000)
      nextState.memory.get(BinaryMixIndex(3001)) must be equalTo BinaryMixWord(0x00000000)
      nextState.memory.get(BinaryMixIndex(3002)) must be equalTo BinaryMixWord(0x00000000)
    }

    "move overlapping ranges in the downward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, BinaryMixIndex(1999)),
        memory = initialState.memory
          .updated(BinaryMixIndex(2000), BinaryMixWord(0x00000001))
          .updated(BinaryMixIndex(2001), BinaryMixWord(0x00000002))
          .updated(BinaryMixIndex(2002), BinaryMixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, BinaryMixWord(0x1f4000c7))
      nextState.memory.get(BinaryMixIndex(1999)) must be equalTo BinaryMixWord(0x00000001)
      nextState.memory.get(BinaryMixIndex(2000)) must be equalTo BinaryMixWord(0x00000002)
      nextState.memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000003)
      nextState.memory.get(BinaryMixIndex(2002)) must be equalTo BinaryMixWord(0x00000003)
    }

    "move overlapping ranges in the upward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, BinaryMixIndex(2001)),
        memory = initialState.memory
          .updated(BinaryMixIndex(2000), BinaryMixWord(0x00000001))
          .updated(BinaryMixIndex(2001), BinaryMixWord(0x00000002))
          .updated(BinaryMixIndex(2002), BinaryMixWord(0x00000003))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, BinaryMixWord(0x1f4000c7))
      nextState.memory.get(BinaryMixIndex(2000)) must be equalTo BinaryMixWord(0x00000001)
      nextState.memory.get(BinaryMixIndex(2001)) must be equalTo BinaryMixWord(0x00000001)
      nextState.memory.get(BinaryMixIndex(2002)) must be equalTo BinaryMixWord(0x00000001)
      nextState.memory.get(BinaryMixIndex(2003)) must be equalTo BinaryMixWord(0x00000001)
    }
  }
}
