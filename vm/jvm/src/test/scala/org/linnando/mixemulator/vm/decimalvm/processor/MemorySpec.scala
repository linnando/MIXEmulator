package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.exceptions.OverflowException
import org.specs2.mutable.Specification

class MemorySpec extends Specification {
  import decimal._
  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(607080900L)) // + 6 7 8 9 0
      .updatedX(MixWord(0x400000000L | 607080900L)) // - 6 7 8 9 0
      .updatedI(1, MixIndex(900)) // + 9 0
      .updatedI(2, MixIndex((0x4000 | 900).toShort)) // - 9 0
      .updatedI(3, MixIndex(900)) // + 9 0
      .updatedI(4, MixIndex(900)) // + 9 0
      .updatedI(5, MixIndex(900)) // + 9 0
      .updatedI(6, MixIndex(900)) // + 9 0
      .updatedJ(MixIndex(3001)),
    memory = initialState.memory
      .updated(MixIndex(2000), MixWord(0x400000000L | 116030504L)) // - 1 16 3 5 4
      .updated(MixIndex(2001), MixWord(0x400000000L | 102030405L)) // - 1 2 3 4 5
  )

  "decimal loading from memory" should {
    "load register A from memory" in {
      // A = 2000, I = 0, F = 0:5, C = 8 LDA
      execute(state, MixWord(2000000508L)).registers.getA must be equalTo MixWord(0x400000000L | 116030504L)
      // A = 2000, I = 0, F = 1:5, C = 8 LDA
      execute(state, MixWord(2000001308L)).registers.getA must be equalTo MixWord(116030504L)
      // A = 2000, I = 0, F = 3:5, C = 8 LDA
      execute(state, MixWord(2000002908L)).registers.getA must be equalTo MixWord(30504L)
      // A = 2000, I = 0, F = 0:3, C = 8 LDA
      execute(state, MixWord(2000000308L)).registers.getA must be equalTo MixWord(0x400000000L | 11603L)
      // A = 2000, I = 0, F = 4:4, C = 8 LDA
      execute(state, MixWord(2000003608L)).registers.getA must be equalTo MixWord(5L)
      // A = 2000, I = 0, F = 0:0, C = 8 LDA
      execute(state, MixWord(2000000008L)).registers.getA must be equalTo MixWord(0x400000000L)
      // A = 2000, I = 0, F = 1:1, C = 8 LDA
      execute(state, MixWord(2000000908L)).registers.getA must be equalTo MixWord(1L)
    }

    "load register I1 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 9 LD1
      execute(state, MixWord(2000000209L)).registers.getI(1) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 9 LD1
      execute(state, MixWord(2000003609L)).registers.getI(1) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 9 LD1
      execute(state, MixWord(2000000009L)).registers.getI(1) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 9 LD1
      execute(state, MixWord(2000000909L)).registers.getI(1) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 9 LD1
      execute(state, MixWord(2000000509L)) must throwAn[OverflowException]
    }

    "load register I2 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 10 LD2
      execute(state, MixWord(2000000210L)).registers.getI(2) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 10 LD2
      execute(state, MixWord(2000003610L)).registers.getI(2) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 10 LD2
      execute(state, MixWord(2000000010L)).registers.getI(2) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 10 LD2
      execute(state, MixWord(2000000910L)).registers.getI(2) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 10 LD2
      execute(state, MixWord(2000000510L)) must throwAn[OverflowException]
    }

    "load register I3 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 11 LD3
      execute(state, MixWord(2000000211L)).registers.getI(3) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 11 LD3
      execute(state, MixWord(2000003611L)).registers.getI(3) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 11 LD3
      execute(state, MixWord(2000000011L)).registers.getI(3) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 11 LD3
      execute(state, MixWord(2000000911L)).registers.getI(3) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 11 LD3
      execute(state, MixWord(2000000511L)) must throwAn[OverflowException]
    }

    "load register I4 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 12 LD4
      execute(state, MixWord(2000000212L)).registers.getI(4) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 12 LD4
      execute(state, MixWord(2000003612L)).registers.getI(4) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 12 LD4
      execute(state, MixWord(2000000012L)).registers.getI(4) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 12 LD4
      execute(state, MixWord(2000000912L)).registers.getI(4) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 12 LD4
      execute(state, MixWord(2000000512L)) must throwAn[OverflowException]
    }

    "load register I5 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 13 LD5
      execute(state, MixWord(2000000213L)).registers.getI(5) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 13 LD5
      execute(state, MixWord(2000003613L)).registers.getI(5) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 13 LD5
      execute(state, MixWord(2000000013L)).registers.getI(5) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 13 LD5
      execute(state, MixWord(2000000913L)).registers.getI(5) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 13 LD5
      execute(state, MixWord(2000000513L)) must throwAn[OverflowException]
    }

    "load register I6 from memory" in {
      // A = 2000, I = 0, F = 0:2, C = 14 LD6
      execute(state, MixWord(2000000214L)).registers.getI(6) must be equalTo MixIndex((0x4000 | 116).toShort)
      // A = 2000, I = 0, F = 4:4, C = 14 LD6
      execute(state, MixWord(2000003614L)).registers.getI(6) must be equalTo MixIndex(5)
      // A = 2000, I = 0, F = 0:0, C = 14 LD6
      execute(state, MixWord(2000000014L)).registers.getI(6) must be equalTo MixIndex(0x4000)
      // A = 2000, I = 0, F = 1:1, C = 14 LD6
      execute(state, MixWord(2000000914L)).registers.getI(6) must be equalTo MixIndex(1)
      // A = 2000, I = 0, F = 0:5, C = 14 LD6
      execute(state, MixWord(2000000514L)) must throwAn[OverflowException]
    }

    "load register X from memory" in {
      // A = 2000, I = 0, F = 0:5, C = 15 LDX
      execute(state, MixWord(2000000515L)).registers.getX must be equalTo MixWord(0x400000000L | 116030504L)
      // A = 2000, I = 0, F = 1:5, C = 15 LDX
      execute(state, MixWord(2000001315L)).registers.getX must be equalTo MixWord(116030504L)
      // A = 2000, I = 0, F = 3:5, C = 15 LDX
      execute(state, MixWord(2000002915L)).registers.getX must be equalTo MixWord(30504L)
      // A = 2000, I = 0, F = 0:3, C = 15 LDX
      execute(state, MixWord(2000000315L)).registers.getX must be equalTo MixWord(0x400000000L | 11603L)
      // A = 2000, I = 0, F = 4:4, C = 15 LDX
      execute(state, MixWord(2000003615L)).registers.getX must be equalTo MixWord(5L)
      // A = 2000, I = 0, F = 0:0, C = 15 LDX
      execute(state, MixWord(2000000015L)).registers.getX must be equalTo MixWord(0x400000000L)
      // A = 2000, I = 0, F = 1:1, C = 15 LDX
      execute(state, MixWord(2000000915L)).registers.getX must be equalTo MixWord(1)
    }

    "load register A from memory with negation" in {
      // A = 2000, I = 0, F = 0:5, C = 16 LDAN
      execute(state, MixWord(2000000516L)).registers.getA must be equalTo MixWord(116030504L)
      // A = 2000, I = 0, F = 1:5, C = 16 LDAN
      execute(state, MixWord(2000001316L)).registers.getA must be equalTo MixWord(0x400000000L | 116030504L)
      // A = 2000, I = 0, F = 3:5, C = 16 LDAN
      execute(state, MixWord(2000002916L)).registers.getA must be equalTo MixWord(0x400000000L | 30504L)
      // A = 2000, I = 0, F = 0:3, C = 16 LDAN
      execute(state, MixWord(2000000316L)).registers.getA must be equalTo MixWord(11603L)
      // A = 2000, I = 0, F = 4:4, C = 16 LDAN
      execute(state, MixWord(2000003616L)).registers.getA must be equalTo MixWord(0x400000005L)
      // A = 2000, I = 0, F = 0:0, C = 16 LDAN
      execute(state, MixWord(2000000016L)).registers.getA must be equalTo MixWord(0L)
      // A = 2000, I = 0, F = 1:1, C = 16 LDAN
      execute(state, MixWord(2000000916L)).registers.getA must be equalTo MixWord(0x400000001L)
    }

    "load register I1 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 17 LD1N
      execute(state, MixWord(2000000217L)).registers.getI(1) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 17 LD1N
      execute(state, MixWord(2000003617L)).registers.getI(1) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 17 LD1N
      execute(state, MixWord(2000000017L)).registers.getI(1) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 17 LD1N
      execute(state, MixWord(2000000917L)).registers.getI(1) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 17 LD1N
      execute(state, MixWord(2000000517L)) must throwAn[OverflowException]
    }

    "load register I2 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 18 LD2N
      execute(state, MixWord(2000000218L)).registers.getI(2) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 18 LD2N
      execute(state, MixWord(2000003618L)).registers.getI(2) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 18 LD2N
      execute(state, MixWord(2000000018L)).registers.getI(2) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 18 LD2N
      execute(state, MixWord(2000000918L)).registers.getI(2) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 18 LD2N
      execute(state, MixWord(2000000518L)) must throwAn[OverflowException]
    }

    "load register I3 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 19 LD3N
      execute(state, MixWord(2000000219L)).registers.getI(3) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 19 LD3N
      execute(state, MixWord(2000003619L)).registers.getI(3) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 19 LD3N
      execute(state, MixWord(2000000019L)).registers.getI(3) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 19 LD3N
      execute(state, MixWord(2000000919L)).registers.getI(3) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 19 LD3N
      execute(state, MixWord(2000000519L)) must throwAn[OverflowException]
    }

    "load register I4 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 20 LD4N
      execute(state, MixWord(2000000220L)).registers.getI(4) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 20 LD4N
      execute(state, MixWord(2000003620L)).registers.getI(4) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 20 LD4N
      execute(state, MixWord(2000000020L)).registers.getI(4) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 20 LD4N
      execute(state, MixWord(2000000920L)).registers.getI(4) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 20 LD4N
      execute(state, MixWord(2000000520L)) must throwAn[OverflowException]
    }

    "load register I5 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 21 LD5N
      execute(state, MixWord(2000000221L)).registers.getI(5) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 21 LD5N
      execute(state, MixWord(2000003621L)).registers.getI(5) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 21 LD5N
      execute(state, MixWord(2000000021L)).registers.getI(5) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 21 LD5N
      execute(state, MixWord(2000000921L)).registers.getI(5) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 21 LD5N
      execute(state, MixWord(2000000521L)) must throwAn[OverflowException]
    }

    "load register I6 from memory with negation" in {
      // A = 2000, I = 0, F = 0:2, C = 22 LD6N
      execute(state, MixWord(2000000222L)).registers.getI(6) must be equalTo MixIndex(116)
      // A = 2000, I = 0, F = 4:4, C = 22 LD6N
      execute(state, MixWord(2000003622L)).registers.getI(6) must be equalTo MixIndex(0x4005)
      // A = 2000, I = 0, F = 0:0, C = 22 LD6N
      execute(state, MixWord(2000000022L)).registers.getI(6) must be equalTo MixIndex(0)
      // A = 2000, I = 0, F = 1:1, C = 22 LD6N
      execute(state, MixWord(2000000922L)).registers.getI(6) must be equalTo MixIndex(0x4001)
      // A = 2000, I = 0, F = 0:5, C = 22 LD6N
      execute(state, MixWord(2000000522L)) must throwAn[OverflowException]
    }

    "load register X from memory with negation" in {
      // A = 2000, I = 0, F = 0:5, C = 23 LDXN
      execute(state, MixWord(2000000523L)).registers.getX must be equalTo MixWord(116030504L)
      // A = 2000, I = 0, F = 1:5, C = 23 LDXN
      execute(state, MixWord(2000001323L)).registers.getX must be equalTo MixWord(0x400000000L | 116030504L)
      // A = 2000, I = 0, F = 3:5, C = 23 LDXN
      execute(state, MixWord(2000002923L)).registers.getX must be equalTo MixWord(0x400000000L | 30504L)
      // A = 2000, I = 0, F = 0:3, C = 23 LDXN
      execute(state, MixWord(2000000323L)).registers.getX must be equalTo MixWord(11603L)
      // A = 2000, I = 0, F = 4:4, C = 23 LDXN
      execute(state, MixWord(2000003623L)).registers.getX must be equalTo MixWord(0x400000005L)
      // A = 2000, I = 0, F = 0:0, C = 23 LDXN
      execute(state, MixWord(2000000023L)).registers.getX must be equalTo MixWord(0L)
      // A = 2000, I = 0, F = 1:1, C = 23 LDXN
      execute(state, MixWord(2000000923L)).registers.getX must be equalTo MixWord(0x400000001L)
    }
  }

  "decimal storing to memory" should {
    "store register A to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 24 STA
      execute(state, MixWord(2001000524L)).memory.get(2001.toShort) must be equalTo MixWord(607080900L)
      // A = 2001, I = 0, F = 1:5, C = 24 STA
      execute(state, MixWord(2001001324L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 607080900L)
      // A = 2001, I = 0, F = 5:5, C = 24 STA
      execute(state, MixWord(2001004524L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 24 STA
      execute(state, MixWord(2001001824L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 24 STA
      execute(state, MixWord(2001001924L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 24 STA
      execute(state, MixWord(2001000124L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register I1 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 25 ST1
      execute(state, MixWord(2001000525L)).memory.get(2001.toShort) must be equalTo MixWord(900L)
      // A = 2001, I = 0, F = 1:5, C = 25 ST1
      execute(state, MixWord(2001001325L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 25 ST1
      execute(state, MixWord(2001004525L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 25 ST1
      execute(state, MixWord(2001001825L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 25 ST1
      execute(state, MixWord(2001001925L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 25 ST1
      execute(state, MixWord(2001000125L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register I2 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 26 ST2
      execute(state, MixWord(2001000526L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 1:5, C = 26 ST2
      execute(state, MixWord(2001001326L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 26 ST2
      execute(state, MixWord(2001004526L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 26 ST2
      execute(state, MixWord(2001001826L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 26 ST2
      execute(state, MixWord(2001001926L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 26 ST2
      execute(state, MixWord(2001000126L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 2030405L)
    }

    "store register I3 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 27 ST3
      execute(state, MixWord(2001000527L)).memory.get(2001.toShort) must be equalTo MixWord(900L)
      // A = 2001, I = 0, F = 1:5, C = 27 ST3
      execute(state, MixWord(2001001327L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 27 ST3
      execute(state, MixWord(2001004527L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 27 ST3
      execute(state, MixWord(2001001827L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 27 ST3
      execute(state, MixWord(2001001927L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 27 ST3
      execute(state, MixWord(2001000127L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register I4 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 28 ST4
      execute(state, MixWord(2001000527L)).memory.get(2001.toShort) must be equalTo MixWord(900L)
      // A = 2001, I = 0, F = 1:5, C = 28 ST4
      execute(state, MixWord(2001001328L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 28 ST4
      execute(state, MixWord(2001004528L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 28 ST4
      execute(state, MixWord(2001001828L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 28 ST4
      execute(state, MixWord(2001001928L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 28 ST4
      execute(state, MixWord(2001000128L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register I5 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 29 ST5
      execute(state, MixWord(2001000529L)).memory.get(2001.toShort) must be equalTo MixWord(900L)
      // A = 2001, I = 0, F = 1:5, C = 29 ST5
      execute(state, MixWord(2001001329L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 29 ST5
      execute(state, MixWord(2001004529L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 29 ST5
      execute(state, MixWord(2001001829L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 29 ST5
      execute(state, MixWord(2001001929L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 29 ST5
      execute(state, MixWord(2001000129L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register I6 to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 30 ST6
      execute(state, MixWord(2001000530L)).memory.get(2001.toShort) must be equalTo MixWord(900L)
      // A = 2001, I = 0, F = 1:5, C = 30 ST6
      execute(state, MixWord(2001001330L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 900L)
      // A = 2001, I = 0, F = 5:5, C = 30 ST6
      execute(state, MixWord(2001004530L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 30 ST6
      execute(state, MixWord(2001001830L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 30 ST6
      execute(state, MixWord(2001001930L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 30 ST6
      execute(state, MixWord(2001000130L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }

    "store register X to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 31 STX
      execute(state, MixWord(2001000531L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 607080900L)
      // A = 2001, I = 0, F = 1:5, C = 31 STX
      execute(state, MixWord(2001001331L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 607080900L)
      // A = 2001, I = 0, F = 5:5, C = 31 STX
      execute(state, MixWord(2001004531L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 31 STX
      execute(state, MixWord(2001001831L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 31 STX
      execute(state, MixWord(2001001931L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 109000405L)
      // A = 2001, I = 0, F = 0:1, C = 31 STX
      execute(state, MixWord(2001000131L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 2030405L)
    }

    "store register J to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 32 STJ
      execute(state, MixWord(2001000532L)).memory.get(2001.toShort) must be equalTo MixWord(3001L)
      // A = 2001, I = 0, F = 1:5, C = 32 STJ
      execute(state, MixWord(2001001332L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 3001L)
      // A = 2001, I = 0, F = 5:5, C = 32 STJ
      execute(state, MixWord(2001004532L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030401L)
      // A = 2001, I = 0, F = 2:2, C = 32 STJ
      execute(state, MixWord(2001001832L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 101030405L)
      // A = 2001, I = 0, F = 2:3, C = 32 STJ
      execute(state, MixWord(2001001932L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 130010405L)
      // A = 2001, I = 0, F = 0:1, C = 32 STJ
      execute(state, MixWord(2001000132L)).memory.get(2001.toShort) must be equalTo MixWord(102030405L)
    }

    "store zero to memory" in {
      // A = 2001, I = 0, F = 0:5, C = 33 STZ
      execute(state, MixWord(2001000533L)).memory.get(2001.toShort) must be equalTo MixWord(0L)
      // A = 2001, I = 0, F = 1:5, C = 33 STZ
      execute(state, MixWord(2001001333L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L)
      // A = 2001, I = 0, F = 5:5, C = 33 STZ
      execute(state, MixWord(2001004533L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 102030400L)
      // A = 2001, I = 0, F = 2:2, C = 33 STZ
      execute(state, MixWord(2001001833L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100030405L)
      // A = 2001, I = 0, F = 2:3, C = 33 STZ
      execute(state, MixWord(2001001933L)).memory.get(2001.toShort) must be equalTo MixWord(0x400000000L | 100000405L)
      // A = 2001, I = 0, F = 0:1, C = 33 STZ
      execute(state, MixWord(2001000133L)).memory.get(2001.toShort) must be equalTo MixWord(2030405L)
    }
  }

  "decimal moving in memory" should {
    "move memory words" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, MixWord(2000000307L))
      nextState.memory.get(3000.toShort) must be equalTo MixWord(1L)
      nextState.memory.get(3001.toShort) must be equalTo MixWord(2L)
      nextState.memory.get(3002.toShort) must be equalTo MixWord(3L)
    }

    "do nothing when the number of words to move is zero" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(3000)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 0, C = 7 MOVE
      val nextState = execute(prevState, MixWord(2000000007L))
      nextState.memory.get(3000.toShort) must be equalTo MixWord(0L)
      nextState.memory.get(3001.toShort) must be equalTo MixWord(0L)
      nextState.memory.get(3002.toShort) must be equalTo MixWord(0L)
    }

    "move overlapping ranges in the downward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(1999)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, MixWord(2000000307L))
      nextState.memory.get(1999.toShort) must be equalTo MixWord(1L)
      nextState.memory.get(2000.toShort) must be equalTo MixWord(2L)
      nextState.memory.get(2001.toShort) must be equalTo MixWord(3L)
      nextState.memory.get(2002.toShort) must be equalTo MixWord(3L)
    }

    "move overlapping ranges in the upward direction" in {
      val prevState = initialState.copy(
        registers = initialState.registers.updatedI(1, MixIndex(2001)),
        memory = initialState.memory
          .updated(MixIndex(2000), MixWord(1L))
          .updated(MixIndex(2001), MixWord(2L))
          .updated(MixIndex(2002), MixWord(3L))
      )
      // A = 2000, I = 0, F = 3, C = 7 MOVE
      val nextState = execute(prevState, MixWord(2000000307L))
      nextState.memory.get(2000.toShort) must be equalTo MixWord(1L)
      nextState.memory.get(2001.toShort) must be equalTo MixWord(1L)
      nextState.memory.get(2002.toShort) must be equalTo MixWord(1L)
      nextState.memory.get(2003.toShort) must be equalTo MixWord(1L)
    }
  }
}
