package org.linnando.mixemulator.vm.decimalvm.processor

import org.linnando.mixemulator.vm.{Comparison, decimal}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ComparisonSpec(implicit ee: ExecutionEnv) extends Specification {

  import decimal._

  private val initialState = decimal.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x400000000L | 1918000009L)) // - 19 18 0 0 9
      .updatedX(MixWord(23032L)) // + 0 0 2 30 32
      .updatedI(1, MixIndex((0x4000 | 5937).toShort)) // - 59 37
      .updatedI(2, MixIndex(319)) // + 3 19
      .updatedI(3, MixIndex(3042)) // + 30 42
      .updatedI(4, MixIndex((0x4000 | 5926).toShort)) // - 59 26
      .updatedI(5, MixIndex(857)) // + 8 57
      .updatedI(6, MixIndex((0x4000 | 1246).toShort)), // - 12 46
    memory = initialState.memory
      .updated(MixIndex(1000), MixWord(0x400000000L | 22200L)) // - 0 0 2 22 0
  )

  "decimal comparison module" should {
    "compare registers to memory with sign" in {
      // A = 1000, I = 0, F = 0:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000000556L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000000557L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000000558L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000000559L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000000560L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000000561L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000000562L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000000563L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare registers to memory without sign" in {
      // A = 1000, I = 0, F = 1:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000001356L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000001357L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000001358L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000001359L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000001360L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000001361L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000001362L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000001363L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare partial fields with sign" in {
      // A = 1000, I = 0, F = 0:3, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000000356L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:3, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000000357L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000000358L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000000359L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000000360L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000000361L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000000362L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000000363L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare left-side partial fields without sign" in {
      // A = 1000, I = 0, F = 1:3, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000001156L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:3, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000001157L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000001158L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000001159L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000001160L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000001161L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000001162L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000001163L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "compare right-side partial fields without sign" in {
      // A = 1000, I = 0, F = 4:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000003756L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000003757L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000003758L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000003759L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000003760L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000003761L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000003762L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000003763L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare register field to zero with sign" in {
      // A = 1000, I = 0, F = 0:2, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000000256L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:2, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000000257L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000000258L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000000259L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000000260L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000000261L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000000262L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000000263L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "compare register field to zero without sign" in {
      // A = 1000, I = 0, F = 1:2, C = 56 CMPA
      val nextStateA = execute(state, MixWord(1000001056L))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:2, C = 57 CMP1
      val nextState1 = execute(state, MixWord(1000001057L))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 58 CMP2
      val nextState2 = execute(state, MixWord(1000001058L))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 59 CMP3
      val nextState3 = execute(state, MixWord(1000001059L))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 60 CMP4
      val nextState4 = execute(state, MixWord(1000001060L))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 61 CMP5
      val nextState5 = execute(state, MixWord(1000001061L))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 62 CMP6
      val nextState6 = execute(state, MixWord(1000001062L))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 63 CMPX
      val nextStateX = execute(state, MixWord(1000001063L))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }
  }
}
