package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.{Comparison, binary}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class ComparisonSpec(implicit ee: ExecutionEnv) extends Specification {
  import binary._
  private val initialState = binary.initialState
  private val state = initialState.copy(
    registers = initialState.registers
      .updatedA(MixWord(0x53480009))  // - 19 18 0 0 9
      .updatedX(MixWord(0x000027a0))  // + 0 0 2 30 32
      .updatedI(1, MixIndex(0x1ee5))  // - 59 37
      .updatedI(2, MixIndex(0x00d3))  // + 3 19
      .updatedI(3, MixIndex(0x07aa))  // + 30 42
      .updatedI(4, MixIndex(0x1eda))  // - 59 26
      .updatedI(5, MixIndex(0x0239))  // + 8 57
      .updatedI(6, MixIndex(0x132e)), // - 12 46
    memory = initialState.memory
      .updated(MixIndex(1000), MixWord(0x40002580)) // - 0 0 2 22 0
  )

  "binary comparison module" should {
    "compare registers to memory with sign" in {
      // A = 1000, I = 0, F = 0:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa00178))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa00179))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa0017a))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa0017b))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa0017c))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa0017d))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa0017e))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa0017f))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare registers to memory without sign" in {
      // A = 1000, I = 0, F = 1:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa00378))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa00379))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa0037a))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa0037b))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa0037c))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa0037d))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa0037e))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa0037f))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare partial fields with sign" in {
      // A = 1000, I = 0, F = 0:3, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa000f8))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:3, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa000f9))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa000fa))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa000fb))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa000fc))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa000fd))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa000fe))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 0:3, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa000ff))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare left-side partial fields without sign" in {
      // A = 1000, I = 0, F = 1:3, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa002f8))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:3, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa002f9))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa002fa))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa002fb))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa002fc))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa002fd))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa002fe))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 1:3, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa002ff))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "compare right-side partial fields without sign" in {
      // A = 1000, I = 0, F = 4:5, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa00978))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa00979))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa0097a))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa0097b))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa0097c))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 4:5, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa0097d))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa0097e))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 4:5, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa0097f))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
    }

    "compare register field to zero with sign" in {
      // A = 1000, I = 0, F = 0:2, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa000b8))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.LESS).await
      // A = 1000, I = 0, F = 0:2, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa000b9))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa000ba))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa000bb))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa000bc))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa000bd))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa000be))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 0:2, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa000bf))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }

    "compare register field to zero without sign" in {
      // A = 1000, I = 0, F = 1:2, C = 56 CMPA
      val nextStateA = execute(state, MixWord(0x0fa002b8))
      nextStateA.map(_.registers.getCMP) must beEqualTo(Comparison.GREATER).await
      // A = 1000, I = 0, F = 1:2, C = 57 CMP1
      val nextState1 = execute(state, MixWord(0x0fa002b9))
      nextState1.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 58 CMP2
      val nextState2 = execute(state, MixWord(0x0fa002ba))
      nextState2.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 59 CMP3
      val nextState3 = execute(state, MixWord(0x0fa002bb))
      nextState3.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 60 CMP4
      val nextState4 = execute(state, MixWord(0x0fa002bc))
      nextState4.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 61 CMP5
      val nextState5 = execute(state, MixWord(0x0fa002bd))
      nextState5.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 62 CMP6
      val nextState6 = execute(state, MixWord(0x0fa002be))
      nextState6.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
      // A = 1000, I = 0, F = 1:2, C = 63 CMPX
      val nextStateX = execute(state, MixWord(0x0fa002bf))
      nextStateX.map(_.registers.getCMP) must beEqualTo(Comparison.EQUAL).await
    }
  }
}
