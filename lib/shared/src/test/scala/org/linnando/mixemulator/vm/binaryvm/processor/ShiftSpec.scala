package org.linnando.mixemulator.vm.binaryvm.processor

import org.linnando.mixemulator.vm.binary
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext

class ShiftSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

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
      binary.execute(state, MixWord(0x00080006)).map(s => {
        s.registers.getA mustEqual MixWord(0x03105000) // + 3 4 5 0 0
        s.registers.getX mustEqual MixWord(0x461c824a) // - 6 7 8 9 10
      })
    }

    "shift A to the right" in {
      // A = 2, I = 0, F = 1, C = 6 SRA
      binary.execute(state, MixWord(0x00080046)).map(s => {
        s.registers.getA mustEqual MixWord(0x00001083) // + 0 0 1 2 3
        s.registers.getX mustEqual MixWord(0x461c824a) // - 6 7 8 9 10
      })
    }

    "shift AX to the left" in {
      // A = 1, I = 0, F = 2, C = 6 SLAX
      binary.execute(state, MixWord(0x00040086)).map(s => {
        s.registers.getA mustEqual MixWord(0x020c4146) // + 2 3 4 5 6
        s.registers.getX mustEqual MixWord(0x47209280) // - 7 8 9 10 0
      })
    }

    "shift AX to the right" in {
      // A = 1, I = 0, F = 3, C = 6 SRAX
      binary.execute(state, MixWord(0x000400c6)).map(s => {
        s.registers.getA mustEqual MixWord(0x000420c4) // + 0 1 2 3 4
        s.registers.getX mustEqual MixWord(0x45187209) // - 5 6 7 8 9
      })
    }

    "shift AX to the left circularly" in {
      // A = 1, I = 0, F = 4, C = 6 SLC
      binary.execute(state, MixWord(0x00040106)).map(s => {
        s.registers.getA mustEqual MixWord(0x020c4146) // + 2 3 4 5 6
        s.registers.getX mustEqual MixWord(0x47209281) // - 7 8 9 10 1
      })
    }

    "shift AX to the right circularly" in {
      // A = 1, I = 0, F = 5, C = 6 SRC
      binary.execute(state, MixWord(0x00040146)).map(s => {
        s.registers.getA mustEqual MixWord(0x0a0420c4) // + 10 1 2 3 4
        s.registers.getX mustEqual MixWord(0x45187209) // - 5 6 7 8 9
      })
    }
  }
}
