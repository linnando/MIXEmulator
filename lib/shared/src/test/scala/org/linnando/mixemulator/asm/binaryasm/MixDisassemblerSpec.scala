package org.linnando.mixemulator.asm.binaryasm

import org.linnando.mixemulator.asm.MixDisassembler
import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MixDisassemblerSpec extends AnyWordSpec with Matchers {
  private val disassembler = new MixDisassembler(binary)

  "binary MIX disassembler" should {
    "disassemble a command" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(31, 16, 1, 13, 8)))
      line mustEqual "           LDA  2000,1(1:5)"
    }

    "preserve a zero address" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(0, 0, 1, 13, 8)))
      line mustEqual "           LDA  0,1(1:5)"
    }

    "omit a zero index specification" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(31, 16, 0, 13, 8)))
      line mustEqual "           LDA  2000(1:5)"
    }

    "omit a default field specification" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(31, 16, 1, 5, 8)))
      line mustEqual "           LDA  2000,1"
    }

    "disassemble a command with a negative address" in {
      val line = disassembler.disassembleLine(IOWord(negative = true, Seq(31, 16, 0, 5, 8)))
      line mustEqual "           LDA  -2000"
    }

    "disassemble a command when field specification is used to distinguish operators" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(31, 16, 0, 1, 48)))
      line mustEqual "           DECA 2000"
    }
  }
}
