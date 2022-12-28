package org.linnando.mixemulator.asm.decimalasm

import org.linnando.mixemulator.asm.MixAssembler
import org.linnando.mixemulator.asm.exceptions._
import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.decimal.DecimalVirtualMachineBuilder
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.mutable.Specification

class MixAssemblerSpec extends Specification {
  private val builder = decimal.createVirtualMachineBuilder()
  private val assembler = MixAssembler(builder)

  "decimal MIX assembler" should {
    "skip a comment line" in {
      val nextState = assembler.withLine(1, " * COMMENT")
      nextState.builder.build.currentState must be equalTo builder.build.currentState
      nextState.symbolsBeforeCounter must be equalTo List((None, Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(0), None)
    }

    "throw an exception on a wrong line" in {
      assembler.withLine(1, "abc") must throwA[WrongLineException]
    }

    "assign a symbol" in {
      val nextState = assembler.withLine(1, "ABC EQU 2000")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.symbols must havePair("ABC", decimal.MixWord(2000L))
      }
      nextState.symbolsBeforeCounter must be equalTo List((None, Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(0), None)
    }

    "throw an exception without a symbol value" in {
      assembler.withLine(1, "ABC EQU   ") must throwA[WrongAddressPartException]
    }

    "change the current address counter value" in {
      val nextState = assembler.withLine(1, " ORIG 2000")
      nextState.builder.getCounter must be equalTo 2000
      nextState.symbolsBeforeCounter.head must be equalTo(None, Some(1))
      nextState.symbolsBeforeCounter(1) must be equalTo(Some(1999), None)
      nextState.symbolsAfterCounter.head must be equalTo(Some(2000), None)
    }

    "throw an exception without an address counter value" in {
      assembler.withLine(1, " ORIG ") must throwA[WrongAddressPartException]
      assembler.withLine(1, "ABC ORIG ") must throwA[WrongAddressPartException]
    }

    "store a constant" in {
      val nextState = assembler.withLine(1, " CON 2000")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 20, 0))
      }
      nextState.symbolsBeforeCounter must be equalTo List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(1), None)
    }

    "throw an exception without a value of a constant" in {
      assembler.withLine(1, " CON ") must throwA[WrongAddressPartException]
      assembler.withLine(1, "ABC CON ") must throwA[WrongAddressPartException]
    }

    "store a character constant separated with one space" in {
      val nextState = assembler.withLine(1, " ALF ABCDE")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(1, 2, 3, 4, 5))
      }
      nextState.symbolsBeforeCounter must be equalTo List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(1), None)
    }

    "store a character constant separated with two spaces" in {
      val nextState = assembler.withLine(1, " ALF   ABCD")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 1, 2, 3, 4))
      }
      nextState.symbolsBeforeCounter must be equalTo List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(1), None)
    }

    "translate a command" in {
      val nextState = assembler.withLine(1, " LDA 2000")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(20, 0, 0, 5, 8))
      }
      nextState.symbolsBeforeCounter must be equalTo List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(1), None)
    }

    "translate a command without an address part" in {
      val nextState = assembler.withLine(1, " HLT")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 2, 5))
      }
      nextState.symbolsBeforeCounter must be equalTo List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head must be equalTo(Some(1), None)
    }

    "throw an exception on a wrong operator" in {
      assembler.withLine(1, " LDZ 1000") must throwA[WrongOperatorException]
    }

    "throw an exception with a wrong address part format" in {
      assembler.withLine(1, " LDA 0,") must throwA[WrongAddressPartException]
      assembler.withLine(1, " LDA 0()") must throwA[WrongAddressPartException]
    }

    "throw an exception on an attempt to set a field specification when it is used to distinguish commands" in {
      assembler.withLine(1, " HLT 0,0(0)") must throwA[FixedFieldSpecException]
    }

    "generate a final section with undefined symbols and literals" in {
      val nextState = assembler
        .withLine(0, " ORIG 3000")
        .withLine(1, " LDA =-2000=,1(1:3)")
        .withLine(2, " STA ABC")
        .withLine(3, " END 3000")
      nextState.builder match {
        case b: DecimalVirtualMachineBuilder =>
          b.state.get(3000) must be equalTo IOWord(negative = false, Seq(30, 3, 1, 11, 8))
          b.state.get(3001) must be equalTo IOWord(negative = false, Seq(30, 2, 0, 5, 24))
          b.state.get(3002) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
          b.state.get(3003) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 20, 0))
          b.state.programCounter must be equalTo decimal.MixIndex(3000)
      }
      nextState.symbolsBeforeCounter.head must be equalTo(Some(3003), None)
      nextState.symbolsBeforeCounter(1) must be equalTo(Some(3002), None)
      nextState.symbolsBeforeCounter(2) must be equalTo(Some(3001), Some(2))
      nextState.symbolsBeforeCounter(3) must be equalTo(Some(3000), Some(1))
      nextState.symbolsBeforeCounter(4) must be equalTo(None, Some(0))
      nextState.symbolsBeforeCounter(5) must be equalTo(Some(2999), None)
      nextState.symbolsAfterCounter.head must be equalTo(Some(3004), Some(3))
    }

    "throw an exception without a program start address" in {
      assembler.withLine(1, " END ") must throwA[WrongAddressPartException]
    }

    "translate a whole program" in {
      val program = MixAssembler.translateNonTracking(
        decimal.createVirtualMachineBuilder(),
        Seq(
          " ORIG 3000",
          " LDA =-2000=,1(1:3)",
          " STA ABC",
          " END 3000"
        )
      )
      program._1.currentState.get(3000) must be equalTo IOWord(negative = false, Seq(30, 3, 1, 11, 8))
      program._1.currentState.get(3001) must be equalTo IOWord(negative = false, Seq(30, 2, 0, 5, 24))
      program._1.currentState.get(3002) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
      program._1.currentState.get(3003) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 20, 0))
      program._1.currentState.getProgramCounter must be equalTo 3000
      program._2 must be equalTo (
        (0 until 3000).map(i => (Some(i.toShort), None)) ++
          Seq((None, Some(0)),
            (Some(3000.toShort), Some(1)),
            (Some(3001.toShort), Some(2)),
            (Some(3002.toShort), None),
            (Some(3003.toShort), None),
            (Some(3004.toShort), Some(3))) ++
          (3005 until 4000).map(i => (Some(i.toShort), None))
      ).toList
    }
  }
}
