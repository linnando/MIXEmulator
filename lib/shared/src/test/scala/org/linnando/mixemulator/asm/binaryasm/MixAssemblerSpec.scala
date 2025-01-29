package org.linnando.mixemulator.asm.binaryasm

import org.linnando.mixemulator.asm.MixAssembler
import org.linnando.mixemulator.asm.exceptions._
import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.binary.BinaryVirtualMachineBuilder
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.Inside
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MixAssemblerSpec extends AnyWordSpec with Matchers with Inside {
  private val builder = binary.createVirtualMachineBuilder()
  private val assembler = MixAssembler(builder)

  "binary MIX assembler" should {
    "skip a comment line" in {
      val nextState = assembler.withLine(1, " * COMMENT")
      nextState.builder.build.currentState mustEqual builder.build.currentState
      nextState.symbolsBeforeCounter mustEqual List((None, Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(0), None)
    }

    "throw an exception on a wrong line" in {
      a[WrongLineException] must be thrownBy assembler.withLine(1, "abc")
    }

    "assign a symbol" in {
      val nextState = assembler.withLine(1, "ABC EQU 2000")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols must contain("ABC", binary.MixWord(2000))
      }
      nextState.symbolsBeforeCounter mustEqual List((None, Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(0), None)
    }

    "throw an exception without a symbol value" in {
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, "ABC EQU   ")
    }

    "change the current address counter value" in {
      val nextState = assembler.withLine(1, " ORIG 2000")
      nextState.builder.getCounter mustEqual 2000
      nextState.symbolsBeforeCounter.head mustEqual(None, Some(1))
      nextState.symbolsBeforeCounter(1) mustEqual(Some(1999), None)
      nextState.symbolsAfterCounter.head mustEqual(Some(2000), None)
    }

    "throw an exception without an address counter value" in {
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, " ORIG ")
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, "ABC ORIG ")
    }

    "store a constant" in {
      val nextState = assembler.withLine(1, " CON 2000")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 31, 16))
      }
      nextState.symbolsBeforeCounter mustEqual List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(1), None)
    }

    "throw an exception without a value of a constant" in {
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, " CON ")
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, "ABC CON ")
    }

    "store a character constant separated with one space" in {
      val nextState = assembler.withLine(1, " ALF ABCDE")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(1, 2, 3, 4, 5))
      }
      nextState.symbolsBeforeCounter mustEqual List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(1), None)
    }

    "store a character constant separated with two spaces" in {
      val nextState = assembler.withLine(1, " ALF   ABCD")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 1, 2, 3, 4))
      }
      nextState.symbolsBeforeCounter mustEqual List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(1), None)
    }

    "translate a command" in {
      val nextState = assembler.withLine(1, " LDA 2000")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      }
      nextState.symbolsBeforeCounter mustEqual List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(1), None)
    }

    "translate a command without an address part" in {
      val nextState = assembler.withLine(1, " HLT")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 2, 5))
      }
      nextState.symbolsBeforeCounter mustEqual List((Some(0), Some(1)))
      nextState.symbolsAfterCounter.head mustEqual(Some(1), None)
    }

    "throw an exception on a wrong operator" in {
      a[WrongOperatorException] must be thrownBy assembler.withLine(1, " LDZ 1000")
    }

    "throw an exception with a wrong address part format" in {
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, " LDA 0,")
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, " LDA 0()")
    }

    "throw an exception on an attempt to set a field specification when it is used to distinguish commands" in {
      a[FixedFieldSpecException] must be thrownBy assembler.withLine(1, " HLT 0,0(0)")
    }

    "generate a final section with undefined symbols and literals" in {
      val nextState = assembler
        .withLine(0, " ORIG 3000")
        .withLine(1, " LDA =-2000=,1(1:3)")
        .withLine(2, " STA ABC")
        .withLine(3, " END 3000")
      inside(nextState.builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(3000) mustEqual IOWord(negative = false, Seq(46, 59, 1, 11, 8))
        b.state.get(3001) mustEqual IOWord(negative = false, Seq(46, 58, 0, 5, 24))
        b.state.get(3002) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        b.state.get(3003) mustEqual IOWord(negative = true, Seq(0, 0, 0, 31, 16))
        b.state.programCounter mustEqual binary.MixIndex(3000)
      }
      nextState.symbolsBeforeCounter.head mustEqual(Some(3003), None)
      nextState.symbolsBeforeCounter(1) mustEqual(Some(3002), None)
      nextState.symbolsBeforeCounter(2) mustEqual(Some(3001), Some(2))
      nextState.symbolsBeforeCounter(3) mustEqual(Some(3000), Some(1))
      nextState.symbolsBeforeCounter(4) mustEqual(None, Some(0))
      nextState.symbolsBeforeCounter(5) mustEqual(Some(2999), None)
      nextState.symbolsAfterCounter.head mustEqual(Some(3004), Some(3))
    }

    "throw an exception without a program start address" in {
      a[WrongAddressPartException] must be thrownBy assembler.withLine(1, " END ")
    }

    "translate a whole program" in {
      val program = MixAssembler.translateNonTracking(
        binary.createVirtualMachineBuilder(),
        Seq(
          " ORIG 3000",
          " LDA =-2000=,1(1:3)",
          " STA ABC",
          " END 3000"
        )
      )
      program._1.currentState.get(3000) mustEqual IOWord(negative = false, Seq(46, 59, 1, 11, 8))
      program._1.currentState.get(3001) mustEqual IOWord(negative = false, Seq(46, 58, 0, 5, 24))
      program._1.currentState.get(3002) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
      program._1.currentState.get(3003) mustEqual IOWord(negative = true, Seq(0, 0, 0, 31, 16))
      program._1.currentState.getProgramCounter mustEqual 3000
      program._2 mustEqual (
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
