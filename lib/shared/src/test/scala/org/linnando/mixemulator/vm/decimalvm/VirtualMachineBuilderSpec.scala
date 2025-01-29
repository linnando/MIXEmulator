package org.linnando.mixemulator.vm.decimalvm

import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.decimal.DecimalVirtualMachineBuilder
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.Inside
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VirtualMachineBuilderSpec extends AnyWordSpec with Matchers with Inside {
  "decimal virtual machine builder" should {
    "assign a value to a global symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.symbols("ABC") mustEqual decimal.MixWord(0x400000000L | 116030504L)
        b.getCounter mustEqual 0
      }
    }

    "not allow redefining a global symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      val thrown = the[DuplicateSymbolException] thrownBy builder.withWValueSymbol("ABC", "1000")
      thrown.symbol mustEqual "ABC"
    }

    "assign a value to a local symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.symbols("0H") mustEqual decimal.MixWord(0x400000000L | 116030504L)
        b.getCounter mustEqual 0
      }
    }

    "allow redefining a local symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withWValueSymbol("0H", "-2000(0:2),3000(4:5)")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.symbols("0H") mustEqual decimal.MixWord(0x400000000L | 2000003000L)
        b.getCounter mustEqual 0
      }
    }

    "not allow using local forward references as defined symbols" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[WrongLabelException] thrownBy builder.withWValueSymbol("0F", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      thrown.label mustEqual "0F"
    }

    "not allow using local backward references as defined symbols" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[WrongLabelException] thrownBy builder.withWValueSymbol("0B", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      thrown.label mustEqual "0B"
    }

    "evaluate correct expressions" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("2000")
        .withWValueSymbol("A", "-1+5")
        .withWValueSymbol("B", "-1+5*20/6")
        .withWValueSymbol("C", "1//3")
        .withWValueSymbol("D", "1:3")
        .withWValueSymbol("E", "*-3")
        .withWValueSymbol("F", "***")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.symbols("A") mustEqual decimal.MixWord(4L)
        b.symbols("B") mustEqual decimal.MixWord(13L)
        b.symbols("C") mustEqual decimal.MixWord(3333333333L)
        b.symbols("D") mustEqual decimal.MixWord(11L)
        b.symbols("E") mustEqual decimal.MixWord(1997L)
        b.symbols("F") mustEqual decimal.MixWord(4000000L)
      }
    }

    "throw an exception for a lacking operand" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[InvalidExpressionException] thrownBy builder.withWValueSymbol("ABC", "/2000")
      thrown.expression mustEqual "/2000"
    }

    "throw an exception for an invalid operator" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[InvalidExpressionException] thrownBy builder.withWValueSymbol("ABC", "2000\\a")
      thrown.expression mustEqual "2000\\a"
    }

    "throw an exception for a local reference definition in an expression" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[InvalidExpressionException] thrownBy builder.withWValueSymbol("ABC", "3+2H")
      thrown.expression mustEqual "2H"
    }

    "throw an exception for an invalid field specification" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[InvalidExpressionException] thrownBy builder.withWValueSymbol("ABC", "2000,1(3:5")
      thrown.expression mustEqual "1(3:5"
    }

    "throw an exception when an undefined local backward reference is used in an expression" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[UndefinedSymbolException] thrownBy builder.withWValueSymbol("ABC", "2B-1")
      thrown.symbol mustEqual "2H"
    }

    "throw an exception when an undefined local forward reference is used in an expression" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[UndefinedSymbolException] thrownBy builder.withWValueSymbol("ABC", "2*2F")
      thrown.symbol mustEqual "2H"
    }

    "throw an exception when an undefined global symbol is used in an expression" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[UndefinedSymbolException] thrownBy builder.withWValueSymbol("ABC", "DEF+5")
      thrown.symbol mustEqual "DEF"
    }

    "assign the address current counter value to a symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("2000")
        .withCurrentCounterSymbol("ABC")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.symbols("ABC") mustEqual decimal.MixWord(2000)
      }
    }

    "change the current address counter value" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("2000")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.getCounter mustEqual 2000
      }
    }

    "throw an exception if the memory address is too big" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[WrongMemoryAddressException] thrownBy builder.withOrig("4000")
      thrown.address mustEqual 4000
    }

    "store a value in memory" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withConstant("-2000")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = true, Seq(0, 0, 0, 20, 0))
        b.getCounter mustEqual 1
      }
    }

    "store a character constant in memory" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCharCode(" ABCD")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 1, 2, 3, 4))
        b.getCounter mustEqual 1
      }
    }

    "throw an exception when unsupported characters are encountered" in {
      val builder = decimal.createVirtualMachineBuilder()
      val thrown = the[UnsupportedCharacterException] thrownBy builder.withCharCode(" AbCd")
      thrown.char mustEqual 'b'
    }

    "not store a value outside the memory" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("3999")
        .withConstant("-2000")
      val thrown = the[WrongMemoryAddressException] thrownBy builder.withCharCode(" ABCD")
      thrown.address mustEqual 4000
    }

    "store a command in memory" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("2000", null, null, 8, 5)
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(20, 0, 0, 5, 8))
        b.getCounter mustEqual 1
      }
    }

    "evaluate an expression in the address part of a command in memory" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("2000+2", null, null, 8, 5)
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(20, 2, 0, 5, 8))
        b.getCounter mustEqual 1
      }
    }

    "store forward references" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 5, 8))
        b.forwardReferences must contain("ABC", Seq(decimal.MixIndex(0)))
      }
    }

    "substitute forward references after the global symbol is defined" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
        .withWValueSymbol("ABC", "2000")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(20, 0, 0, 5, 8))
        b.forwardReferences must not contain key("ABC")
      }
    }

    "throw an exception if the symbol defined for a forward reference is too big" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
      an[OverflowException] must be thrownBy builder.withWValueSymbol("ABC", "2000(1:2)")
    }

    "store literals" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withCommand("=2000=", null, null, 8, 5)
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 5, 8))
        b.getCounter mustEqual 1
        b.literals must contain(decimal.MixWord(2000L), Seq(decimal.MixIndex(0)))
      }
    }

    "generate a final section with undefined symbols and literals" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection(null, "3000-1")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(3000) mustEqual IOWord(negative = false, Seq(30, 3, 1, 11, 8))
        b.state.get(3001) mustEqual IOWord(negative = false, Seq(30, 2, 0, 5, 24))
        b.state.get(3002) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        b.state.get(3003) mustEqual IOWord(negative = true, Seq(0, 0, 0, 20, 0))
        b.getCounter mustEqual 3004
        b.state.programCounter mustEqual decimal.MixIndex(2999)
      }
    }

    "treat final section label as a usual symbol" in {
      val builder = decimal.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection("ABC", "0")
      inside(builder) { case b: DecimalVirtualMachineBuilder =>
        b.state.get(3000) mustEqual IOWord(negative = false, Seq(30, 2, 1, 11, 8))
        b.state.get(3001) mustEqual IOWord(negative = false, Seq(30, 3, 0, 5, 24))
        b.state.get(3002) mustEqual IOWord(negative = true, Seq(0, 0, 0, 20, 0))
        b.state.get(3003) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        b.getCounter mustEqual 3003
        b.state.programCounter mustEqual decimal.MixIndex(0)
      }
    }

    "build a non-tracking virtual machine" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .build
      machine.currentState.get(2000) mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) mustEqual IOWord(negative = false, Seq(20, 0, 0, 5, 8))
      machine.currentState.getProgramCounter mustEqual 3000
    }

    "build a tracking virtual machine" in {
      val machine = decimal.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .buildTracking
      machine.currentState.get(2000) mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) mustEqual IOWord(negative = false, Seq(20, 0, 0, 5, 8))
      machine.currentState.getProgramCounter mustEqual 3000
    }
  }
}
