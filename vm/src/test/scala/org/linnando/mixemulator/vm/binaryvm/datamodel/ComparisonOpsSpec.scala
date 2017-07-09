package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.binary._
import org.specs2.mutable.Specification

class ComparisonOpsSpec extends Specification {

  "binary word comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixWord(0x2) <=> MixWord(0x1) must be equalTo Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixWord(0x1) <=> MixWord(0x2) must be equalTo Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixWord(0x1) <=> MixWord(0x1) must be equalTo Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixWord(0x40000001) <=> MixWord(0x40000001) must be equalTo Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixWord(0x1) <=> MixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixWord(0x40000001) <=> MixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixWord(0x20000000) <=> MixWord(0x60000000) must be equalTo Comparison.GREATER
    }
  }

  "binary index comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixIndex(0x2) <=> MixWord(0x1) must be equalTo Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixIndex(0x1) <=> MixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a bigger negative number to a smaller negative number" in {
      MixIndex(0x1001) <=> MixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "compare a smaller negative number to a bigger negative number" in {
      MixIndex(0x1002) <=> MixWord(0x40000001) must be equalTo Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixIndex(0x1) <=> MixWord(0x1) must be equalTo Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixIndex(0x1001) <=> MixWord(0x40000001) must be equalTo Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixIndex(0x1) <=> MixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixIndex(0x1001) <=> MixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixIndex(0x800) <=> MixWord(0x7ffff800) must be equalTo Comparison.GREATER
    }
  }

}
