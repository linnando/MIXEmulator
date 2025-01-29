package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.binary._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComparisonOpsSpec extends AnyWordSpec with Matchers {
  "binary word comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixWord(0x2) <=> MixWord(0x1) mustEqual Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixWord(0x1) <=> MixWord(0x2) mustEqual Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixWord(0x1) <=> MixWord(0x1) mustEqual Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixWord(0x40000001) <=> MixWord(0x40000001) mustEqual Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixWord(0x1) <=> MixWord(0x40000002) mustEqual Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixWord(0x40000001) <=> MixWord(0x2) mustEqual Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixWord(0x20000000) <=> MixWord(0x60000000) mustEqual Comparison.GREATER
    }
  }

  "binary index comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixIndex(0x2) <=> MixWord(0x1) mustEqual Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixIndex(0x1) <=> MixWord(0x2) mustEqual Comparison.LESS
    }

    "compare a bigger negative number to a smaller negative number" in {
      MixIndex(0x1001) <=> MixWord(0x40000002) mustEqual Comparison.GREATER
    }

    "compare a smaller negative number to a bigger negative number" in {
      MixIndex(0x1002) <=> MixWord(0x40000001) mustEqual Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixIndex(0x1) <=> MixWord(0x1) mustEqual Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixIndex(0x1001) <=> MixWord(0x40000001) mustEqual Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixIndex(0x1) <=> MixWord(0x40000002) mustEqual Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixIndex(0x1001) <=> MixWord(0x2) mustEqual Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixIndex(0x800) <=> MixWord(0x7ffff800) mustEqual Comparison.GREATER
    }
  }
}
