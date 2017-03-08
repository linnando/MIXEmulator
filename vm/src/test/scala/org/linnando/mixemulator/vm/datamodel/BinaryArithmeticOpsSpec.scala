package org.linnando.mixemulator.vm.datamodel

import org.linnando.mixemulator.vm.BinaryVirtualMachine._
import org.linnando.mixemulator.vm.exceptions.{DivisionByZeroException, OverflowException}
import org.specs2.mutable.Specification

class BinaryArithmeticOpsSpec extends Specification {

  "binary word negation" should {
    "negate a positive number to a negative number" in {
      -BinaryMixWord(0x1) must be equalTo BinaryMixWord(0x40000001)
    }

    "negate a negative number to a positive number" in {
      -BinaryMixWord(0x40000001) must be equalTo BinaryMixWord(0x1)
    }

    "negate the positive zero to the negative zero" in {
      -BinaryMixWord(0x0) must be equalTo BinaryMixWord(0x40000000)
    }

    "negate the negative zero to the positive zero" in {
      -BinaryMixWord(0x40000000) must be equalTo BinaryMixWord(0x0)
    }
  }

  "binary word addition" should {
    "sum two positive numbers" in {
      BinaryMixWord(0x1) + BinaryMixWord(0x2) must be equalTo (false, BinaryMixWord(0x3))
    }

    "sum two negative numbers" in {
      BinaryMixWord(0x40000001) + BinaryMixWord(0x40000002) must be equalTo (false, BinaryMixWord(0x40000003))
    }

    "sum two positive numbers with an overflow" in {
      BinaryMixWord(0x20000000) + BinaryMixWord(0x20000000) must be equalTo (true, BinaryMixWord(0x0))
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      BinaryMixWord(0x2) + BinaryMixWord(0x40000001) must be equalTo (false, BinaryMixWord(0x1))
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      BinaryMixWord(0x1) + BinaryMixWord(0x40000002) must be equalTo (false, BinaryMixWord(0x40000001))
    }

    "sum a positive number and its negation to the positive zero" in {
      BinaryMixWord(0x1) + BinaryMixWord(0x40000001) must be equalTo (false, BinaryMixWord(0x0))
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      BinaryMixWord(0x40000001) + BinaryMixWord(0x1) must be equalTo (false, BinaryMixWord(0x40000000))
    }
  }

  "binary word subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      BinaryMixWord(0x2) - BinaryMixWord(0x1) must be equalTo (false, BinaryMixWord(0x1))
    }

    "subtract a positive number from a smaller positive number" in {
      BinaryMixWord(0x1) - BinaryMixWord(0x2) must be equalTo (false, BinaryMixWord(0x40000001))
    }

    "subtract a positive number from itself giving the positive zero" in {
      BinaryMixWord(0x1) - BinaryMixWord(0x1) must be equalTo (false, BinaryMixWord(0x0))
    }

    "subtract a negative number from itself giving the negative zero" in {
      BinaryMixWord(0x40000001) - BinaryMixWord(0x40000001) must be equalTo (false, BinaryMixWord(0x40000000))
    }

    "subtract a negative number from a positive number" in {
      BinaryMixWord(0x1) - BinaryMixWord(0x40000002) must be equalTo (false, BinaryMixWord(0x3))
    }

    "subtract a positive number from a negative number" in {
      BinaryMixWord(0x40000001) - BinaryMixWord(0x2) must be equalTo (false, BinaryMixWord(0x40000003))
    }

    "subtract a negative number from a positive number with an overflow" in {
      BinaryMixWord(0x20000000) - BinaryMixWord(0x60000000) must be equalTo (true, BinaryMixWord(0x0))
    }
  }

  "binary word multiplication" should {
    "multiply two positive numbers" in {
      BinaryMixWord(0x2) * BinaryMixWord(0x2) must be equalTo BinaryMixDWord(0x4L)
    }

    "multiply two negative numbers" in {
      BinaryMixWord(0x40000002) * BinaryMixWord(0x40000002) must be equalTo BinaryMixDWord(0x4L)
    }

    "multiply a positive and a negative numbers" in {
      BinaryMixWord(0x2) * BinaryMixWord(0x40000002) must be equalTo BinaryMixDWord(0x1000000000000004L)
    }

    "multiply the positive zero and a positive number the positive zero" in {
      BinaryMixWord(0x0) * BinaryMixWord(0x1) must be equalTo BinaryMixDWord(0x0L)
    }

    "multiply the positive zero and a negative number to the negative zero" in {
      BinaryMixWord(0x0) * BinaryMixWord(0x40000001) must be equalTo BinaryMixDWord(0x1000000000000000L)
    }

    "multiply the negative zero and a positive number the negative zero" in {
      BinaryMixWord(0x40000000) * BinaryMixWord(0x1) must be equalTo BinaryMixDWord(0x1000000000000000L)
    }

    "multiply the negative zero and a negative number to the positive zero" in {
      BinaryMixWord(0x40000000) * BinaryMixWord(0x40000001) must be equalTo BinaryMixDWord(0x0L)
    }
  }

  "binary double word division" should {
    "divide a positive number by a positive number" in {
      BinaryMixDWord(0xfL) / BinaryMixWord(0x7) must be equalTo (BinaryMixWord(0x2), BinaryMixWord(0x1))
    }

    "divide a positive number by a negative number" in {
      BinaryMixDWord(0xfL) / BinaryMixWord(0x40000007) must be equalTo (BinaryMixWord(0x40000002), BinaryMixWord(0x1))
    }

    "divide a negative number by a positive number" in {
      BinaryMixDWord(0x100000000000000fL) / BinaryMixWord(0x7) must be equalTo (BinaryMixWord(0x40000002), BinaryMixWord(0x40000001))
    }

    "divide a negative number by a negative number" in {
      BinaryMixDWord(0x100000000000000fL) / BinaryMixWord(0x40000007) must be equalTo (BinaryMixWord(0x2), BinaryMixWord(0x40000001))
    }

    "divide the positive zero by a positive number" in {
      BinaryMixDWord(0x0L) / BinaryMixWord(0x1) must be equalTo (BinaryMixWord(0x0), BinaryMixWord(0x0))
    }

    "divide the positive zero by a negative number" in {
      BinaryMixDWord(0x0L) / BinaryMixWord(0x40000001) must be equalTo (BinaryMixWord(0x40000000), BinaryMixWord(0x0))
    }

    "divide the negative zero by a positive number" in {
      BinaryMixDWord(0x1000000000000000L) / BinaryMixWord(0x1) must be equalTo (BinaryMixWord(0x40000000), BinaryMixWord(0x40000000))
    }

    "divide the negative zero by a negative number" in {
      BinaryMixDWord(0x1000000000000000L) / BinaryMixWord(0x40000001) must be equalTo (BinaryMixWord(0x0), BinaryMixWord(0x40000000))
    }

    "throw an exception on division by the positive zero" in {
      BinaryMixDWord(0x1L) / BinaryMixWord(0x0) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      BinaryMixDWord(0x1L) / BinaryMixWord(0x40000000) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      BinaryMixDWord(0x40000000L) / BinaryMixWord(0x1) must throwAn[OverflowException]
    }
  }

  "binary index negation" should {
    "negate a positive number to a negative number" in {
      -BinaryMixIndex(0x1) must be equalTo BinaryMixIndex(0x1001)
    }

    "negate a negative number to a positive number" in {
      -BinaryMixIndex(0x1001) must be equalTo BinaryMixIndex(0x1)
    }

    "negate the positive zero to the negative zero" in {
      -BinaryMixIndex(0x0) must be equalTo BinaryMixIndex(0x1000)
    }

    "negate the negative zero to the positive zero" in {
      -BinaryMixIndex(0x1000) must be equalTo BinaryMixIndex(0x0)
    }
  }

  "binary index addition" should {
    "sum two positive numbers" in {
      BinaryMixIndex(0x1) + BinaryMixIndex(0x2) must be equalTo BinaryMixIndex(0x3)
    }

    "sum two negative numbers" in {
      BinaryMixIndex(0x1001) + BinaryMixIndex(0x1002) must be equalTo BinaryMixIndex(0x1003)
    }

    "throw an exception if the sum is too big" in {
      BinaryMixIndex(0x800) + BinaryMixIndex(0x800) must throwAn[OverflowException]
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      BinaryMixIndex(0x2) + BinaryMixIndex(0x1001) must be equalTo BinaryMixIndex(0x1)
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      BinaryMixIndex(0x1) + BinaryMixIndex(0x1002) must be equalTo BinaryMixIndex(0x1001)
    }

    "sum a positive number and its negation to the positive zero" in {
      BinaryMixIndex(0x1) + BinaryMixIndex(0x1001) must be equalTo BinaryMixIndex(0x0)
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      BinaryMixIndex(0x1001) + BinaryMixIndex(0x1) must be equalTo BinaryMixIndex(0x1000)
    }
  }

  "binary index offsetting" should {
    "add a positive offset to a positive index" in {
      BinaryMixIndex(0x1) + 0x2 must be equalTo BinaryMixIndex(0x3)
    }

    "add a positive offset to a negative index smaller in absolute value" in {
      BinaryMixIndex(0x1001) + 0x2 must be equalTo BinaryMixIndex(0x1)
    }

    "add a positive offset to a negative index bigger in absolute value" in {
      BinaryMixIndex(0x1002) + 0x1 must be equalTo BinaryMixIndex(0x1001)
    }

    "throw an exception if an offset is negative" in {
      BinaryMixIndex(0x1) + (-2) must throwA[Error]
    }

    "throw an exception if the sum is too big" in {
      BinaryMixIndex(0x800) + 0x800 must throwAn[OverflowException]
    }

    "calculate the next index of a positive index" in {
      BinaryMixIndex(0x1).next must be equalTo BinaryMixIndex(0x2)
    }

    "throw an exception if the next index of a negative index is requested" in {
      BinaryMixIndex(0x1001).next must throwA[Error]
    }

    "throw an exception if the next index is too big" in {
      BinaryMixIndex(0xfff).next must throwAn[OverflowException]
    }
  }

  "binary index subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      BinaryMixIndex(0x2) - BinaryMixIndex(0x1) must be equalTo BinaryMixIndex(0x1)
    }

    "subtract a positive number from a smaller positive number" in {
      BinaryMixIndex(0x1) - BinaryMixIndex(0x2) must be equalTo BinaryMixIndex(0x1001)
    }

    "subtract a positive number from itself giving the positive zero" in {
      BinaryMixIndex(0x1) - BinaryMixIndex(0x1) must be equalTo BinaryMixIndex(0x0)
    }

    "subtract a negative number from itself giving the negative zero" in {
      BinaryMixIndex(0x1001) - BinaryMixIndex(0x1001) must be equalTo BinaryMixIndex(0x1000)
    }

    "subtract a negative number from a positive number" in {
      BinaryMixIndex(0x1) - BinaryMixIndex(0x1002) must be equalTo BinaryMixIndex(0x3)
    }

    "subtract a positive number from a negative number" in {
      BinaryMixIndex(0x1001) - BinaryMixIndex(0x2) must be equalTo BinaryMixIndex(0x1003)
    }

    "subtract a negative number from a positive number with an overflow" in {
      BinaryMixIndex(0x800) - BinaryMixIndex(0x1800) must throwAn[OverflowException]
    }
  }

}
