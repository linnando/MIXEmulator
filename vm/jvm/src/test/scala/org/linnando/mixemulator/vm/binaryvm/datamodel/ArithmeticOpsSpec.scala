package org.linnando.mixemulator.vm.binaryvm.datamodel

import org.linnando.mixemulator.vm.binary._
import org.linnando.mixemulator.vm.exceptions.{DivisionByZeroException, OverflowException}
import org.specs2.mutable.Specification

class ArithmeticOpsSpec extends Specification {

  "binary word negation" should {
    "negate a positive number to a negative number" in {
      -MixWord(0x1) must be equalTo MixWord(0x40000001)
    }

    "negate a negative number to a positive number" in {
      -MixWord(0x40000001) must be equalTo MixWord(0x1)
    }

    "negate the positive zero to the negative zero" in {
      -MixWord(0x0) must be equalTo MixWord(0x40000000)
    }

    "negate the negative zero to the positive zero" in {
      -MixWord(0x40000000) must be equalTo MixWord(0x0)
    }
  }

  "binary word addition" should {
    "sum two positive numbers" in {
      MixWord(0x1) + MixWord(0x2) must be equalTo (false, MixWord(0x3))
    }

    "sum two negative numbers" in {
      MixWord(0x40000001) + MixWord(0x40000002) must be equalTo (false, MixWord(0x40000003))
    }

    "sum two positive numbers with an overflow" in {
      MixWord(0x20000000) + MixWord(0x20000000) must be equalTo (true, MixWord(0x0))
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      MixWord(0x2) + MixWord(0x40000001) must be equalTo (false, MixWord(0x1))
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      MixWord(0x1) + MixWord(0x40000002) must be equalTo (false, MixWord(0x40000001))
    }

    "sum a positive number and its negation to the positive zero" in {
      MixWord(0x1) + MixWord(0x40000001) must be equalTo (false, MixWord(0x0))
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      MixWord(0x40000001) + MixWord(0x1) must be equalTo (false, MixWord(0x40000000))
    }
  }

  "binary word subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      MixWord(0x2) - MixWord(0x1) must be equalTo (false, MixWord(0x1))
    }

    "subtract a positive number from a smaller positive number" in {
      MixWord(0x1) - MixWord(0x2) must be equalTo (false, MixWord(0x40000001))
    }

    "subtract a positive number from itself giving the positive zero" in {
      MixWord(0x1) - MixWord(0x1) must be equalTo (false, MixWord(0x0))
    }

    "subtract a negative number from itself giving the negative zero" in {
      MixWord(0x40000001) - MixWord(0x40000001) must be equalTo (false, MixWord(0x40000000))
    }

    "subtract a negative number from a positive number" in {
      MixWord(0x1) - MixWord(0x40000002) must be equalTo (false, MixWord(0x3))
    }

    "subtract a positive number from a negative number" in {
      MixWord(0x40000001) - MixWord(0x2) must be equalTo (false, MixWord(0x40000003))
    }

    "subtract a negative number from a positive number with an overflow" in {
      MixWord(0x20000000) - MixWord(0x60000000) must be equalTo (true, MixWord(0x0))
    }
  }

  "binary word multiplication" should {
    "multiply two positive numbers" in {
      MixWord(0x2) * MixWord(0x2) must be equalTo MixDWord(0x4L)
    }

    "multiply two negative numbers" in {
      MixWord(0x40000002) * MixWord(0x40000002) must be equalTo MixDWord(0x4L)
    }

    "multiply a positive and a negative numbers" in {
      MixWord(0x2) * MixWord(0x40000002) must be equalTo MixDWord(0x1000000000000004L)
    }

    "multiply the positive zero and a positive number the positive zero" in {
      MixWord(0x0) * MixWord(0x1) must be equalTo MixDWord(0x0L)
    }

    "multiply the positive zero and a negative number to the negative zero" in {
      MixWord(0x0) * MixWord(0x40000001) must be equalTo MixDWord(0x1000000000000000L)
    }

    "multiply the negative zero and a positive number the negative zero" in {
      MixWord(0x40000000) * MixWord(0x1) must be equalTo MixDWord(0x1000000000000000L)
    }

    "multiply the negative zero and a negative number to the positive zero" in {
      MixWord(0x40000000) * MixWord(0x40000001) must be equalTo MixDWord(0x0L)
    }
  }

  "binary word division" should {
    "divide a positive number by a positive number" in {
      MixWord(0xf) / MixWord(0x7) must be equalTo (MixWord(0x2), MixWord(0x1))
    }

    "divide a positive number by a negative number" in {
      MixWord(0xf) / MixWord(0x40000007) must be equalTo (MixWord(0x40000002), MixWord(0x1))
    }

    "divide a negative number by a positive number" in {
      MixWord(0x4000000f) / MixWord(0x7) must be equalTo (MixWord(0x40000002), MixWord(0x40000001))
    }

    "divide a negative number by a negative number" in {
      MixWord(0x4000000f) / MixWord(0x40000007) must be equalTo (MixWord(0x2), MixWord(0x40000001))
    }

    "divide the positive zero by a positive number" in {
      MixWord(0x0) / MixWord(0x1) must be equalTo (MixWord(0x0), MixWord(0x0))
    }

    "divide the positive zero by a negative number" in {
      MixWord(0x0) / MixWord(0x40000001) must be equalTo (MixWord(0x40000000), MixWord(0x0))
    }

    "divide the negative zero by a positive number" in {
      MixWord(0x40000000) / MixWord(0x1) must be equalTo (MixWord(0x40000000), MixWord(0x40000000))
    }

    "divide the negative zero by a negative number" in {
      MixWord(0x40000000) / MixWord(0x40000001) must be equalTo (MixWord(0x0), MixWord(0x40000000))
    }

    "throw an exception on division by the positive zero" in {
      MixWord(0x1) / MixWord(0x0) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixWord(0x1) / MixWord(0x40000000) must throwA[DivisionByZeroException]
    }
  }

  "binary word division to the fractional part" should {
    "divide a positive number by a positive number" in {
      MixWord(0x2) /\ MixWord(0x7) must be equalTo (MixWord(0x12492492), MixWord(0x2))
    }

    "divide a positive number by a negative number" in {
      MixWord(0x2) /\ MixWord(0x40000007) must be equalTo (MixWord(0x52492492), MixWord(0x2))
    }

    "divide a negative number by a positive number" in {
      MixWord(0x40000002) /\ MixWord(0x7) must be equalTo (MixWord(0x52492492), MixWord(0x40000002))
    }

    "divide a negative number by a negative number" in {
      MixWord(0x40000002) /\ MixWord(0x40000007) must be equalTo (MixWord(0x12492492), MixWord(0x40000002))
    }

    "divide the positive zero by a positive number" in {
      MixWord(0x0) /\ MixWord(0x1) must be equalTo (MixWord(0x0), MixWord(0x0))
    }

    "divide the positive zero by a negative number" in {
      MixWord(0x0) /\ MixWord(0x40000001) must be equalTo (MixWord(0x40000000), MixWord(0x0))
    }

    "divide the negative zero by a positive number" in {
      MixWord(0x40000000) /\ MixWord(0x1) must be equalTo (MixWord(0x40000000), MixWord(0x40000000))
    }

    "divide the negative zero by a negative number" in {
      MixWord(0x40000000) /\ MixWord(0x40000001) must be equalTo (MixWord(0x0), MixWord(0x40000000))
    }

    "throw an exception on division by the positive zero" in {
      MixWord(0x1) /\ MixWord(0x0) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixWord(0x1) /\ MixWord(0x40000000) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      MixWord(0x7) /\ MixWord(0x7) must throwAn[OverflowException]
    }
  }

  "binary double word division" should {
    "divide a positive number by a positive number" in {
      MixDWord(0xfL) / MixWord(0x7) must be equalTo (MixWord(0x2), MixWord(0x1))
    }

    "divide a positive number by a negative number" in {
      MixDWord(0xfL) / MixWord(0x40000007) must be equalTo (MixWord(0x40000002), MixWord(0x1))
    }

    "divide a negative number by a positive number" in {
      MixDWord(0x100000000000000fL) / MixWord(0x7) must be equalTo (MixWord(0x40000002), MixWord(0x40000001))
    }

    "divide a negative number by a negative number" in {
      MixDWord(0x100000000000000fL) / MixWord(0x40000007) must be equalTo (MixWord(0x2), MixWord(0x40000001))
    }

    "divide the positive zero by a positive number" in {
      MixDWord(0x0L) / MixWord(0x1) must be equalTo (MixWord(0x0), MixWord(0x0))
    }

    "divide the positive zero by a negative number" in {
      MixDWord(0x0L) / MixWord(0x40000001) must be equalTo (MixWord(0x40000000), MixWord(0x0))
    }

    "divide the negative zero by a positive number" in {
      MixDWord(0x1000000000000000L) / MixWord(0x1) must be equalTo (MixWord(0x40000000), MixWord(0x40000000))
    }

    "divide the negative zero by a negative number" in {
      MixDWord(0x1000000000000000L) / MixWord(0x40000001) must be equalTo (MixWord(0x0), MixWord(0x40000000))
    }

    "throw an exception on division by the positive zero" in {
      MixDWord(0x1L) / MixWord(0x0) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixDWord(0x1L) / MixWord(0x40000000) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      MixDWord(0x40000000L) / MixWord(0x1) must throwAn[OverflowException]
    }
  }

  "binary index negation" should {
    "negate a positive number to a negative number" in {
      -MixIndex(0x1) must be equalTo MixIndex(0x1001)
    }

    "negate a negative number to a positive number" in {
      -MixIndex(0x1001) must be equalTo MixIndex(0x1)
    }

    "negate the positive zero to the negative zero" in {
      -MixIndex(0x0) must be equalTo MixIndex(0x1000)
    }

    "negate the negative zero to the positive zero" in {
      -MixIndex(0x1000) must be equalTo MixIndex(0x0)
    }
  }

  "binary index addition" should {
    "sum two positive numbers" in {
      MixIndex(0x1) + MixIndex(0x2) must be equalTo MixIndex(0x3)
    }

    "sum two negative numbers" in {
      MixIndex(0x1001) + MixIndex(0x1002) must be equalTo MixIndex(0x1003)
    }

    "throw an exception if the sum is too big" in {
      MixIndex(0x800) + MixIndex(0x800) must throwAn[OverflowException]
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      MixIndex(0x2) + MixIndex(0x1001) must be equalTo MixIndex(0x1)
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      MixIndex(0x1) + MixIndex(0x1002) must be equalTo MixIndex(0x1001)
    }

    "sum a positive number and its negation to the positive zero" in {
      MixIndex(0x1) + MixIndex(0x1001) must be equalTo MixIndex(0x0)
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      MixIndex(0x1001) + MixIndex(0x1) must be equalTo MixIndex(0x1000)
    }
  }

  "binary index offsetting" should {
    "add a positive offset to a positive index" in {
      MixIndex(0x1) + 0x2 must be equalTo MixIndex(0x3)
    }

    "add a positive offset to a negative index smaller in absolute value" in {
      MixIndex(0x1001) + 0x2 must be equalTo MixIndex(0x1)
    }

    "add a positive offset to a negative index bigger in absolute value" in {
      MixIndex(0x1002) + 0x1 must be equalTo MixIndex(0x1001)
    }

    "throw an exception if an offset is negative" in {
      MixIndex(0x1) + (-2) must throwA[Error]
    }

    "throw an exception if the sum is too big" in {
      MixIndex(0x800) + 0x800 must throwAn[OverflowException]
    }

    "calculate the next index of a positive index" in {
      MixIndex(0x1).next must be equalTo MixIndex(0x2)
    }

    "throw an exception if the next index of a negative index is requested" in {
      MixIndex(0x1001).next must throwA[Error]
    }

    "throw an exception if the next index is too big" in {
      MixIndex(0xfff).next must throwAn[OverflowException]
    }
  }

  "binary index subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      MixIndex(0x2) - MixIndex(0x1) must be equalTo MixIndex(0x1)
    }

    "subtract a positive number from a smaller positive number" in {
      MixIndex(0x1) - MixIndex(0x2) must be equalTo MixIndex(0x1001)
    }

    "subtract a positive number from itself giving the positive zero" in {
      MixIndex(0x1) - MixIndex(0x1) must be equalTo MixIndex(0x0)
    }

    "subtract a negative number from itself giving the negative zero" in {
      MixIndex(0x1001) - MixIndex(0x1001) must be equalTo MixIndex(0x1000)
    }

    "subtract a negative number from a positive number" in {
      MixIndex(0x1) - MixIndex(0x1002) must be equalTo MixIndex(0x3)
    }

    "subtract a positive number from a negative number" in {
      MixIndex(0x1001) - MixIndex(0x2) must be equalTo MixIndex(0x1003)
    }

    "subtract a negative number from a positive number with an overflow" in {
      MixIndex(0x800) - MixIndex(0x1800) must throwAn[OverflowException]
    }
  }

}
