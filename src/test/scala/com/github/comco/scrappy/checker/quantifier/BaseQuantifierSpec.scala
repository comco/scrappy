package com.github.comco.scrappy.checker.quantifier

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers

final class BaseQuantifierSpec extends FlatSpec with CustomMatchers {
  sealed class TestQuantifier extends BaseQuantifier {
    def doPut(next: Boolean): Unit = ???

    def isUnusual(): Boolean = ???

    def isValid(): Boolean = ???
  }

  "A BaseQuantifier" should "check state before put" in {
    object TestPutRequireQuantifier extends TestQuantifier {
      state = Quantifier.Done
    }

    an[AssertionError] should be thrownBy TestPutRequireQuantifier.put(false)
  }

  it should "ensure state after put" in {
    object TestPutEnsureQuantifier extends TestQuantifier {
      override def doPut(next: Boolean) {
        state = Quantifier.Empty
      }
    }

    an[AssertionError] should be thrownBy TestPutEnsureQuantifier.put(false)
  }

  it should "check unusual state before" in {
    object TestUnusualRequireQuantifier extends TestQuantifier {
      state = Quantifier.Empty
    }

    an[AssertionError] should be thrownBy TestUnusualRequireQuantifier.unusual
  }

  it should "check unusual state after" in {
    object TestUnusualEnsureQuantifier extends TestQuantifier {
      state = Quantifier.Current

      override def isUnusual(): Boolean = {
        state = Quantifier.Done
        return false
      }
    }

    an[AssertionError] should be thrownBy TestUnusualEnsureQuantifier.unusual
  }

  it should "check finish state before" in {
    object TestFinishRequireQuantifier extends TestQuantifier {
      state = Quantifier.Done
    }

    an[AssertionError] should be thrownBy TestFinishRequireQuantifier.finish()
  }

  it should "check finish state after" in {
    object TestFinishEnsuringQuantifier extends TestQuantifier {
      override def doFinish() {
        state = Quantifier.Current
      }
    }

    TestFinishEnsuringQuantifier.finish()
    TestFinishEnsuringQuantifier.state shouldEqual Quantifier.Done
  }

  it should "check valid state before" in {
    object TestValidRequireQuantifier extends TestQuantifier {
      state = Quantifier.Current
    }

    an[AssertionError] should be thrownBy TestValidRequireQuantifier.valid
  }

  it should "check valid state after" in {
    object TestValidEnsuringQuantifier extends TestQuantifier {
      state = Quantifier.Done

      override def isValid(): Boolean = {
        state = Quantifier.Current
        return false
      }
    }

    an[AssertionError] should be thrownBy TestValidEnsuringQuantifier.valid
  }
}