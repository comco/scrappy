package com.github.comco.scrappy

import org.scalatest.Matchers

trait CustomMatchers extends Matchers {
  object itShouldBeDisallowed {
    def calling(f: => Unit): Unit = {
      an[IllegalArgumentException] should be thrownBy f
    }
  }
}