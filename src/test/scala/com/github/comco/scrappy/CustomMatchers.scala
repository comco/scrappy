package com.github.comco.scrappy

import org.scalatest.Matchers

trait CustomMatchers extends Matchers {
  def shouldBeDisallowed(f: => Unit): Unit = {
    an[IllegalArgumentException] should be thrownBy f
  }
}