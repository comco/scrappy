package com.github.comco.scrappy.checker.quantifier

abstract class Marking {
  /**
   * Marks the current result as interesting.
   */
  def markCurrentResult(): Unit
}

object Marking {
  object Empty extends Marking {
    def markCurrentResult(): Unit = {}
  }
}