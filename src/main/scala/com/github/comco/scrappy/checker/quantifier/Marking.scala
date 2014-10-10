package com.github.comco.scrappy.checker.quantifier

trait Marking {
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