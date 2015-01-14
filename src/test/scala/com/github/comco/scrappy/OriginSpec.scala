package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class OriginSpec extends FlatSpec with Matchers {
  val bare = Origin.Bare
  
  "An Origin.Bare" should "provide computed without witnesses" in {
    bare.computed shouldEqual Origin.Computed(Set.empty)
  }
  
  "An Origin.Computed" should "provide the same computed" in {
    // TODO
  }
  
  "An Origin.Original" should "provide pointer" in {
    // TODO
  }
}