package com.github.comco.scrappy.data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import PrimitiveData.apply
import java.util.HashSet

class SomeDataSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)
  val someData = SomeData(optionType, 3)
  
  "A SomeData" should "provide value" in {
    someData.value shouldEqual PrimitiveData(3)
  }
  
  it should "check equality" in {
    (someData == PrimitiveData(3)) shouldEqual false
    val s = new HashSet[Data]()
    s.add(someData)
    s.contains(someData) shouldEqual true
  }
  
  "A SomeData during construction" should "check the type of the value" in {
    itShouldBeDisallowed calling SomeData(optionType, "hi")
  }
}