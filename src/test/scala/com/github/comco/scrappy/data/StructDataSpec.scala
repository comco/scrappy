package com.github.comco.scrappy.data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.StructType
import PrimitiveData.apply
import com.github.comco.scrappy.OptionType
import java.util.HashSet

class StructDataSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)
  val structType = StructType("struct",
    "a" -> IntPrimitiveType,
    "b" -> StringPrimitiveType,
    "c" -> optionType,
    "d" -> optionType)
  val structData = StructData(structType)(
      "a" -> 3, 
      "b" -> "hi", 
      "d" -> SomeData(5))
  
  "A StructData" should "provide datatype" in {
    structData.datatype shouldEqual structType
  }
  
  it should "provide features" in {
    structData.features shouldEqual
      Map("a" -> PrimitiveData(3),
        "b" -> PrimitiveData("hi"),
        "c" -> NoneData(optionType),
        "d" -> SomeData(5))
  }

  it should "provide a feature with a name by feature" in {
    structData.feature("a") shouldEqual PrimitiveData(3)
    structData.feature("b") shouldEqual PrimitiveData("hi")
    structData.feature("c") shouldEqual NoneData(optionType)
    structData.feature("d") shouldEqual SomeData(5)
  }
  
  it should "check for valid feature name in feature" in {
    itShouldBeDisallowed calling structData.feature("non-existing")
  }
  
  it should "provide isOccupied" in {
    structData.isOccupied("a") shouldEqual true
    structData.isOccupied("c") shouldEqual false
    structData.isOccupied("d") shouldEqual true
    structData.isOccupied("non-existing") shouldEqual false
  }
  
  "A StructData during construction" should "check for incompatible features" in {
    itShouldBeDisallowed calling StructData(structType)("a" -> 3, "b" -> 4)
  }
  
  it should "check for invalid feature names" in {
    itShouldBeDisallowed calling StructData(structType)("a" -> 4, "none" -> "hi")
  }
  
  it should "check for missing non-optional features" in  {
    itShouldBeDisallowed calling StructData(structType)()
  }
  
  it should "support creating optional values directly" in {
    val structWithBlanks = StructData(structType)(
        "a" -> 3, 
        "b" -> "hi",
        "d" -> 5)
    structWithBlanks.feature("d") shouldEqual SomeData(5)
  }
  
  it should "check equality" in {
    (structData == PrimitiveData(3)) shouldEqual false
    val s = new HashSet[Data]()
    s.add(structData)
    s.contains(structData) shouldEqual true
  }
}