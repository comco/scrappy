package com.github.comco.scrappy.data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.StructType

import PrimitiveData.apply

class StructDataSpec extends FlatSpec with CustomMatchers {
  val structType = StructType("struct",
    "a" -> IntPrimitiveType,
    "b" -> StringPrimitiveType)
  val structData = StructData(structType)("a" -> 3, "b" -> "hi")

  "A StructData" should "provide datatype" in {
    structData.datatype shouldEqual structType
  }

  it should "provide features" in {
    structData.features shouldEqual
      Map("a" -> PrimitiveData(3),
        "b" -> PrimitiveData("hi"))
  }

  it should "provide a feature with a name by feature" in {
    structData.feature("a") shouldEqual PrimitiveData(3)
  }
}