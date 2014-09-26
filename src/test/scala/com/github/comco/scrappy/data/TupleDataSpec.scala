package com.github.comco.scrappy.data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType

import PrimitiveData.apply

class TupleDataSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  val tuple = TupleData(3, "hi", false)

  "A TupleData" should "provide datatype" in {
    tuple.datatype shouldEqual tupleType
  }

  it should "provide coordinates" in {
    tuple.coordinates shouldEqual
      Seq(PrimitiveData(3), PrimitiveData("hi"), PrimitiveData(false))
  }

  "A TupleData during construction" should "validate the number of coordinates" in {
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)()
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)(3, "hi")
  }

  it should "validate the types of coordinates" in {
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)(3, "hi", "bye")
  }
}