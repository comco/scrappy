package com.github.comco.scrappy.data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType

import PrimitiveData.apply

final class TupleDataSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  val tupleData = TupleData(3, "hi", false)

  val optionType = OptionType(StringPrimitiveType)
  val tupleTypeWithBlanks = TupleType(IntPrimitiveType, optionType)
  val tupleDataWithBlanks = TupleData(tupleTypeWithBlanks)(4, NoneData(optionType))

  "A TupleData" should "provide datatype" in {
    tupleData.datatype shouldEqual tupleType
  }

  it should "provide length" in {
    tupleData.length shouldEqual 3
    tupleDataWithBlanks.length shouldEqual 2
  }

  it should "provide coordinates" in {
    tupleData.coordinates shouldEqual
      Seq(PrimitiveData(3), PrimitiveData("hi"), PrimitiveData(false))
  }

  it should "provide a coordinate at a position" in {
    tupleData.coordinate(0) shouldEqual PrimitiveData(3)
    tupleData.coordinate(1) shouldEqual PrimitiveData("hi")
    tupleData.coordinate(2) shouldEqual PrimitiveData(false)

    tupleDataWithBlanks.coordinate(1) shouldEqual NoneData(optionType)
  }

  it should "check if the position passed to coordinate is valid" in {
    itShouldBeDisallowed calling tupleData.coordinate(-1)
    itShouldBeDisallowed calling tupleData.coordinate(3)
  }

  it should "support checking for occupied position by isOccupied" in {
    tupleData.isOccupied(0) shouldEqual true
    tupleData.isOccupied(-1) shouldEqual false
    tupleData.isOccupied(3) shouldEqual false

    tupleDataWithBlanks.isOccupied(1) shouldEqual false
  }

  "A TupleData during construction" should "check the number of coordinates" in {
    itShouldBeDisallowed calling TupleData(tupleType)()
    itShouldBeDisallowed calling TupleData(tupleType)(3, "hi")
  }

  it should "check the types of coordinates" in {
    itShouldBeDisallowed calling TupleData(tupleType)(3, "hi", "bye")
  }

  it should "check equality" in {
    (tupleData == PrimitiveData(3)) shouldEqual false
    val s = new HashSet[Data]()
    s.add(tupleData)
    s.contains(tupleData) shouldEqual true

  }
}