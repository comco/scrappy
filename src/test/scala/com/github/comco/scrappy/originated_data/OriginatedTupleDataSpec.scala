package com.github.comco.scrappy.originated_data

import java.util.HashSet

import scala.IndexedSeq

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.SelfPointer

final class OriginatedTupleDataSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val selfTupleOrigin = OriginalOrigin(SelfPointer(tupleType))
  val tupleData = TupleData(tupleType)(3, "hi")
  val originalTupleData = OriginatedTupleData.original(tupleData, selfTupleOrigin)

  "An Original OriginatedTupleData" should "provide datatype" in {
    originalTupleData.datatype shouldEqual tupleType
  }

  it should "support construction with apply" in {
    val originated2 = OriginatedTupleData(tupleData, selfTupleOrigin)
    originated2 shouldEqual originalTupleData
  }

  it should "provide data" in {
    originalTupleData.data shouldEqual tupleData
  }

  it should "provide origin" in {
    originalTupleData.origin shouldEqual selfTupleOrigin
  }

  it should "provide length" in {
    originalTupleData.length shouldEqual 2
  }

  it should "support checking if a coordinate at a position is occupied" in {
    originalTupleData.isOccupied(0) shouldEqual true
    originalTupleData.isOccupied(1) shouldEqual true
    originalTupleData.isOccupied(2) shouldEqual false
    originalTupleData.isOccupied(-1) shouldEqual false
  }

  it should "support retrieving a coordinate at a position by coordinate method" in {
    originalTupleData.coordinate(0) shouldEqual
      OriginatedPrimitiveData(3, selfTupleOrigin.append(CoordinateStep(tupleType, 0)))

    originalTupleData.coordinate(1) shouldEqual
      OriginatedPrimitiveData("hi", selfTupleOrigin.append(CoordinateStep(tupleType, 1)))
  }

  it should "check the validity of coordinate at a position" in {
    itShouldBeDisallowed calling originalTupleData.coordinate(-1)
    itShouldBeDisallowed calling originalTupleData.coordinate(2)
  }

  it should "check equality" in {
    val otherTupleData = TupleData(tupleType)(3, "hoi")
    (originalTupleData == OriginatedTupleData.original(otherTupleData, selfTupleOrigin)) shouldEqual false
    (originalTupleData == OriginatedData.fromSelf(PrimitiveData(3))) shouldEqual false
    val s = new HashSet[OriginatedTupleData]()
    s.add(originalTupleData)
    s.contains(originalTupleData) shouldEqual true
  }

  val coord1 = OriginatedData.fromSelf(PrimitiveData(3))
  val coord2 = OriginatedData.fromSelf(PrimitiveData("hi"))
  val computedTupleData = OriginatedTupleData.computed(tupleData,
    selfTupleOrigin,
    IndexedSeq(coord1, coord2))

  "A Computed OriginatedTupleData" should "provide coordinates" in {
    computedTupleData.coordinates shouldEqual IndexedSeq(coord1, coord2)
  }

  it should "support construction using apply" in {
    val computed = OriginatedTupleData(tupleData, selfTupleOrigin, originalTupleData.coordinates)
    computed shouldEqual originalTupleData
  }

  "An OriginatedTupleData" should "support pattern-matching" in {
    originalTupleData match {
      case OriginatedTupleData(data, origin, coordinates) =>
        data shouldEqual originalTupleData.data
        origin shouldEqual originalTupleData.origin
        coordinates shouldEqual originalTupleData.coordinates
    }

    computedTupleData match {
      case OriginatedTupleData(data, origin, coordinates) =>
        data shouldEqual computedTupleData.data
        origin shouldEqual computedTupleData.origin
        coordinates shouldEqual computedTupleData.coordinates
    }
  }
}