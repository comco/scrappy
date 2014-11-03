package com.github.comco.scrappy.originated_data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.pointer.SelfPointer

final class OriginatedStructDataSpec extends FlatSpec with CustomMatchers {
  val structType = StructType("name",
    "a" -> IntPrimitiveType, "b" -> StringPrimitiveType)
  val selfStructOrigin = OriginalOrigin(SelfPointer(structType))
  val structData = StructData(structType)("a" -> 3, "b" -> "hi")
  val originalStructData = OriginatedStructData.original(structData, selfStructOrigin)

  "An Original OriginatedStructData" should "provide datatype" in {
    originalStructData.datatype shouldEqual structType
  }

  it should "provide data" in {
    originalStructData.data shouldEqual structData
  }

  it should "provide origin" in {
    originalStructData.origin shouldEqual selfStructOrigin
  }

  it should "support checking if a feature name is provided" in {
    originalStructData.isOccupied("a") shouldEqual true
    originalStructData.isOccupied("b") shouldEqual true
    originalStructData.isOccupied("") shouldEqual false
    originalStructData.isOccupied("assd") shouldEqual false
  }

  it should "support retrieving a feature by name" in {
    originalStructData.feature("a") shouldEqual
      OriginatedPrimitiveData(3, selfStructOrigin.append(FeatureStep(structType, "a")))

    originalStructData.feature("b") shouldEqual
      OriginatedPrimitiveData("hi", selfStructOrigin.append(FeatureStep(structType, "b")))
  }

  it should "check the validity of feature names" in {
    itShouldBeDisallowed calling originalStructData.feature("asdas")
  }

  it should "check equality" in {
    val otherStructData = OriginatedData.fromSelf(StructData(structType)("a" -> 3, "b" -> "hoi"))
    (originalStructData == otherStructData) shouldEqual false
    (originalStructData == OriginatedData.fromSelf(PrimitiveData(3))) shouldEqual false

    val s = new HashSet[OriginatedStructData]()
    s.add(originalStructData)
    s.contains(originalStructData) shouldEqual true
  }

  it should "provide apply" in {
    val otherStructData = OriginatedStructData(structData, selfStructOrigin)
    otherStructData shouldEqual originalStructData

    val otherStructData2 = OriginatedStructData(structData, selfStructOrigin, originalStructData.features)
    otherStructData2 shouldEqual originalStructData
  }

  val featureA = OriginatedData.fromSelf(PrimitiveData(3))
  val featureB = OriginatedData.fromSelf(PrimitiveData("hi"))
  val originatedFeatures = Map("a" -> featureA, "b" -> featureB)
  val computedStructData = OriginatedStructData.computed(structData,
    selfStructOrigin, originatedFeatures)

  "A Computed OriginatedStructData" should "provide features" in {
    computedStructData.features shouldEqual originatedFeatures
  }
}