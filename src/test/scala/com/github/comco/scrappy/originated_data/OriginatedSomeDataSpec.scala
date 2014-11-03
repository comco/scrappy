package com.github.comco.scrappy.originated_data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.SomeStep

final class OriginatedSomeDataSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)
  val selfOptionOrigin = OriginalOrigin(SelfPointer(optionType))
  val optionData = SomeData(3)
  val originalOptionData = OriginatedSomeData.original(optionData, selfOptionOrigin)

  "An Original OriginatedSomeData" should "provide datatype" in {
    originalOptionData.datatype shouldEqual optionType
  }

  it should "provide data" in {
    originalOptionData.data shouldEqual optionData
  }

  it should "provide origin" in {
    originalOptionData.origin shouldEqual selfOptionOrigin
  }

  it should "provide value" in {
    originalOptionData.value shouldEqual OriginatedPrimitiveData(3, selfOptionOrigin.append(SomeStep(optionType)))
  }

  it should "be some" in {
    originalOptionData.isSome shouldEqual true
  }

  it should "check equality" in {
    (originalOptionData == OriginatedData.fromSelf(PrimitiveData(3))) shouldEqual false
    (originalOptionData == OriginatedData.fromSelf(NoneData(OptionType(IntPrimitiveType)))) shouldEqual false
    val s = new HashSet[OriginatedOptionData]()
    s.add(originalOptionData)
    s.contains(originalOptionData) shouldEqual true
  }

  val valueData = originalOptionData.asInstanceOf[OriginatedSomeData].value
  val computedSomeData = OriginatedSomeData.computed(optionData, selfOptionOrigin, valueData)

  "A ComputedSomeData" should "provide members" in {
    computedSomeData.data shouldEqual optionData
    computedSomeData.origin shouldEqual selfOptionOrigin
    computedSomeData.value shouldEqual valueData
  }
}