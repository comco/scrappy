package com.github.comco.scrappy.originated_data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.SomeStep

class OriginatedSomeDataSpec extends FlatSpec with CustomMatchers {
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
}