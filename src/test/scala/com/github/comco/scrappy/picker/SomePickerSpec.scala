package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.SomeStep

final class SomePickerSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)
  val somePicker = SomePicker(optionType)
  val someValue = SomeData(optionType, 3)

  "A SomePicker" should "provide sourceType" in {
    somePicker.sourceType shouldEqual optionType
  }

  it should "provide targetType" in {
    somePicker.targetType shouldEqual IntPrimitiveType
  }

  it should "pickData" in {
    somePicker.pickData(someValue) shouldEqual PrimitiveData(3)
  }

  it should "check for some data in pickData" in {
    itShouldBeDisallowed calling somePicker.pickData(NoneData(optionType))
  }

  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(optionType).append(SomeStep(optionType)))
    val originatedData = OriginatedData.fromSelf(someValue)
    somePicker.pickOriginatedData(originatedData) shouldEqual
      OriginatedData.from(PrimitiveData(3), origin)
  }

  it should "check for some originated data in pickOriginatedData" in {
    val originatedNone = OriginatedData.fromSelf(NoneData(optionType))
    itShouldBeDisallowed calling somePicker.pickOriginatedData(originatedNone)
  }
}