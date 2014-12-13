package com.github.comco.scrappy.originated_data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedNoneData
import com.github.comco.scrappy.pointer.SelfPointer

final class OriginatedNoneDataSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)
  val origin = OriginalOrigin(SelfPointer(optionType))
  val data = NoneData(optionType)
  val originated = OriginatedNoneData.simple(origin)

  "An OriginatedNoneData" should "provide datatype" in {
    originated.datatype shouldEqual optionType
  }

  it should "provide data" in {
    originated.data shouldEqual data
  }

  it should "provide isSome" in {
    originated.isSome shouldEqual false
  }

  it should "check equality" in {
    (OriginatedData.fromSelf(NoneData(OptionType(IntPrimitiveType))) ==
      OriginatedData.fromSelf(SomeData(OptionType(IntPrimitiveType), PrimitiveData(3)))) shouldEqual false
    (OriginatedData.fromSelf(NoneData(OptionType(IntPrimitiveType))) == PrimitiveData(3)) shouldEqual false

    val s = new HashSet[OriginatedNoneData]()
    s.add(originated)
    s.contains(originated) shouldEqual true
  }

  "An OriginatedNoneData during construction" should "check origin" in {
    val intOrigin = OriginalOrigin(SelfPointer(IntPrimitiveType))
    itShouldBeDisallowed calling SimpleOriginatedNoneData(intOrigin)
  }
}