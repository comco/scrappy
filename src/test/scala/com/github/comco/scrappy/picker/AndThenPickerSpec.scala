package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.SelfPointer

class AndThenPickerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val firstPicker = SelfPicker(tupleType)
  val nextPicker = CoordinatePicker(tupleType, 1)
  val andThenPicker = AndThenPicker(firstPicker, nextPicker)
  val tupleData = TupleData(tupleType)(3, "hi")
  
  "An AndThenPicker" should "provide sourceType" in {
    andThenPicker.sourceType shouldEqual tupleType
  }

  it should "provide targetType" in {
    andThenPicker.targetType shouldEqual StringPrimitiveType
  }

  it should "pickData" in {
    andThenPicker.pickData(tupleData) shouldEqual PrimitiveData("hi")
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(tupleData)
    val origin = OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 1)))
    andThenPicker.pickOriginatedData(originated) shouldEqual
      OriginatedPrimitiveData(PrimitiveData("hi"), origin)
  }

  "An AndThenPicker during construction" should "check that the targetType of " +
    "the first picker is the same as the sourceType of the next picker" in {
      itShouldBeDisallowed calling AndThenPicker(nextPicker, nextPicker)
    }
}