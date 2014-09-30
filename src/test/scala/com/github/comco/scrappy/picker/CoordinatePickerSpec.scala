package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

class CoordinatePickerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val coordinatePicker = CoordinatePicker(tupleType, 1)
  val tupleData = TupleData(tupleType)(1, "hi")
  
  "A CoordinatePicker" should "provide sourceType" in {
    coordinatePicker.sourceType shouldEqual tupleType
  }
  
  it should "provide targetType" in {
    coordinatePicker.targetType shouldEqual StringPrimitiveType
  }
  
  it should "pickData" in {
    coordinatePicker.pickData(tupleData) shouldEqual
      PrimitiveData("hi")
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 1)))
    val originatedData = OriginatedData.fromSelf(tupleData)
    coordinatePicker.pickOriginatedData(originatedData) shouldEqual
      OriginatedPrimitiveData(PrimitiveData("hi"), origin)
  }
  
  "A CoordinatePicker during construction" should "check that the source tuple type contains the position" in {
    itShouldBeDisallowed calling CoordinatePicker(tupleType, 2)
  }
}