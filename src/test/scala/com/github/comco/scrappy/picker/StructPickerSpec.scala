package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.ComputedOrigin
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.SelfPointer

final class StructPickerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val tupleData = TupleData(4, "hi")
  val structType = StructType("str", "a" -> StringPrimitiveType, "b" -> IntPrimitiveType)
  val structPicker = StructPicker(structType)(
    "a" -> CoordinatePicker(tupleType, 1),
    "b" -> CoordinatePicker(tupleType, 0))

  "A StructPicker" should "provide sourceType" in {
    structPicker.sourceType shouldEqual tupleType
  }

  it should "provide targetType" in {
    structPicker.targetType shouldEqual structType
  }

  val expected = StructData(structType)("a" -> "hi", "b" -> 4)

  it should "pickData" in {
    structPicker.pickData(tupleData) shouldEqual expected
  }

  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(tupleData)
    val result = structPicker.pickOriginatedData(originated)

    result.datatype shouldEqual structType
    result.data shouldEqual expected
    result.origin shouldEqual
      ComputedOrigin(tupleType, structType, Set(SelfPointer(tupleType)))

    val struct = result.asInstanceOf[OriginatedStructData]
    struct.feature("a") shouldEqual
      OriginatedPrimitiveData("hi",
        OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 1))))
    struct.feature("b") shouldEqual
      OriginatedPrimitiveData(4,
        OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 0))))
  }

  "A StructPicker during construction" should "check that all the features have the same sourceType" in {
    itShouldBeDisallowed calling
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1),
        "b" -> SelfPicker(StringPrimitiveType))
  }

  it should "check that all the features are given" in {
    itShouldBeDisallowed calling
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1))
  }

  it should "check that all the features have the right names" in {
    itShouldBeDisallowed calling
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1),
        "b" -> CoordinatePicker(tupleType, 0),
        "c" -> CoordinatePicker(tupleType, 0))
  }
}