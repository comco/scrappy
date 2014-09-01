package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy.DataDomain.PrimitiveData._

class StructPickerSpec extends FlatSpec with Matchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val tupleData = DataDomain.TupleData(4, "hi")
  val structType = StructType("str", "a" -> StringPrimitiveType, "b" -> IntPrimitiveType)
  val structPicker = StructPicker(structType)(
    "a" -> CoordinatePicker(tupleType, 1),
    "b" -> CoordinatePicker(tupleType, 0))

  "A StructPicker" should "have the right sourceType" in {
    structPicker.sourceType shouldEqual tupleType
  }

  it should "have the right targetType" in {
    structPicker.targetType shouldEqual structType
  }

  it should "check that all the features have the same sourceType" in {
    an[IllegalArgumentException] should be thrownBy
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1),
        "b" -> SelfPicker(StringPrimitiveType))
  }

  it should "check that all the features are given" in {
    an[IllegalArgumentException] should be thrownBy
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1))
  }

  it should "check that all the features have the right names" in {
    an[IllegalArgumentException] should be thrownBy
      StructPicker(structType)(
        "a" -> CoordinatePicker(tupleType, 1),
        "b" -> CoordinatePicker(tupleType, 0),
        "c" -> CoordinatePicker(tupleType, 0))
  }

  val expected = DataDomain.StructData(structType)("a" -> "hi", "b" -> 4)

  it should "pickData" in {
    structPicker.pickData(tupleData) shouldEqual expected
  }

  it should "pickOriginatedData" in {
    val originated = OriginatedDataDomain.mkDataOriginatedFromSelf(tupleData)
    val result = structPicker.pickOriginatedData(originated)

    result.datatype shouldEqual structType
    result.data shouldEqual expected
    result.origin shouldEqual
      ComputedOrigin(tupleType, structType, Set(SelfPointer(tupleType)))

    val struct = result.asInstanceOf[OriginatedDataDomain.ComputedStructData]
    struct.feature("a") shouldEqual
      OriginatedDataDomain.PrimitiveData("hi",
        OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 1))))
    struct.feature("b") shouldEqual
      OriginatedDataDomain.PrimitiveData(4,
        OriginalOrigin(SelfPointer(tupleType).append(CoordinateStep(tupleType, 0))))   
  }
}