package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy.PrimitiveType._

class PointerSpec extends FlatSpec {
  "A SelfPointer" should "have the right targetType" in {
    SelfPointer(IntPrimitiveType).targetType shouldEqual IntPrimitiveType
  }

  it should "have a SelfPicker" in {
    SelfPointer(IntPrimitiveType).picker shouldEqual SelfPicker(IntPrimitiveType)
  }

  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)

  "A StepPointer" should "have the right targetType" in {
    val ptr = StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 1))
    ptr.targetType shouldEqual StringPrimitiveType
  }

  it should "validate its step during construction" in {
    val structType = StructType("name", "a" -> BooleanPrimitiveType)
    an[IllegalArgumentException] should be thrownBy
      StepPointer(SelfPointer(tupleType), FeatureStep(structType, "a"))
  }

  it should "have the right composite picker" in {
    StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 1)).picker shouldEqual
      AndThenPicker(SelfPicker(tupleType), CoordinatePicker(tupleType, 1))
  }
}