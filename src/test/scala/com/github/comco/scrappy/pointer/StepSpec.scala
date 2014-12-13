package com.github.comco.scrappy.pointer

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.SomePicker

final class StepSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val coordinateStep0 = CoordinateStep(tupleType, 0)
  val coordinateStep1 = CoordinateStep(tupleType, 1)

  "A Step" should "provide mkPointer" in {
    coordinateStep0.mkPointer shouldEqual StepPointer(SelfPointer(tupleType), coordinateStep0)
  }

  "A CoordinateStep" should "have the right targetType" in {
    coordinateStep0.targetType shouldEqual IntPrimitiveType
    coordinateStep1.targetType shouldEqual StringPrimitiveType
  }

  it should "have the right position" in {
    coordinateStep0.position shouldEqual 0
    coordinateStep1.position shouldEqual 1
  }

  it should "validate for correct position" in {
    itShouldBeDisallowed calling CoordinateStep(tupleType, -1)
    itShouldBeDisallowed calling CoordinateStep(tupleType, 3)
  }

  it should "have a CoordinatePicker" in {
    coordinateStep0.picker shouldEqual CoordinatePicker(tupleType, 0)
    coordinateStep1.picker shouldEqual CoordinatePicker(tupleType, 1)
  }

  val structType = StructType("name", "a" -> IntPrimitiveType, "b" -> BooleanPrimitiveType)
  val featureStepA = FeatureStep(structType, "a")
  val featureStepB = FeatureStep(structType, "b")

  "A FeatureStep" should "have the right targetType" in {
    featureStepA.targetType shouldEqual IntPrimitiveType
    featureStepB.targetType shouldEqual BooleanPrimitiveType
  }

  it should "have the right name" in {
    featureStepA.name shouldEqual "a"
    featureStepB.name shouldEqual "b"
  }

  it should "validate for correct name" in {
    an[IllegalArgumentException] should be thrownBy FeatureStep(structType, "c")
  }

  it should "have a FeaturePicker" in {
    featureStepA.picker shouldEqual FeaturePicker(structType, "a")
    featureStepB.picker shouldEqual FeaturePicker(structType, "b")
  }

  val seqType = SeqType(IntPrimitiveType)
  val seqStep = ElementStep(seqType, 3)

  "An ElementStep" should "have the right datatype" in {
    seqStep.targetType shouldEqual IntPrimitiveType
  }

  it should "have the right index" in {
    seqStep.index shouldEqual 3
  }

  it should "validate for correct indices" in {
    an[IllegalArgumentException] should be thrownBy ElementStep(seqType, -3)
  }

  it should "have an ElementPicker" in {
    seqStep.picker shouldEqual ElementPicker(seqType, 3)
  }

  "A SomeStep" should "have the right picker" in {
    SomeStep(OptionType(seqType)).picker shouldEqual SomePicker(OptionType(seqType))
  }

  it should "provide targetType" in {
    SomeStep(OptionType(seqType)).targetType shouldEqual seqType
  }
}