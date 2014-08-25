package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType

class StepSpec extends FlatSpec {
	val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
	val coordinateStep0 = CoordinateStep(tupleType, 0)
	val coordinateStep1 = CoordinateStep(tupleType, 1)

	"A CoordinateStep" should "have the right targetType" in {
    coordinateStep0.targetType shouldEqual IntPrimitiveType
    coordinateStep1.targetType shouldEqual StringPrimitiveType
  }
  
	it should "have the right position" in {
	  coordinateStep0.position shouldEqual 0
	  coordinateStep1.position shouldEqual 1
	}
	
  it should "validate for correct position" in {
    an[IllegalArgumentException] should be thrownBy CoordinateStep(tupleType, -1)
    an[IllegalArgumentException] should be thrownBy CoordinateStep(tupleType, 3)
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
}