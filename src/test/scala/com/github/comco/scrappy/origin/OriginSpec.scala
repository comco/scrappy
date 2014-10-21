package com.github.comco.scrappy.origin

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.pointer.SelfPointer

class OriginSpec extends FlatSpec with CustomMatchers {
  val pointType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val structType = StructType("name", "a" -> pointType, "b" -> IntPrimitiveType)
  val point = TupleData(pointType)(3, 4)
  val pointer = SelfPointer(structType).append(FeatureStep(structType, "a")).append(CoordinateStep(pointType, 1))
  val struct = StructData(structType)("a" -> point, "b" -> 4)
  val original = OriginalOrigin(pointer)
  
  "An Origin" should "provide merge" in {
    val result = original.merge(original)
    result.sourceType shouldEqual original.sourceType
    result.targetType shouldEqual original.targetType
    result.pointers shouldEqual Set(original.pointer, original.pointer)
  }
  
   "An Original origin" should "provide sourceType" in {
    original.sourceType shouldEqual structType
  }
  
  it should "provide targetType" in {
    original.targetType shouldEqual IntPrimitiveType
  }
  
  it should "transform to computed" in {
    original.computed.pointers shouldEqual Set(pointer)
  }
  
  it should "provide pointers" in {
    original.pointers shouldEqual Set(pointer)
  }
  
  it should "support appending" in {
    val ptr = SelfPointer(structType)
    val orig = OriginalOrigin(ptr)
    val orig2 = orig.append(FeatureStep(structType, "a"))
    orig2.pointer shouldEqual ptr.append(FeatureStep(structType, "a"))
  }
  
  it should "validate for incorrect steps" in {
    itShouldBeDisallowed calling original.append(CoordinateStep(pointType, 1))
  }
  
  it should "change its targetType when computedWithTargetType" in {
    val result = original.computedWithTargetType(IntPrimitiveType)
    result.sourceType shouldEqual original.sourceType
    result.targetType shouldEqual IntPrimitiveType
  }
  
  val computed = original.computed
  
  "A ComputedOrigin origin" should "have the right sourceType" in {
    computed.sourceType shouldEqual structType
  }
  
  it should "have the right targetType" in {
    computed.targetType shouldEqual IntPrimitiveType
  }
  
  it should "stay the same when computed" in {
    computed.computed shouldEqual computed
  }
  
  it should "allow appending by not changing the pointers, just the targetType" in {
    val orig = OriginalOrigin(SelfPointer(structType))
    val comp = orig.computed
    val comp2 = comp.append(FeatureStep(structType, "b"))
    comp2.pointers shouldEqual Set(SelfPointer(structType))
  }
  
  it should "validate for incorrect steps" in {
    an[IllegalArgumentException] should be thrownBy computed.append(CoordinateStep(pointType, 1))
  }
  
  it should "change its targetType when computedWithTargetType" in {
   computed.computedWithTargetType(IntPrimitiveType) .targetType shouldEqual IntPrimitiveType
  }
}