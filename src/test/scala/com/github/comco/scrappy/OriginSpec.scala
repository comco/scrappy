package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.DataDomain._
import com.github.comco.scrappy.DataDomain.PrimitiveData._

class OriginSpec extends FlatSpec with Matchers {
  val pointType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val structType = StructType("name", "a" -> pointType, "b" -> IntPrimitiveType)
  val point = TupleData(pointType)(3, 4)
  val pointer = SelfPointer(structType).append(FeatureStep(structType, "a")).append(CoordinateStep(pointType, 1))
  val struct = StructData(structType)("a" -> point, "b" -> 4)
  val original = Original(pointer)
  
  "An Original origin" should "have the right sourceType" in {
    original.sourceType shouldEqual structType
  }
  
  it should "have the right targetType" in {
    original.targetType shouldEqual IntPrimitiveType
  }
  
  it should "transform ot computed" in {
    original.computed.pointers shouldEqual Set(pointer)
  }
  
  it should "support appending" in {
    val ptr = SelfPointer(structType)
    val orig = Original(ptr)
    val orig2 = orig.append(FeatureStep(structType, "a"))
    orig2.pointer shouldEqual ptr.append(FeatureStep(structType, "a"))
  }
  
  val computed = original.computed
  
  "A Computed origin" should "have the right sourceType" in {
    computed.sourceType shouldEqual structType
  }
  
  it should "have the right targetType" in {
    computed.targetType shouldEqual IntPrimitiveType
  }
  
  it should "stay the same when computed" in {
    computed.computed shouldEqual computed
  }
  
  it should "allow appending by not changing the pointers, just the targetType" in {
    val orig = Original(SelfPointer(structType))
    val comp = orig.computed
    val comp2 = comp.append(FeatureStep(structType, "b"))
    comp2.pointers shouldEqual Set(SelfPointer(structType))
  }
}