package com.github.comco.scrappy

import org.scalatest.FlatSpec
import OriginatedDataDomain._
import PrimitiveType._
import org.scalatest.Matchers._
import DataDomain.PrimitiveData._
import com.github.comco.scrappy.OriginatedDataDomain.OriginalSeqData

class OriginatedDataDomainSpec extends FlatSpec {
  val selfIntOrigin = Original(SelfPointer(IntPrimitiveType))
  val selfStringOrigin = Original(SelfPointer(StringPrimitiveType))
  
  val intData: DataDomain.PrimitiveData[Int] = 3
  
  val originatedIntData = PrimitiveData(intData, selfIntOrigin)
  
  "An OriginatedDataDomain.PrimitiveData" should "have the right members" in {
    originatedIntData.data shouldEqual intData
    originatedIntData.origin shouldEqual selfIntOrigin
    originatedIntData.value shouldEqual 3
    originatedIntData.datatype shouldEqual IntPrimitiveType
  }
  
  it should "validate its origin datatype" in {
    an[IllegalArgumentException] should be thrownBy PrimitiveData(intData, selfStringOrigin)
  }
  
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val selfTupleOrigin = Original(SelfPointer(tupleType))
  val tupleData = DataDomain.TupleData(tupleType)(3, "hi")
  val originalTupleData = OriginalTupleData(tupleData, selfTupleOrigin)
  
  "An OriginatedDataDomain.OriginalTupleData" should "have the right members" in {
    originalTupleData.datatype shouldEqual tupleType
    originalTupleData.data shouldEqual tupleData
    originalTupleData.origin shouldEqual selfTupleOrigin
  }
  
  it should "compute the right coordinates" in {
    originalTupleData.coordinate(0) shouldEqual
      PrimitiveData(3, selfTupleOrigin.append(CoordinateStep(tupleType, 0)))
    
    originalTupleData.coordinate(1) shouldEqual
      PrimitiveData("hi", selfTupleOrigin.append(CoordinateStep(tupleType, 1)))
  }
  
  it should "validate its origin datatype" in {
    an[IllegalArgumentException] should be thrownBy OriginalTupleData(tupleData, selfStringOrigin)
  }
  
  val structType = StructType("name", "a" -> IntPrimitiveType, "b" -> StringPrimitiveType)
  val structOrigin = Original(SelfPointer(structType))
  val structData = DataDomain.StructData(structType)("a" -> 3, "b" -> "hi")
  val originalStructData = OriginalStructData(structData, structOrigin)
  
  "An OriginaltedDataDomain.OriginalStructData" should "have the right members" in {
    originalStructData.datatype shouldEqual structType
    originalStructData.data shouldEqual structData
    originalStructData.origin shouldEqual structOrigin
  }
  
  it should "compute the right features" in {
    originalStructData.feature("a") shouldEqual
      PrimitiveData(3, structOrigin.append(FeatureStep(structType, "a")))
    originalStructData.feature("b") shouldEqual
      PrimitiveData("hi", structOrigin.append(FeatureStep(structType, "b")))
  }
  
  it should "validate its origin datatype" in {
    an[IllegalArgumentException] should be thrownBy OriginalStructData(structData, selfStringOrigin)
  }
  
  val seqType = SeqType(structType)
  val seqOrigin = Original(SelfPointer(seqType))
  val seqData = DataDomain.SeqData(structData, structData)
  val originalSeqData = OriginalSeqData(seqData, seqOrigin)
  
  "An OriginatedDataDomain.OriginalSeqData" should "have the right members" in {
    originalSeqData.datatype shouldEqual seqType
    originalSeqData.data shouldEqual seqData
    originalSeqData.origin shouldEqual seqOrigin
  }
  
  it should "compute the right elements" in {
    originalSeqData.element(0) shouldEqual
      OriginalStructData(structData, seqOrigin.append(ElementStep(seqType, 0)))
    
    originalSeqData.element(1) shouldEqual
      OriginalStructData(structData, seqOrigin.append(ElementStep(seqType, 1)))
  }
  
  it should "validate its origin datatype" in {
    an[IllegalArgumentException] should be thrownBy OriginalSeqData(seqData, selfStringOrigin)
  }
  
  val optionType = OptionType(structType)
  val optionOrigin = Original(SelfPointer(optionType))
  val someData = DataDomain.SomeData(optionType, structData)
  val originalSomeData = OriginalSomeData(someData, optionOrigin)
  
  "An OriginatedDataDomain.OriginalSomeData" should "have the right members" in {
    originalSomeData.datatype shouldEqual optionType
    originalSomeData.data shouldEqual someData
    originalSomeData.origin shouldEqual optionOrigin
  }
  
  it should "compute the right value" in {
    originalSomeData.value shouldEqual
      OriginalStructData(structData, optionOrigin.append(SomeStep(optionType)))
  }
}