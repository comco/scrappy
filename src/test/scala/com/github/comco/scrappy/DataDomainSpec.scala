package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import DataDomain._
import PrimitiveType._
import PrimitiveData.raw2PrimitiveData
    
class DataDomainSpec extends FlatSpec {
  val a = PrimitiveData(3)
  val b = PrimitiveData("hi")
  val c = PrimitiveData(false)
  
  "A DataDomain.PrimitiveData" should "have the right value" in {
    a.value shouldEqual 3
    b.value shouldEqual "hi"
    c.value shouldEqual false
  }
  
  it should "have the right datatype" in {
    a.datatype shouldEqual IntPrimitiveType
    b.datatype shouldEqual StringPrimitiveType
    c.datatype shouldEqual BooleanPrimitiveType
  }
  
  it should "implicitly convert raw values" in {   
    3.datatype shouldEqual IntPrimitiveType
    3.value shouldEqual 3
  }
  
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  val tuple = TupleData(tupleType)(3, "hi", false)
  
  "A DataDomain.TupleData" should "provide the right coordinates" in {
    tuple.coordinates shouldEqual
      Seq(PrimitiveData(3), PrimitiveData("hi"), PrimitiveData(false))
  }
  
  it should "validate the number of coordinates during construction" in {
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)()
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)(3, "hi")
  }
  
  it should "validate the types of coordinates during construction" in {
    an[IllegalArgumentException] should be thrownBy TupleData(tupleType)(3, "hi", "bye")
  }
  
  val structType = StructType("struct", "a" -> IntPrimitiveType, "b" -> tupleType)
  val struct = StructData(structType)("a" -> 3, "b" -> tuple)
  
  "A DataDomain.StructData" should "have the right features" in {
    struct.features shouldEqual Map("a" -> PrimitiveData(3), "b" -> tuple)
  }
  
  it should "validate against constructing with null features" in {
    an[IllegalArgumentException] should be thrownBy struct.feature(null)
  }
  
  it should "validate against constructing with uncompatible features" in {
    an[IllegalArgumentException] should be thrownBy
      StructData(structType)("a" -> 3, "b" -> 4)
  }
  
  it should "validate against wrong feature names" in {
    an[IllegalArgumentException] should be thrownBy
      StructData(structType)("a" -> 4, "none" -> "hi")
  }
  
  val seqType = SeqType(IntPrimitiveType)
  val seq = SeqData(seqType)(1, 2, 3)
  
  "A DataDomain.SeqType" should "have the right elements" in {
    seq.elements shouldEqual Seq(PrimitiveData(1), PrimitiveData(2), PrimitiveData(3))
  }
  
  it should "validate for elements with wrong datatype" in {
    an[IllegalArgumentException] should be thrownBy SeqData(seqType)("hi")
  }
}