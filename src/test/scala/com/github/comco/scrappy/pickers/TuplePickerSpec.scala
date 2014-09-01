package com.github.comco.scrappy.pickers

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.comco.scrappy._
import com.github.comco.scrappy.DataDomain._
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy.DataDomain.PrimitiveData._

class TuplePickerSpec extends FlatSpec with Matchers {
  val data = DataDomain.PrimitiveData(3)
  val pick0 = ApplyPicker[Int, Int](_ + 1)
  val pick1 = ApplyPicker[Int, String](_.toString)
  
  val tuplePicker = TuplePicker(pick0, pick1)
  
  "A TuplePicker" should "have the right sourceType" in {
    tuplePicker.sourceType shouldEqual IntPrimitiveType
  }
  
  it should "have the right targetType" in {
    tuplePicker.targetType shouldEqual
      TupleType(IntPrimitiveType, StringPrimitiveType)
  }
  
  it should "validate for no coordinates given" in {
    an[IllegalArgumentException] should be thrownBy TuplePicker(IndexedSeq.empty)
  }
  
  it should "check that all the coordinate pickers have the same sourceType" in {
    val pickString = ApplyPicker[String, String](a => a)
    an[IllegalArgumentException] should be thrownBy TuplePicker(pick0, pickString)
  }
  
  val expectedData = DataDomain.TupleData(4, "3")
  
  it should "pickData" in {
    tuplePicker.pickData(data) shouldEqual expectedData
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedDataDomain.mkDataOriginatedFromSelf(data)
    val result = tuplePicker.pickOriginatedData(originated)
    result.datatype shouldEqual TupleType(IntPrimitiveType, StringPrimitiveType)
    result.data shouldEqual expectedData
    val tuple = result.asInstanceOf[OriginatedDataDomain.TupleData]
    tuple.coordinate(0) shouldEqual
      OriginatedDataDomain.PrimitiveData(4, 
          ComputedOrigin(IntPrimitiveType, IntPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
    tuple.coordinate(1) shouldEqual
      OriginatedDataDomain.PrimitiveData("3",
          ComputedOrigin(IntPrimitiveType, StringPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
  }
}