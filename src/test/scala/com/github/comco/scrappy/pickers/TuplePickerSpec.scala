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
  
  val expectedData = DataDomain.TupleData(4, "3")
  
  it should "pickData" in {
    tuplePicker.pickData(data) shouldEqual expectedData
  }
  
  it should "pickOriginatedData" in {
    // TODO
  }
}