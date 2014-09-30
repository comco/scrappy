package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.ComputedOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.pointer.SelfPointer

class TuplePickerSpec extends FlatSpec with CustomMatchers {
  val data = PrimitiveData(3)
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

  val expectedData = TupleData(4, "3")

  it should "pickData" in {
    tuplePicker.pickData(data) shouldEqual expectedData
  }

  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(data)
    val result = tuplePicker.pickOriginatedData(originated)
    result.datatype shouldEqual TupleType(IntPrimitiveType, StringPrimitiveType)
    result.data shouldEqual expectedData
    val tuple = result.asInstanceOf[OriginatedTupleData]
    tuple.coordinate(0) shouldEqual
      OriginatedPrimitiveData(4,
        ComputedOrigin(IntPrimitiveType, IntPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
    tuple.coordinate(1) shouldEqual
      OriginatedPrimitiveData("3",
        ComputedOrigin(IntPrimitiveType, StringPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
  }
}