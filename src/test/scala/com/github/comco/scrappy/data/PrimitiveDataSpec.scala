package com.github.comco.scrappy.data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import java.util.HashSet
import com.github.comco.scrappy.TupleType

class PrimitiveDataSpec extends FlatSpec with CustomMatchers {
  val intData = PrimitiveData(3)
  val stringData = PrimitiveData("hi")
  val booleanData = PrimitiveData(false)

  "A PrimitiveData" should "provide datatype" in {
    intData.datatype shouldEqual IntPrimitiveType
    stringData.datatype shouldEqual StringPrimitiveType
    booleanData.datatype shouldEqual BooleanPrimitiveType
  }

  it should "provide value" in {
    intData.value shouldEqual 3
    stringData.value shouldEqual "hi"
    booleanData.value shouldEqual false
  }

  it should "provide implicit conversion from raw values by apply" in {
    import PrimitiveData.apply

    3.datatype shouldEqual IntPrimitiveType
    "hi".datatype shouldEqual StringPrimitiveType
    false.datatype shouldEqual BooleanPrimitiveType
  }
  
  it should "check equality" in {
    (PrimitiveData(3) == PrimitiveData("hi")) shouldEqual false
    (PrimitiveData(3) == TupleData(TupleType(IndexedSeq.empty), IndexedSeq.empty))
    val s = new HashSet[Data]()
    s.add(intData)
    s.contains(intData) shouldEqual true
  }
}