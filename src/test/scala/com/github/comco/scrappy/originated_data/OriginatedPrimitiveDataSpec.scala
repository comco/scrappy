package com.github.comco.scrappy.originated_data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer

final class OriginatedPrimitiveDataSpec extends FlatSpec with CustomMatchers {
  val selfIntOrigin = OriginalOrigin(SelfPointer(IntPrimitiveType))
  val selfStringOrigin = OriginalOrigin(SelfPointer(StringPrimitiveType))
  val intData: PrimitiveData[Int] = 3
  val originatedIntData = OriginatedPrimitiveData(intData, selfIntOrigin)

  "An OriginatedPrimitiveData" should "provide data" in {
    originatedIntData.data shouldEqual intData
  }

  it should "provide origin" in {
    originatedIntData.origin shouldEqual selfIntOrigin
  }

  it should "provide value" in {
    originatedIntData.value shouldEqual 3
  }

  it should "provide datatype" in {
    originatedIntData.datatype shouldEqual IntPrimitiveType
  }

  it should "check equality" in {
    def intData(i: Int) = OriginatedData.fromSelf(PrimitiveData(i))
    (intData(3) == intData(4)) shouldEqual false
    (intData(3) == OriginatedData.fromSelf(TupleData(1, 2))) shouldEqual false
    val s = new HashSet[OriginatedData]()
    s.add(intData(3))
    s.contains(intData(3)) shouldEqual true
  }

  "An OriginatedPrimitiveData during construction" should "check its origin datatype" in {
    itShouldBeDisallowed calling OriginatedPrimitiveData(intData, selfStringOrigin)
  }
}