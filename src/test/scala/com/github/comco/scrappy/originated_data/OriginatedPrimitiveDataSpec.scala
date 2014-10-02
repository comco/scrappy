package com.github.comco.scrappy.originated_data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer

class OriginatedPrimitiveDataSpec extends FlatSpec with CustomMatchers {
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
  
  "An OriginatedPrimitiveData during construction" should "check its origin datatype" in {
    itShouldBeDisallowed calling OriginatedPrimitiveData(intData, selfStringOrigin)
  }
}