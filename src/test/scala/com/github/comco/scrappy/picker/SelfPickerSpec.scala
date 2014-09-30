package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData

class SelfPickerSpec extends FlatSpec with CustomMatchers {
  val selfPicker = SelfPicker(IntPrimitiveType)
  
  "A SelfPicker" should "provide sourceType" in {
    selfPicker.sourceType shouldEqual IntPrimitiveType  
  }
  
  it should "provide targetType" in {
    selfPicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "pickData" in {
    val data = PrimitiveData(3)
    selfPicker.pickData(data) shouldEqual data
  }
  
  it should "check the type of data passed to pickData" in {
    itShouldBeDisallowed calling selfPicker.pickData(PrimitiveData("hi"))
  }
  
  it should "pickOriginatedData" in {
    val originatedData = OriginatedData.fromSelf(PrimitiveData(3))
    selfPicker.pickOriginatedData(originatedData) shouldEqual originatedData
  }
  
  it should "check the type of originated data passed to pickOriginatedData" in {
    val stringOriginatedData = OriginatedData.fromSelf(PrimitiveData("hi"))
    itShouldBeDisallowed calling selfPicker.pickOriginatedData(stringOriginatedData)
  }
}