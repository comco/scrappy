package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData

class ConstPickerSpec extends FlatSpec with Matchers {
  val data = SeqData(1, 2, 3)
  val c = PrimitiveData("hi")
  val const = ConstPicker(data.datatype, c)
  
  "A ConstPicker" should "have the right types" in {
    const.sourceType shouldEqual SeqType(IntPrimitiveType)
    const.targetType shouldEqual StringPrimitiveType
  }
  
  it should "pickData" in {
    const.pickData(data) shouldEqual c
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(data)
    const.pickOriginatedData(originated) shouldEqual
      OriginatedData.from(c, 
          originated.origin.computedWithTargetType(StringPrimitiveType))
  }
}