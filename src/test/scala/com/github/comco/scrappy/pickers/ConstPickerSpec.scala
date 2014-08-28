package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy.DataDomain
import com.github.comco.scrappy.DataDomain.PrimitiveData.raw2PrimitiveData
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.OriginatedDataDomain


class ConstPickerSpec extends FlatSpec with Matchers {
  val data = DataDomain.SeqData(1, 2, 3)
  val c = DataDomain.PrimitiveData("hi")
  val const = ConstPicker(data.datatype, c)
  
  "A ConstPicker" should "have the right types" in {
    const.sourceType shouldEqual SeqType(IntPrimitiveType)
    const.targetType shouldEqual StringPrimitiveType
  }
  
  it should "pickData" in {
    const.pickData(data) shouldEqual c
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedDataDomain.mkDataOriginatedFromSelf(data)
    const.pickOriginatedData(originated) shouldEqual
      OriginatedDataDomain.mkDataOriginatedFrom(c, 
          originated.origin.computedWithTargetType(StringPrimitiveType))
  }
}