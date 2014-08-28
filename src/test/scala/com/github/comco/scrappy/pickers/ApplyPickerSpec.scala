package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy.DataDomain.PrimitiveData._
import com.github.comco.scrappy.DataDomain
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.OriginatedDataDomain
import com.github.comco.scrappy._

class ApplyPickerSpec extends FlatSpec {
  val inc: Int => Int = (_ + 1)
  val incPicker = ApplyPicker(inc)
  
  val data = DataDomain.PrimitiveData(3)
  
  "An ApplyPicker" should "have the right types" in {
    incPicker.sourceType shouldEqual IntPrimitiveType
    incPicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "pickData" in {
    incPicker.pickData(data) shouldEqual DataDomain.PrimitiveData(4)
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedDataDomain.mkDataOriginatedFrom(data, Original(SelfPointer(IntPrimitiveType)))
    val result = incPicker.pickOriginatedData(originated)
    result shouldEqual OriginatedDataDomain.mkDataOriginatedFrom(4, Computed(IntPrimitiveType, IntPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
  }
}