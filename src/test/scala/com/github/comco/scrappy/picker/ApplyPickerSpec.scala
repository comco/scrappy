package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.origin.ComputedOrigin
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.pointer.SelfPointer

final class ApplyPickerSpec extends FlatSpec with CustomMatchers {
  val inc: Int => Int = (_ + 1)
  val incPicker = ApplyPicker(inc)

  val data = PrimitiveData(3)

  "An ApplyPicker" should "provide sourceType" in {
    incPicker.sourceType shouldEqual IntPrimitiveType
  }

  it should "provide targetType" in {
    incPicker.targetType shouldEqual IntPrimitiveType
  }

  it should "pickData" in {
    incPicker.pickData(data) shouldEqual PrimitiveData(4)
  }

  it should "pickOriginatedData" in {
    val originated = OriginatedData.from(data, OriginalOrigin(SelfPointer(IntPrimitiveType)))
    val origin = OriginatedData.from(PrimitiveData(4),
      ComputedOrigin(IntPrimitiveType, IntPrimitiveType, Set(SelfPointer(IntPrimitiveType))))
    val result = incPicker.pickOriginatedData(originated)
    result shouldEqual origin
  }
}