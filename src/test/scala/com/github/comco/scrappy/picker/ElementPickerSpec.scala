package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

class ElementPickerSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = SeqData(seqType)(1, 2, 3)
  val elementPicker = ElementPicker(seqType, 1)

  "An ElementPicker" should "provide sourceType" in {
    elementPicker.sourceType shouldEqual seqType
  }

  it should "provide targetType" in {
    elementPicker.targetType shouldEqual IntPrimitiveType
  }

  it should "pickData" in {
    elementPicker.pickData(seqData) shouldEqual PrimitiveData(2)
  }

  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(seqType).append(ElementStep(seqType, 1)))
    val originatedData = OriginatedData.fromSelf(seqData)
    elementPicker.pickOriginatedData(originatedData) shouldEqual
      OriginatedPrimitiveData(2, origin)
  }

  "An ElementPicker during construction" should "check for non-negative index" in {
    itShouldBeDisallowed calling ElementPicker(seqType, -1)
  }
}