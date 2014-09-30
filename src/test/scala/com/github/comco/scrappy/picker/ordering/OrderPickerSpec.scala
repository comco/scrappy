package com.github.comco.scrappy.picker.ordering

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.ordering.strategy.IntOrderingStrategies
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

class OrderPickerSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = SeqData(3, 2, 4)
  val originatedSeqData = OriginatedData.fromSelf(seqData)
  val picker = OrderPicker(SelfPicker(IntPrimitiveType), IntOrderingStrategies.Ascending)
  
  "An OrderPicker" should "pick by element" in {
    picker.pickData(seqData) shouldEqual SeqData(2, 3, 4)
  }
  
  it should "pickOriginatedData" in {
    val result = picker.pickOriginatedData(originatedSeqData)
    result.datatype shouldEqual SeqType(IntPrimitiveType)
    result.data shouldEqual SeqData(2, 3, 4)
    val expectedOrigin = OriginalOrigin(SelfPointer(seqType))
    result.origin shouldEqual expectedOrigin.computed
    
    val seq = result.asInstanceOf[OriginatedSeqData]
    seq.element(0) shouldEqual
      OriginatedData.from(2, expectedOrigin.append(ElementStep(seqType, 1)))
    seq.element(1) shouldEqual
      OriginatedData.from(3, expectedOrigin.append(ElementStep(seqType, 0)))
    seq.element(2) shouldEqual
      OriginatedData.from(4, expectedOrigin.append(ElementStep(seqType, 2)))
  }
}