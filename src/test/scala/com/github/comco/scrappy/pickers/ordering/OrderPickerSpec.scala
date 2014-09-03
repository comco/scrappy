package com.github.comco.scrappy.pickers.ordering

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import DataDomain.PrimitiveData.raw2PrimitiveData
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.pickers.ordering.strategies.IntOrderingStrategy
import com.github.comco.scrappy.OriginatedDataDomain._

class OrderPickerSpec extends FlatSpec with Matchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = DataDomain.SeqData(3, 2, 4)
  val originatedSeqData = OriginatedDataDomain.mkDataOriginatedFromSelf(seqData)
  val picker = OrderPicker(SelfPicker(IntPrimitiveType), IntOrderingStrategy.Ascending)
  
  "An OrderPicker" should "pick by element" in {
    picker.pickData(seqData) shouldEqual
      DataDomain.SeqData(2, 3, 4)
  }
  
  it should "pickOriginatedData" in {
    val result = picker.pickOriginatedData(originatedSeqData)
    result.datatype shouldEqual SeqType(IntPrimitiveType)
    result.data shouldEqual DataDomain.SeqData(2, 3, 4)
    val expectedOrigin = OriginalOrigin(SelfPointer(seqType))
    result.origin shouldEqual expectedOrigin.computed
    
    val seq = result.asInstanceOf[OriginatedDataDomain.SeqData]
    seq.element(0) shouldEqual
      mkDataOriginatedFrom(2, expectedOrigin.append(ElementStep(seqType, 1)))
    seq.element(1) shouldEqual
      mkDataOriginatedFrom(3, expectedOrigin.append(ElementStep(seqType, 0)))
    seq.element(2) shouldEqual
      mkDataOriginatedFrom(4, expectedOrigin.append(ElementStep(seqType, 2)))
  }
}