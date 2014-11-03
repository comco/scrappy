package com.github.comco.scrappy.originated_data

import java.util.HashSet

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

final class OriginatedSeqDataSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = SeqData(seqType)(1, 2, 3)
  val originatedSeqData = OriginatedData.fromSelf(seqData)
  val optionType = OptionType(IntPrimitiveType)
  val optionSeqType = SeqType(optionType)
  val seqOptionData = SeqData(SomeData(optionType, 1), NoneData(optionType), SomeData(optionType, 3))
  val originatedOptionSeqData = OriginatedData.fromSelf(seqOptionData)

  "An OriginatedSeqData" should "provide datatype" in {
    originatedSeqData.datatype shouldEqual seqType
  }

  it should "provide origin" in {
    originatedSeqData.origin shouldEqual OriginalOrigin(SelfPointer(seqType))
  }

  it should "provide data" in {
    originatedSeqData.data shouldEqual seqData
  }

  it should "provide elements" in {
    val origin = originatedSeqData.origin
    originatedSeqData.elements shouldEqual
      Seq(OriginatedPrimitiveData(PrimitiveData(1), origin.append(ElementStep(seqType, 0))),
        OriginatedPrimitiveData(PrimitiveData(2), origin.append(ElementStep(seqType, 1))),
        OriginatedPrimitiveData(PrimitiveData(3), origin.append(ElementStep(seqType, 2))))
  }

  it should "provide isOccupied" in {
    originatedSeqData.isOccupied(-1) shouldEqual false
    originatedSeqData.isOccupied(0) shouldEqual true
    originatedSeqData.isOccupied(3) shouldEqual false

    originatedOptionSeqData.isOccupied(0) shouldEqual true
    originatedOptionSeqData.isOccupied(1) shouldEqual false
    originatedOptionSeqData.isOccupied(2) shouldEqual true
  }

  it should "provide element" in {
    originatedSeqData.element(0) shouldEqual OriginatedPrimitiveData(1, originatedSeqData.origin.append(ElementStep(seqType, 0)))
    originatedSeqData.element(1) shouldEqual OriginatedPrimitiveData(2, originatedSeqData.origin.append(ElementStep(seqType, 1)))
    originatedSeqData.element(2) shouldEqual OriginatedPrimitiveData(3, originatedSeqData.origin.append(ElementStep(seqType, 2)))

    originatedOptionSeqData.element(0) shouldEqual OriginatedData.from(SomeData(optionType, 1), originatedOptionSeqData.origin.append(ElementStep(optionSeqType, 0)))
    originatedOptionSeqData.element(1) shouldEqual OriginatedData.from(NoneData(optionType), originatedOptionSeqData.origin.append(ElementStep(optionSeqType, 0)))
    originatedOptionSeqData.element(2) shouldEqual OriginatedData.from(SomeData(optionType, 3), originatedOptionSeqData.origin.append(ElementStep(optionSeqType, 2)))
  }

  it should "validate element index" in {
    itShouldBeDisallowed calling originatedSeqData.element(-1)
    itShouldBeDisallowed calling originatedSeqData.element(3)
  }

  it should "provide length" in {
    originatedSeqData.length shouldEqual 3
    originatedOptionSeqData.length shouldEqual 3
  }

  it should "check equals" in {
    (originatedSeqData == SeqData(seqType)(1, 2)) shouldEqual false
    (originatedSeqData == originatedOptionSeqData) shouldEqual false
    (originatedSeqData == OriginatedData.fromSelf(PrimitiveData(3))) shouldEqual false
    val s = new HashSet[OriginatedSeqData]()
    s.add(originatedSeqData)
    s.contains(originatedSeqData) shouldEqual true
  }

  it should "support construction with apply" in {
    OriginatedSeqData(seqData, originatedSeqData.origin) shouldEqual originatedSeqData
  }
}