package com.github.comco.scrappy.originated_data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

class OriginatedSeqDataSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = SeqData(seqType)(1, 2, 3)
  val originatedSeqData = OriginatedData.fromSelf(seqData)

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
    // TODO: Implement
  }

  it should "provide element" in {

  }

  it should "validate element index" in {

  }

  it should "provide length" in {

  }

  it should "check equals" in {

  }
}