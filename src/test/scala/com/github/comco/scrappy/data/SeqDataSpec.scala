package com.github.comco.scrappy.data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType

import PrimitiveData.apply

class SeqDataSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)
  val seqData = SeqData(seqType)(1, 2, 3)

  val optionType = OptionType(IntPrimitiveType)
  val optionSeqType = SeqType(OptionType(IntPrimitiveType))
  val optionSeqData = SeqData(optionSeqType)(1, NoneData(optionType), 3)
  
  "A SeqData" should "provide elements" in {
    seqData.elements shouldEqual
      Seq(PrimitiveData(1), PrimitiveData(2), PrimitiveData(3))
  }
  
  it should "support checking for occupied elements by isOccupied" in {
    (-1 to 3).map(optionSeqData.isOccupied(_)) shouldEqual
      Seq(false, true, false, true, false)
  }

  "A SeqData during construction" should "check the types of elements" in {
    itShouldBeDisallowed calling
      SomeData(OptionType(IntPrimitiveType), PrimitiveData("hi"))
  }

  it should "support assigning raw values to optional elements" in {
    optionSeqData.element(0) shouldEqual SomeData(1)
  }
}