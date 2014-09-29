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

  val optionSeqType = SeqType(OptionType(IntPrimitiveType))

  "A SeqData" should "provide elements" in {
    seqData.elements shouldEqual
      Seq(PrimitiveData(1), PrimitiveData(2), PrimitiveData(3))
  }

  "A SeqData during construction" should "check the types of elements" in {
    itShouldBeDisallowed calling
      SomeData(OptionType(IntPrimitiveType), PrimitiveData("hi"))
  }

  it should "support assigning raw values to optional elements" in {
    val optionSeqData = SeqData(optionSeqType)(1, 2, 3)
    optionSeqData.element(0) shouldEqual SomeData(1)
  }
}