package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData._
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

class AndAlsoCheckerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val firstChecker = EqualChecker(
    ConstPicker(tupleType, PrimitiveData(3)),
    CoordinatePicker(tupleType, 0))
  val secondChecker = EqualChecker(
    ConstPicker(tupleType, PrimitiveData("hi")),
    CoordinatePicker(tupleType, 1))
  val thirdChecker = EqualChecker(SelfPicker(IntPrimitiveType), SelfPicker(IntPrimitiveType))
  val andChecker = AndAlsoChecker(firstChecker, secondChecker)

  "An AndAlsoChecker" should "provide sourceType" in {
    andChecker.sourceType shouldEqual tupleType
  }

  val dataTrue = TupleData(3, "hi")
  val dataFalse1 = TupleData(4, "hi")
  val dataFalse2 = TupleData(3, "ho")

  it should "checkData" in {
    andChecker.checkData(dataTrue).successful shouldEqual true
    andChecker.checkData(dataFalse1).successful shouldEqual false
    andChecker.checkData(dataFalse2).successful shouldEqual false
  }

  it should "checkOriginatedData when valid" in {
    val originatedDataTrue = OriginatedData.fromSelf(dataTrue).asInstanceOf[OriginatedTupleData]
    val result = andChecker.checkOriginatedData(originatedDataTrue)
    result.successful shouldEqual true
    result.scope shouldEqual originatedDataTrue.origin
    result.checker shouldEqual andChecker
    result.witnesses shouldEqual Set(
      firstChecker.checkOriginatedData(originatedDataTrue),
      secondChecker.checkOriginatedData(originatedDataTrue))
  }
  
  it should "checkOriginatedData when first invalid" in {
    val originatedDataFalse1 = OriginatedData.fromSelf(dataFalse1).asInstanceOf[OriginatedTupleData]
    val result = andChecker.checkOriginatedData(originatedDataFalse1)
    result.successful shouldEqual false
    result.scope shouldEqual originatedDataFalse1.origin
    result.checker shouldEqual andChecker
    result.witnesses shouldEqual Set(
        firstChecker.checkOriginatedData(originatedDataFalse1))
  }
  
  it should "checkOriginatedData when second invalid" in {
    val originatedDataFalse2 = OriginatedData.fromSelf(dataFalse2).asInstanceOf[OriginatedTupleData]
    val result = andChecker.checkOriginatedData(originatedDataFalse2)
    result.successful shouldEqual false
    result.scope shouldEqual originatedDataFalse2.origin
    result.checker shouldEqual andChecker
    result.witnesses shouldEqual Set(
        secondChecker.checkOriginatedData(originatedDataFalse2))
  }

  "An AndAlsoChecker during construction" should "check the source types of its arguments" in {
    itShouldBeDisallowed calling AndAlsoChecker(firstChecker, thirdChecker)
  }
}