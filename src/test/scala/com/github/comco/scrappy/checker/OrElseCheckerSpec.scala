package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.SelfPicker

final class OrElseCheckerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val firstChecker = EqualChecker(
    ConstPicker(tupleType, PrimitiveData(3)),
    CoordinatePicker(tupleType, 0))
  val secondChecker = EqualChecker(
    ConstPicker(tupleType, PrimitiveData("hi")),
    CoordinatePicker(tupleType, 1))
  val thirdChecker = EqualChecker(SelfPicker(IntPrimitiveType), SelfPicker(IntPrimitiveType))
  val orChecker = OrElseChecker(firstChecker, secondChecker)

  "An OrElseChecker" should "provide sourceType" in {
    orChecker.sourceType shouldEqual tupleType
  }

  val dataFalse = TupleData(4, "ho")
  val dataTrue1 = TupleData(3, "ho")
  val dataTrue2 = TupleData(4, "hi")

  it should "checkData" in {
    orChecker.checkData(dataFalse).successful shouldEqual false
    orChecker.checkData(dataTrue1).successful shouldEqual true
    orChecker.checkData(dataTrue2).successful shouldEqual true
  }

  it should "checkOriginatedData when first valid" in {
    val originatedDataTrue1 = OriginatedData.fromSelf(dataTrue1)
    val result = orChecker.checkOriginatedData(originatedDataTrue1)
    result.successful shouldEqual true
    result.scope shouldEqual originatedDataTrue1.origin
    result.checker shouldEqual orChecker
    result.witnesses shouldEqual Set(
      firstChecker.checkOriginatedData(originatedDataTrue1))
  }

  it should "checkOriginatedData when second valid" in {
    val originatedDataTrue2 = OriginatedData.fromSelf(dataTrue2)
    val result = orChecker.checkOriginatedData(originatedDataTrue2)
    result.successful shouldEqual true
    result.scope shouldEqual originatedDataTrue2.origin
    result.checker shouldEqual orChecker
    result.witnesses shouldEqual Set(
      secondChecker.checkOriginatedData(originatedDataTrue2))
  }

  it should "checkOriginatedData when invalid" in {
    val originatedDataFalse = OriginatedData.fromSelf(dataFalse)
    val result = orChecker.checkOriginatedData(originatedDataFalse)
    result.successful shouldEqual false
    result.scope shouldEqual originatedDataFalse.origin
    result.checker shouldEqual orChecker
    result.witnesses shouldEqual Set(
      firstChecker.checkOriginatedData(originatedDataFalse),
      secondChecker.checkOriginatedData(originatedDataFalse))
  }

  "An OrElseChecker during construction" should "check the source types of its arguments" in {
    itShouldBeDisallowed calling OrElseChecker(firstChecker, thirdChecker)
  }
}