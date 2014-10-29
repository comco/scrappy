package com.github.comco.scrappy.checker.quantifier

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.checker.EqualChecker
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData

class QuantifierCheckerSpec extends FlatSpec with CustomMatchers {
  val quantifier = ForSomeQuantifier
  val checker = EqualChecker(SelfPicker(IntPrimitiveType), ConstPicker(IntPrimitiveType, PrimitiveData(3)))
  val quantifierChecker = QuantifierChecker(quantifier, checker)
  val okData = SeqData(2, 3, 4)
  val badData = SeqData(4, 4, 5)
  
  "A QuantifierChecker" should "checkData by short-circuting" in {
    val result = quantifierChecker.checkData(okData)
    result.successful shouldEqual true
  }
  
  it should "checkData otherwise" in {
    val result = quantifierChecker.checkData(badData)
    result.successful shouldEqual false
  }
  
  it should "checkOriginatedData by chort-circuting" in {
    val originatedGoodData = OriginatedData.fromSelf(okData)
    val result = quantifierChecker.checkOriginatedData(originatedGoodData)
    result.successful shouldEqual true
    result.checker shouldEqual quantifierChecker
    result.scope shouldEqual originatedGoodData.origin
    result.witnesses shouldEqual Set(checker.checkOriginatedData(originatedGoodData.element(1)))
  }
  
  it should "checkOriginatedData otherwise" in {
    val originatedBadData = OriginatedData.fromSelf(badData)
    val result = quantifierChecker.checkOriginatedData(originatedBadData)
    result.successful shouldEqual false
    result.checker shouldEqual quantifierChecker
    result.scope shouldEqual originatedBadData.origin
    result.witnesses shouldEqual Set.empty
  }
}