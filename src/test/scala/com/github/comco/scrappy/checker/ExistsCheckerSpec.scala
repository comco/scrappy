package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.picker.SelfPicker

final class ExistsCheckerSpec extends FlatSpec with CustomMatchers {
  val data = SeqData(1, 2, 3)
  val intSeqType = data.datatype

  // does some element equal x
  def mkElementChecker(n: Int) = EqualChecker(ConstPicker(n), SelfPicker(IntPrimitiveType))

  val elementChecker3 = mkElementChecker(3)
  val elementChecker4 = mkElementChecker(4)

  val checker3 = ExistsChecker(elementChecker3)
  val checker4 = ExistsChecker(elementChecker4)

  "A ExistsChecker" should "provide sourceType" in {
    checker3.sourceType shouldEqual intSeqType
  }

  it should "checkData" in {
    checker3.checkData(data).successful shouldEqual true
    checker4.checkData(data).successful shouldEqual false
  }

  it should "checkOriginatedData when valid" in {
    val originated = OriginatedData.fromSelf(data)
    val result = checker3.checkOriginatedData(originated)
    result.successful shouldEqual true
    result.scope shouldEqual originated.origin
    result.checker shouldEqual checker3
    result.witnesses shouldEqual Set(elementChecker3.checkOriginatedData(originated.element(2)))
  }

  it should "checkOriginatedData when invalid" in {
    val originated = OriginatedData.fromSelf(data)
    val result = checker4.checkOriginatedData(originated)
    result.successful shouldEqual false
    result.scope shouldEqual originated.origin
    result.checker shouldEqual checker4
    result.witnesses shouldEqual Set.empty
  }
}