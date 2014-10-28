package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.picker.SelfPicker

class ForallCheckerSpec extends FlatSpec with CustomMatchers {
  val data = SeqData(3, 3, 3)
  val data2 = SeqData(3, 2, 3)
  val intSeqType = data.datatype
  
  // does some element equal x
  def mkElementChecker(n: Int) = EqualChecker(ConstPicker(IntPrimitiveType, n), SelfPicker(IntPrimitiveType))
  
  val elementChecker3 = mkElementChecker(3)
  
  val checker3 = ForallChecker(elementChecker3)
  
  "A ForallChecker" should "provide sourceType" in {
    checker3.sourceType shouldEqual intSeqType
  }
  
  it should "checkData" in {
    checker3.checkData(data).successful shouldEqual true
    checker3.checkData(data2).successful shouldEqual false  
  }
  
  it should "checkOriginatedData when valid" in {
    val originated = OriginatedData.fromSelf(data)
    val result = checker3.checkOriginatedData(originated)
    result.successful shouldEqual true
    result.scope shouldEqual originated.origin
    result.checker shouldEqual checker3
    result.witnesses shouldEqual Set.empty
  }
  
  it should "checkOriginatedData when invalid" in {
    val originated = OriginatedData.fromSelf(data2)
    val result = checker3.checkOriginatedData(originated)
    result.successful shouldEqual false
    result.scope shouldEqual originated.origin
    result.checker shouldEqual checker3
    result.witnesses shouldEqual Set(elementChecker3.checkOriginatedData(originated.element(1)))
  }
}