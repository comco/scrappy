package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.SelfPicker

class PickingCheckerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val armPicker = CoordinatePicker(tupleType, 1)
  val childChecker = EqualChecker(
    ConstPicker(IntPrimitiveType, PrimitiveData(3)), SelfPicker(IntPrimitiveType))
  val pickingChecker = PickingChecker(armPicker, childChecker)

  val data = TupleData(1, 3)

  "A PickingChecker" should "provide sourceType" in {
    pickingChecker.sourceType shouldEqual tupleType
  }

  it should "checkData" in {
    pickingChecker.checkData(data).successful shouldEqual true
  }

  it should "checkOriginatedData" in {
    val originated = OriginatedData.fromSelf(data)
    val result = pickingChecker.checkOriginatedData(originated)
    result.successful shouldEqual true
    result.scope shouldEqual originated.origin
    result.checker shouldEqual pickingChecker
    result.witnesses shouldEqual Set(
      childChecker.checkOriginatedData(originated.coordinate(1)))
  }
  
  "A PickingChecker during construction" should "check for compatible picker and checker" in {
    itShouldBeDisallowed calling PickingChecker(SelfPicker(tupleType), childChecker)
  }
}