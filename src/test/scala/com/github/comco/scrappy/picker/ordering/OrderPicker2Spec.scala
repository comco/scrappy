package com.github.comco.scrappy.picker.ordering

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.ordering.strategy.IntOrderingStrategies
import com.github.comco.scrappy.picker.ordering.strategy.StringOrderingStrategies
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

class OrderPicker2Spec extends FlatSpec with CustomMatchers {
  val structType = StructType("person",
    "firstName" -> StringPrimitiveType,
    "lastName" -> StringPrimitiveType)

  val seqType = SeqType(structType)

  val element0 = StructData(structType)(
    "firstName" -> "John",
    "lastName" -> "Smith")
  val element1 = StructData(structType)(
    "firstName" -> "John",
    "lastName" -> "Adams")
  val element2 = StructData(structType)(
    "firstName" -> "Zebra",
    "lastName" -> "Giraffe")

  val seqData = SeqData(element0, element1, element2)
  val originatedSeqData = OriginatedData.fromSelf(seqData)

  val firstNamePicker = FeaturePicker(structType, "firstName")
  val lastNamePicker = FeaturePicker(structType, "lastName")

  val picker = OrderPicker2(
    firstNamePicker, StringOrderingStrategies.Ascending)(
      lastNamePicker, StringOrderingStrategies.Ascending)

  val expectedData = SeqData(element1, element0, element2)

  "An OrderPicker2" should "pick by element" in {
    picker.pickData(seqData) shouldEqual expectedData
  }

  it should "pickOriginatedData" in {
    val result = picker.pickOriginatedData(originatedSeqData)
    result.datatype shouldEqual seqType
    result.data shouldEqual expectedData
    val expectedOrigin = OriginalOrigin(SelfPointer(seqType))
    result.origin shouldEqual expectedOrigin.computed

    val seq = result.asInstanceOf[OriginatedSeqData]
    seq.element(0) shouldEqual
      OriginatedData.from(element1, expectedOrigin.append(ElementStep(seqType, 1)))
    seq.element(1) shouldEqual
      OriginatedData.from(element0, expectedOrigin.append(ElementStep(seqType, 0)))
    seq.element(2) shouldEqual
      OriginatedData.from(element2, expectedOrigin.append(ElementStep(seqType, 2)))
  }

  "An OrderPicker2 during construction" should "check the datatypes of its arguments" in {
    an[IllegalArgumentException] should be thrownBy
      OrderPicker2(firstNamePicker, IntOrderingStrategies.Ascending)(
        lastNamePicker, StringOrderingStrategies.Ascending)
  }

  it should "check that the two by-pickers have the same sourceType" in {
    itShouldBeDisallowed calling
      OrderPicker2(firstNamePicker, IntOrderingStrategies.Ascending)(
        SelfPicker(IntPrimitiveType), IntOrderingStrategies.Ascending)
  }
}