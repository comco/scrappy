package com.github.comco.scrappy.pickers.ordering

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import DataDomain.PrimitiveData.raw2PrimitiveData
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.OriginatedDataDomain._
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.pickers.ordering.strategies.StringOrderingStrategies
import com.github.comco.scrappy.pickers.ordering.strategies.IntOrderingStrategies

class OrderPicker2Spec extends FlatSpec with Matchers {
  val structType = StructType("person",
    "firstName" -> StringPrimitiveType,
    "lastName" -> StringPrimitiveType)
    
  val seqType = SeqType(structType)

  val element0 = DataDomain.StructData(structType)(
      "firstName" -> "John",
      "lastName" -> "Smith")
  val element1 = DataDomain.StructData(structType)(
      "firstName" -> "John",
      "lastName" -> "Adams")
  val element2 = DataDomain.StructData(structType)(
      "firstName" -> "Zebra",
      "lastName" -> "Giraffe")

  val seqData = DataDomain.SeqData(element0, element1, element2)
  val originatedSeqData = OriginatedDataDomain.mkDataOriginatedFromSelf(seqData)

  val firstNamePicker = FeaturePicker(structType, "firstName")
  val lastNamePicker = FeaturePicker(structType, "lastName")
  
  val picker = OrderPicker2(
      firstNamePicker, StringOrderingStrategies.Ascending)(
      lastNamePicker, StringOrderingStrategies.Ascending)
  
  val expectedData = DataDomain.SeqData(element1, element0, element2)
  
  "An OrderPicker2" should "pick by element" in {
    picker.pickData(seqData) shouldEqual expectedData
  }

  it should "pickOriginatedData" in {
    val result = picker.pickOriginatedData(originatedSeqData)
    result.datatype shouldEqual seqType
    result.data shouldEqual expectedData
    val expectedOrigin = OriginalOrigin(SelfPointer(seqType))
    result.origin shouldEqual expectedOrigin.computed

    val seq = result.asInstanceOf[OriginatedDataDomain.SeqData]
    seq.element(0) shouldEqual
      mkOriginatedDataFrom(element1, expectedOrigin.append(ElementStep(seqType, 1)))
    seq.element(1) shouldEqual
      mkOriginatedDataFrom(element0, expectedOrigin.append(ElementStep(seqType, 0)))
    seq.element(2) shouldEqual
      mkOriginatedDataFrom(element2, expectedOrigin.append(ElementStep(seqType, 2)))
  }
  
  it should "check the datatypes of its arguments" in {
    an[IllegalArgumentException] should be thrownBy
      OrderPicker2(firstNamePicker, IntOrderingStrategies.Ascending)(
          lastNamePicker, StringOrderingStrategies.Ascending)
  }
}