package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy._
import PrimitiveType._
import DataDomain._
import PrimitiveData._
import OriginatedDataDomain.mkOriginatedDataFrom

class FilterPickerSpec extends FlatSpec {
  val structType = StructType("name", "a" -> IntPrimitiveType, "b" -> BooleanPrimitiveType)
  val element0 = StructData(structType)("a" -> 3, "b" -> true)
  val element1 = StructData(structType)("a" -> 7, "b" -> false)
  val element2 = StructData(structType)("a" -> 5, "b" -> true)
  val elements = SeqData(element0, element1, element2)
  val filterPicker = FilterPicker(FeaturePicker(structType, "b"))
  
  "A FilterPicker" should "have the right sourceType" in {
    filterPicker.sourceType shouldEqual SeqType(structType)
  }
  
  it should "have the right targetType" in {
    filterPicker.targetType shouldEqual SeqType(structType)
  }
  
  it should "validate for the type of the condition picker" in {
    an[IllegalArgumentException] should be thrownBy FilterPicker(FeaturePicker(structType, "a"))
  }
  
  val expectedData = SeqData(element0, element2)
  
  it should "pickData" in {
    filterPicker.pickData(elements) shouldEqual expectedData
  }
  
  it should "pickOriginatedData by preserving the pointers to the underlying elements" in {
    val pointer = SelfPointer(SeqType(structType))
    val origin = OriginalOrigin(pointer)
    val originatedElements = OriginatedDataDomain.mkOriginatedDataFrom(elements, origin)
    val result = filterPicker.pickOriginatedData(originatedElements)
    result.data shouldEqual expectedData
    result.origin shouldEqual origin.computed
    result.datatype shouldEqual SeqType(structType)
    val seqResult = result.asInstanceOf[OriginatedDataDomain.SeqData]
    val expectedElement0 = mkOriginatedDataFrom(element0, origin.append(ElementStep(SeqType(structType), 0)))
    val expectedElement1 = mkOriginatedDataFrom(element2, origin.append(ElementStep(SeqType(structType), 2)))
    seqResult.element(0) shouldEqual expectedElement0
  }
}