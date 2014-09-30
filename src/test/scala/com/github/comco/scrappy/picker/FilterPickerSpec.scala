package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.SelfPointer

class FilterPickerSpec extends FlatSpec with CustomMatchers {
  val structType = StructType("name", "a" -> IntPrimitiveType, "b" -> BooleanPrimitiveType)
  val element0 = StructData(structType)("a" -> 3, "b" -> true)
  val element1 = StructData(structType)("a" -> 7, "b" -> false)
  val element2 = StructData(structType)("a" -> 5, "b" -> true)
  val elements = SeqData(element0, element1, element2)
  val filterPicker = FilterPicker(FeaturePicker(structType, "b"))
  
  "A FilterPicker" should "provide sourceType" in {
    filterPicker.sourceType shouldEqual SeqType(structType)
  }
  
  it should "provide targetType" in {
    filterPicker.targetType shouldEqual SeqType(structType)
  }
  
  
  val expectedData = SeqData(element0, element2)
  
  it should "pickData" in {
    filterPicker.pickData(elements) shouldEqual expectedData
  }
  
  it should "pickOriginatedData by preserving the pointers to the underlying elements" in {
    val pointer = SelfPointer(SeqType(structType))
    val origin = OriginalOrigin(pointer)
    val originatedElements = OriginatedData.from(elements, origin)
    val result = filterPicker.pickOriginatedData(originatedElements)
    result.data shouldEqual expectedData
    result.origin shouldEqual origin.computed
    result.datatype shouldEqual SeqType(structType)
    val seqResult = result.asInstanceOf[OriginatedSeqData]
    val expectedElement0 = OriginatedData.from(element0, origin.append(ElementStep(SeqType(structType), 0)))
    val expectedElement1 = OriginatedData.from(element2, origin.append(ElementStep(SeqType(structType), 2)))
    seqResult.element(0) shouldEqual expectedElement0
  }
  
  "A FilterPicker during construction" should "validate for the type of the condition picker" in {
    itShouldBeDisallowed calling FilterPicker(FeaturePicker(structType, "a"))
  }
}