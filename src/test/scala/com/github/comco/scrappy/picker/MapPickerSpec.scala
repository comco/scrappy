package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.origin._
import com.github.comco.scrappy.pointer._

class MapPickerSpec extends FlatSpec with CustomMatchers {
  val pointType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val points = SeqData(TupleData(3, 4), TupleData(5, 6))
  val firstPicker = CoordinatePicker(pointType, 0)
  val secondPicker = CoordinatePicker(pointType, 1)
  val mapPicker = MapPicker(firstPicker)
  
  "A MapPicker" should "provide sourceType" in {
    mapPicker.sourceType shouldEqual SeqType(pointType)
  }
  
  it should "provide targetType" in {
    mapPicker.targetType shouldEqual SeqType(IntPrimitiveType)
  }
  
  it should "pickData" in {
    mapPicker.pickData(points) shouldEqual SeqData(3, 5)
    MapPicker(secondPicker).pickData(points) shouldEqual SeqData(4, 6)
  }
  
  it should "pickOriginatedData" in {
    val sourcePointer = SelfPointer(SeqType(pointType))
    val originated = OriginatedSeqData.original(points, OriginalOrigin(sourcePointer))
    val results = mapPicker.pickOriginatedData(originated)
    results.datatype shouldEqual SeqType(IntPrimitiveType)
    results.data shouldEqual SeqData(3, 5)
    results.origin shouldEqual ComputedOrigin(SeqType(pointType), SeqType(IntPrimitiveType), Set(sourcePointer))
    val seqResults = results.asInstanceOf[OriginatedSeqData]
    val expectedPointer0 = sourcePointer.append(ElementStep(SeqType(pointType), 0)).append(CoordinateStep(pointType, 0))
    val expectedElement0 = OriginatedPrimitiveData(3, OriginalOrigin(expectedPointer0))
    seqResults.element(0) shouldEqual expectedElement0
  }
}