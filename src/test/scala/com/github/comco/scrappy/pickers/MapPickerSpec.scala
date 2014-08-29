package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy._
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import DataDomain._
import DataDomain.PrimitiveData._

class MapPickerSpec extends FlatSpec {
  val pointType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val points = SeqData(TupleData(3, 4), TupleData(5, 6))
  val firstPicker = CoordinatePicker(pointType, 0)
  val secondPicker = CoordinatePicker(pointType, 1)
  val mapPicker = MapPicker(firstPicker)
  
  "A MapPicker" should "have the right sourceType" in {
    mapPicker.sourceType shouldEqual SeqType(pointType)
  }
  
  it should "have the right targetType" in {
    mapPicker.targetType shouldEqual SeqType(IntPrimitiveType)
  }
  
  it should "pickData" in {
    mapPicker.pickData(points) shouldEqual SeqData(3, 5)
    MapPicker(secondPicker).pickData(points) shouldEqual SeqData(4, 6)
  }
  
  it should "pickOriginatedData" in {
    val sourcePointer = SelfPointer(SeqType(pointType))
    val originated = OriginatedDataDomain.OriginalSeqData(points, OriginalOrigin(sourcePointer))
    val results = mapPicker.pickOriginatedData(originated)
    results.datatype shouldEqual SeqType(IntPrimitiveType)
    results.data shouldEqual SeqData(3, 5)
    results.origin shouldEqual ComputedOrigin(SeqType(pointType), SeqType(IntPrimitiveType), Set(sourcePointer))
    val seqResults = results.asInstanceOf[OriginatedDataDomain.SeqData]
    val expectedPointer0 = sourcePointer.append(ElementStep(SeqType(pointType), 0)).append(CoordinateStep(pointType, 0))
    val expectedElement0 = OriginatedDataDomain.PrimitiveData(3, OriginalOrigin(expectedPointer0))
    seqResults.element(0) shouldEqual expectedElement0
  }
} 