package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.PrimitiveType._
import DataDomain.PrimitiveData.raw2PrimitiveData

class GroupPickerSpec extends FlatSpec with Matchers {
  val pointType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val pointsType = SeqType(pointType)
  val resultType = SeqType(TupleType(IntPrimitiveType, pointsType))
  
  val by = CoordinatePicker(pointType, 0)
  val picker = GroupPicker(by)
  
  "A GroupPicker" should "have the right sourceType" in {
    picker.sourceType shouldEqual pointsType  
  }
  
  it should "have the right targetType" in {
    picker.targetType shouldEqual resultType
  }
  
  def mkPt(i: Int, s: String) = DataDomain.TupleData(i, s)
  val pt1 = mkPt(0, "Hi")
  val pt2 = mkPt(1, "Hello")
  val pt3 = mkPt(0, "Ahoy")
  val pt4 = mkPt(1, "Buy")
  val data = DataDomain.SeqData(pt1, pt2, pt3, pt4)
  def mkTp(i: Int, ss: DataDomain.Data*) = DataDomain.TupleData(i, DataDomain.SeqData(SeqType(ss.head.datatype), ss))
  
  val expectedElements = Set(mkTp(0, pt1, pt3), mkTp(1, pt2, pt4))
  val result = picker.pickData(data).asInstanceOf[DataDomain.SeqData]
  
  it should "pickData" in {
    result.elements.toSet shouldEqual expectedElements
  }
  
  val originatedData = OriginatedDataDomain.mkDataOriginatedFromSelf(data)
  
  it should "pickOriginatedData" in {
    val originatedResult = picker.pickOriginatedData(originatedData).asInstanceOf[OriginatedDataDomain.ComputedSeqData]
    originatedResult.data shouldEqual result
  }
}