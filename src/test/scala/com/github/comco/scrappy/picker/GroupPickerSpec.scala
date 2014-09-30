package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.TupleType
import org.scalatest.Matchers
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.TupleData
import org.scalatest.FlatSpec
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.data.PrimitiveData.apply

class GroupPickerSpec extends FlatSpec with Matchers {
  val pointType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val pointsType = SeqType(pointType)
  val resultType = SeqType(TupleType(IntPrimitiveType, pointsType))
  
  val by = CoordinatePicker(pointType, 0)
  val picker = GroupPicker(by)
  
  "A GroupPicker" should "provide sourceType" in {
    picker.sourceType shouldEqual pointsType  
  }
  
  it should "provide targetType" in {
    picker.targetType shouldEqual resultType
  }
  
  def mkPt(i: Int, s: String) = TupleData(i, s)
  val pt1 = mkPt(0, "Hi")
  val pt2 = mkPt(1, "Hello")
  val pt3 = mkPt(0, "Ahoy")
  val pt4 = mkPt(1, "Buy")
  val data = SeqData(pt1, pt2, pt3, pt4)
  def mkTp(i: Int, ss: Data*) = TupleData(i, SeqData(SeqType(ss.head.datatype), ss))
  
  val expectedElements = Set(mkTp(0, pt1, pt3), mkTp(1, pt2, pt4))
  val result = picker.pickData(data).asInstanceOf[SeqData]
  
  it should "pickData" in {
    result.elements.toSet shouldEqual expectedElements
  }
  
  val originatedData = OriginatedData.fromSelf(data)
  
  it should "pickOriginatedData" in {
    val originatedResult = picker.pickOriginatedData(originatedData).asInstanceOf[OriginatedSeqData]
    originatedResult.data shouldEqual result
  }
}