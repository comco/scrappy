package com.github.comco.scrappy.pickers

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy.DataDomain.PrimitiveData.raw2PrimitiveData
import com.github.comco.scrappy.OriginatedDataDomain.mkDataOriginatedFromSelf
import com.github.comco.scrappy.OriginatedDataDomain.mkOriginatedDataFrom

class FoldPickerSpec extends FlatSpec with Matchers {
  val seq = DataDomain.SeqData(3, 4, 5)
  val sum = (as: Seq[Int]) => as.foldLeft(0)(_ + _)
  
  val foldPicker = FoldPicker(sum)
  
  "A FoldPicker" should "have the right sourceType" in {
    foldPicker.sourceType shouldEqual SeqType(IntPrimitiveType)
    foldPicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "pickData" in {
    foldPicker.pickData(seq) shouldEqual DataDomain.PrimitiveData(12)
  }
  
  it should "pickOriginatedData" in {
    val originated = mkDataOriginatedFromSelf(seq)
    foldPicker.pickOriginatedData(originated) shouldEqual
      mkOriginatedDataFrom(DataDomain.PrimitiveData(12),
          originated.origin.computedWithTargetType(IntPrimitiveType))
  }
}