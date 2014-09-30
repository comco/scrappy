package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData

class FoldPickerSpec extends FlatSpec with Matchers {
  val seq = SeqData(3, 4, 5)
  val sum = (as: Seq[Int]) => as.foldLeft(0)(_ + _)
  
  val foldPicker = FoldPicker(sum)
  
  "A FoldPicker" should "have the right sourceType" in {
    foldPicker.sourceType shouldEqual SeqType(IntPrimitiveType)
    foldPicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "pickData" in {
    foldPicker.pickData(seq) shouldEqual PrimitiveData(12)
  }
  
  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(seq)
    foldPicker.pickOriginatedData(originated) shouldEqual
      OriginatedData.from(PrimitiveData(12),
          originated.origin.computedWithTargetType(IntPrimitiveType))
  }
}