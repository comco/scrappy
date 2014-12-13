package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.Implicits
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.pointer.ElementStep

class ZipWithIndexPickerSpec extends FlatSpec with CustomMatchers {
  import Implicits._

  val cp = int.pick.andThen(const(seq("a", "b", "c")))
  val zip = ZipWithIndexPicker(cp)

  "A ZipWithIndexPicker" should "provide sourceType" in {
    zip.sourceType shouldEqual int
  }

  it should "provide targetType" in {
    zip.targetType shouldEqual seq(tuple(string, int))
  }

  it should "check for the returned type of its argument picker" in {
    itShouldBeDisallowed calling ZipWithIndexPicker(int.pick)
  }

  it should "pickData" in {
    zip.pickData(1) shouldEqual seq(tuple("a", 0), tuple("b", 1), tuple("c", 2))
  }

  it should "pickOriginatedData" in {
    val originated = OriginatedData.fromSelf(PrimitiveData(1))
    val res = zip.pickOriginatedData(originated).asInstanceOf[OriginatedSeqData]
    val targetType = seq(tuple(string, int))
    res.datatype shouldEqual targetType
    res.data shouldEqual seq(tuple("a", 0), tuple("b", 1), tuple("c", 2))
    res.origin shouldEqual originated.origin.computedWithTargetType(targetType)
    res.element(0).origin shouldEqual originated.origin.computedWithTargetType(targetType).append(ElementStep(targetType, 0))
  }
}