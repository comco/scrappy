package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.Implicits
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.origin.OriginalOrigin

final class ZipPickerSpec extends FlatSpec with CustomMatchers {
  import Implicits._
  val intPicker = int.andThen(const(seq(1, 2, 3)))
  val stringPicker = string.andThen(const(seq(4, 5, 6)))

  "A ZipPicker" should "check the compatibility of its arguments" in {
    intPicker.sourceType shouldEqual int
    stringPicker.sourceType shouldEqual string

    itShouldBeDisallowed calling ZipPicker(intPicker, stringPicker)
  }

  it should "check that its arguments return sequences" in {
    itShouldBeDisallowed calling ZipPicker(intPicker, int.andThen(const(1)))
  }

  it should "be constructible with compatible pickers" in {
    val zip = ZipPicker(intPicker, const(seq(4, 5, 6)))
    zip.sourceType shouldEqual int
    zip.targetType shouldEqual seq(tuple(int, int))
  }

  it should "pickData" in {
    val pick1 = seq(int).pick
    val pick2 = seq(int).map { x: Int => x * 2 }
    val zip = ZipPicker(pick1, pick2)
    val data = seq(1, 2, 3)
    zip.pickData(data) shouldEqual seq(tuple(1, 2), tuple(2, 4), tuple(3, 6))
  }

  it should "pickOriginatedData" in {
    val pick1 = seq(int).pick
    val pick2 = seq(int).map { x: Int => x * 2 }
    val data = seq(1)
    val originated = OriginatedData.fromSelf(data)
    val zip = ZipPicker(pick1, pick2)
    val res = zip.pickOriginatedData(originated).asInstanceOf[OriginatedSeqData]
    res.datatype shouldEqual seq(tuple(int, int))
    res.data shouldEqual zip.pickData(data)
    val expectedOrigin = originated.origin.computedWithTargetType(seq(tuple(int, int)))
    res.origin shouldEqual expectedOrigin
    res.length shouldEqual 1
    res.element(0).data shouldEqual tuple(1, 2)
    res.element(0).origin shouldEqual expectedOrigin.append(ElementStep(seq(tuple(int, int)), 0))
    val t = res.element(0).asInstanceOf[OriginatedTupleData].coordinate(0)
    t.datatype shouldEqual int
    t.data shouldEqual PrimitiveData(1)
    t.origin shouldEqual originated.origin.append(ElementStep(seq(int), 0))
  }
}