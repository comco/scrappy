package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.Implicits
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.origin.ComputedOrigin

final class FlatMapPickerSpec extends FlatSpec with CustomMatchers {
  import Implicits._

  "A FlatMapPicker" should "check the shape of its argument" in {
    itShouldBeDisallowed calling FlatMapPicker(int.pick)
  }

  it should "be constructible" in {
    val fmap = FlatMapPicker(int.andThen(const(seq(1, 2))))
    fmap.sourceType shouldEqual seq(int)
    fmap.targetType shouldEqual seq(int)
  }

  it should "pickData" in {
    val data = seq(1, 2, 3)
    val fmap = FlatMapPicker(int.andThen(const(seq(1, 2))))
    fmap.pickData(data) shouldEqual seq(1, 2, 1, 2, 1, 2)
  }

  it should "pickOriginatedData" in {
    val data = seq(1, 2, 3)
    val originated = OriginatedData.fromSelf(data)
    val fmap = FlatMapPicker(int.andThen(const(seq(1, 2))))
    val res = fmap.pickOriginatedData(originated).asInstanceOf[OriginatedSeqData]
    res.datatype shouldEqual seq(int)
    res.data shouldEqual seq(1, 2, 1, 2, 1, 2)
    res.element(0).origin shouldEqual ComputedOrigin(seq(int), int, Set(seq(int).to.element(0)))
  }
}