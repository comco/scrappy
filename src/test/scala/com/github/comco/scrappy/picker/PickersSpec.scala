package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.TupleData

final class PickersSpec extends FlatSpec with CustomMatchers {
  "Pickers" should "provide a fluent interface" in {
    val data = SeqData(1, 2, 3)
    val seq = data.datatype
    val int = IntPrimitiveType

    import Pickers.dsl._
    val p = seq.map(int.tuple(int, int))
    p.pickData(data) shouldEqual SeqData(TupleData(1, 1), TupleData(2, 2), TupleData(3, 3))
  }
}