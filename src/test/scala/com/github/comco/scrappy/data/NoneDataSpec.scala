package com.github.comco.scrappy.data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import java.util.HashSet

class NoneDataSpec extends FlatSpec with CustomMatchers {
  val noneData = NoneData(OptionType(IntPrimitiveType))
  
  "A NoneData" should "check equality" in {
    (noneData == PrimitiveData(3)) shouldEqual false
    val s = new HashSet[Data]()
    s.add(noneData)
    s.contains(noneData) shouldEqual true
  }
}