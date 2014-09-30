package com.github.comco.scrappy.picker.ordering.strategies

import org.scalatest.Matchers
import com.github.comco.scrappy.picker.ordering.strategy.IntOrderingStrategies
import com.github.comco.scrappy.data.PrimitiveData
import org.scalatest.FlatSpec
import com.github.comco.scrappy.originated_data.OriginatedData

class IntOrderingStrategiesSpec extends FlatSpec with Matchers {
  val x = PrimitiveData[Int](3)
  val y = PrimitiveData[Int](5)

  "An IntOrderingStrategy.Ascending" should "compare Data.Int-s" in {
    val ordering = IntOrderingStrategies.Ascending.dataOrdering
    ordering.compare(x, y) shouldEqual -1
    ordering.compare(y, x) shouldEqual 1
    ordering.compare(y, y) shouldEqual 0
  }

  val originatedX = OriginatedData.fromSelf(x)
  val originatedY = OriginatedData.fromSelf(y)

  it should "compare originatedInt-s" in {
    val ordering = IntOrderingStrategies.Ascending.originatedDataOrdering
    ordering.compare(originatedX, originatedY) shouldEqual -1
    ordering.compare(originatedY, originatedX) shouldEqual 1
    ordering.compare(originatedY, originatedY) shouldEqual 0
  }
}