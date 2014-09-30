package com.github.comco.scrappy.picker.ordering.strategies

import com.github.comco.scrappy.picker.ordering.strategy.StringOrderingStrategies
import org.scalatest.Matchers
import com.github.comco.scrappy.originated_data.OriginatedData
import org.scalatest.FlatSpec
import com.github.comco.scrappy.data.PrimitiveData

class StringOrderingStrategySpec extends FlatSpec with Matchers {
  val x = PrimitiveData("ala")
  val y = PrimitiveData("bala")

  "A StringOrderingStrategy.Ascending" should "compare Data.Strings-s" in {
    val ordering = StringOrderingStrategies.Ascending.dataOrdering
    ordering.compare(x, y) shouldEqual -1
    ordering.compare(y, x) shouldEqual 1
    ordering.compare(y, y) shouldEqual 0
  }

  val originatedX = OriginatedData.fromSelf(x)
  val originatedY = OriginatedData.fromSelf(y)

  it should "compare originatedStrings-s" in {
    val ordering = StringOrderingStrategies.Ascending.originatedDataOrdering
    ordering.compare(originatedX, originatedY) shouldEqual -1
    ordering.compare(originatedY, originatedX) shouldEqual 1
    ordering.compare(originatedY, originatedY) shouldEqual 0
  }
}