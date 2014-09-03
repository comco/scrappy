package com.github.comco.scrappy.pickers.ordering.strategies

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.pickers.ordering.OrderingStrategy
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType

class StringOrderingStrategySpec extends FlatSpec with Matchers {
  val x = DataDomain.PrimitiveData("ala")
  val y = DataDomain.PrimitiveData("bala")

  "A StringOrderingStrategy.Ascending" should "compare Data.Strings-s" in {
    val ordering = StringOrderingStrategies.Ascending.dataOrdering
    ordering.compare(x, y) shouldEqual -1
    ordering.compare(y, x) shouldEqual 1
    ordering.compare(y, y) shouldEqual 0
  }

  val originatedX = OriginatedDataDomain.mkDataOriginatedFromSelf(x)
  val originatedY = OriginatedDataDomain.mkDataOriginatedFromSelf(y)

  it should "compare originatedStrings-s" in {
    val ordering = StringOrderingStrategies.Ascending.originatedDataOrdering
    ordering.compare(originatedX, originatedY) shouldEqual -1
    ordering.compare(originatedY, originatedX) shouldEqual 1
    ordering.compare(originatedY, originatedY) shouldEqual 0
  }
}