package com.github.comco.scrappy.pickers.ordering.strategies

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import DataDomain.PrimitiveData.raw2PrimitiveData

class IntOrderingStrategySpec extends FlatSpec with Matchers {
  val x = DataDomain.PrimitiveData[Int](3)
  val y = DataDomain.PrimitiveData[Int](5)

  "An IntOrderingStrategy.Ascending" should "compare Data.Int-s" in {
    val ordering = IntOrderingStrategies.Ascending.dataOrdering
    ordering.compare(x, y) shouldEqual -1
    ordering.compare(y, x) shouldEqual 1
    ordering.compare(y, y) shouldEqual 0
  }

  val originatedX = OriginatedDataDomain.mkDataOriginatedFromSelf(x)
  val originatedY = OriginatedDataDomain.mkDataOriginatedFromSelf(y)

  it should "compare originatedInt-s" in {
    val ordering = IntOrderingStrategies.Ascending.originatedDataOrdering
    ordering.compare(originatedX, originatedY) shouldEqual -1
    ordering.compare(originatedY, originatedX) shouldEqual 1
    ordering.compare(originatedY, originatedY) shouldEqual 0
  }
}