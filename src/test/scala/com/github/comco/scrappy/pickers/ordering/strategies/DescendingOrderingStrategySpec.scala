package com.github.comco.scrappy.pickers.ordering.strategies

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import DataDomain.PrimitiveData.raw2PrimitiveData
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType

class DescendingOrderingStrategySpec extends FlatSpec with Matchers {
  val x = DataDomain.PrimitiveData[Int](3)
  val y = DataDomain.PrimitiveData[Int](5)

  "An IntOrderingStrategy.Descending" should "compare Data.Int-s" in {
    val ordering = IntOrderingStrategies.Descending.dataOrdering
    ordering.compare(x, y) shouldEqual 1
    ordering.compare(y, x) shouldEqual -1
    ordering.compare(y, y) shouldEqual 0
  }
  
  it should "have the right type" in {
    IntOrderingStrategies.Descending.datatype shouldEqual IntPrimitiveType
  }

  val originatedX = OriginatedDataDomain.mkDataOriginatedFromSelf(x)
  val originatedY = OriginatedDataDomain.mkDataOriginatedFromSelf(y)

  it should "compare originatedInt-s" in {
    val ordering = IntOrderingStrategies.Descending.originatedDataOrdering
    ordering.compare(originatedX, originatedY) shouldEqual 1
    ordering.compare(originatedY, originatedX) shouldEqual -1
    ordering.compare(originatedY, originatedY) shouldEqual 0
  }
}