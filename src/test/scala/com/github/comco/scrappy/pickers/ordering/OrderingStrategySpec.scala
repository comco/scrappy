package com.github.comco.scrappy.pickers.ordering

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy.DataDomain.PrimitiveData.raw2PrimitiveData

class OrderingStrategySpec extends FlatSpec with Matchers {
  val tupleType = TupleType(StringPrimitiveType, IntPrimitiveType)

  object TestOrderingStrategy extends OrderingStrategy with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = DataDomain.TupleData
    type SpecificOriginatedData = OriginatedDataDomain.TupleData
    
    def datatype = tupleType

    def doCompareData(x: DataDomain.TupleData,
      y: DataDomain.TupleData): Int = {
      def value(a: DataDomain.TupleData): Int = {
        a.coordinate(1).asInstanceOf[DataDomain.PrimitiveData[Int]].value
      }

      value(x) compare value(y)
    }

    def doCompareOriginatedData(x: OriginatedDataDomain.TupleData,
      y: OriginatedDataDomain.TupleData): Int = {
      def value(a: OriginatedDataDomain.TupleData): Int = {
        a.coordinate(1).asInstanceOf[OriginatedDataDomain.PrimitiveData[Int]].value
      }

      value(x) compare value(y)
    }
  }

  val x = DataDomain.TupleData("ala", 3)
  val y = DataDomain.TupleData("bala", 1)
  val z = DataDomain.PrimitiveData(4)

  "An OrderingStrategy" should "compareData" in {
    TestOrderingStrategy.dataOrdering.compare(x, x) shouldEqual 0
    TestOrderingStrategy.dataOrdering.compare(x, y) shouldEqual 1
    TestOrderingStrategy.dataOrdering.compare(y, x) shouldEqual -1
  }

  val originatedX = OriginatedDataDomain.mkDataOriginatedFromSelf(x)
  val originatedY = OriginatedDataDomain.mkDataOriginatedFromSelf(y)
  val originatedZ = OriginatedDataDomain.mkDataOriginatedFromSelf(z)

  it should "compareOriginatedData" in {
    TestOrderingStrategy.originatedDataOrdering.compare(originatedX, originatedX) shouldEqual 0
    TestOrderingStrategy.originatedDataOrdering.compare(originatedX, originatedY) shouldEqual 1
    TestOrderingStrategy.originatedDataOrdering.compare(originatedY, originatedX) shouldEqual -1
  }

  it should "check its datatypes" in {
    an[IllegalArgumentException] should be thrownBy
      TestOrderingStrategy.dataOrdering.compare(x, z)
    an[IllegalArgumentException] should be thrownBy
      TestOrderingStrategy.dataOrdering.compare(z, y)

    an[IllegalArgumentException] should be thrownBy
      TestOrderingStrategy.originatedDataOrdering.compare(originatedX, originatedZ)
    an[IllegalArgumentException] should be thrownBy
      TestOrderingStrategy.originatedDataOrdering.compare(originatedZ, originatedY)
  }
}