package com.github.comco.scrappy.picker.ordering

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

final class OrderingStrategySpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(StringPrimitiveType, IntPrimitiveType)

  object TestOrderingStrategy extends OrderingStrategy with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = TupleData
    type SpecificOriginatedData = OriginatedTupleData

    def datatype = tupleType

    def doCompareData(x: TupleData,
      y: TupleData): Int = {
      def value(a: TupleData): Int = {
        a.coordinate(1).asInstanceOf[PrimitiveData[Int]].value
      }

      value(x) compare value(y)
    }

    def doCompareOriginatedData(x: OriginatedTupleData,
      y: OriginatedTupleData): Int = {
      def value(a: OriginatedTupleData): Int = {
        a.coordinate(1).asInstanceOf[OriginatedPrimitiveData[Int]].value
      }

      value(x) compare value(y)
    }
  }

  val x = TupleData("ala", 3)
  val y = TupleData("bala", 1)
  val z = PrimitiveData(4)

  "An OrderingStrategy" should "compareData" in {
    TestOrderingStrategy.dataOrdering.compare(x, x) shouldEqual 0
    TestOrderingStrategy.dataOrdering.compare(x, y) shouldEqual 1
    TestOrderingStrategy.dataOrdering.compare(y, x) shouldEqual -1
  }

  val originatedX = OriginatedData.fromSelf(x)
  val originatedY = OriginatedData.fromSelf(y)
  val originatedZ = OriginatedData.fromSelf(z)

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