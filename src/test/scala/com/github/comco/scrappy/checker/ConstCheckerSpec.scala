package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData

final class ConstCheckerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val checkerTrue = ConstChecker(tupleType, true)
  val checkerFalse = ConstChecker(tupleType, false)

  "A ConstChecker" should "provide sourceType" in {
    checkerTrue.sourceType shouldEqual tupleType
  }

  it should "checkData" in {
    checkerTrue.checkData(TupleData(1, 1)).successful shouldEqual true
    checkerFalse.checkData(TupleData(1, 1)).successful shouldEqual false
  }

  it should "checkOriginatedData" in {
    val data = OriginatedData.fromSelf(TupleData(1, 1))
    checkerTrue.checkOriginatedData(data) shouldEqual
      OriginatedCheckResult(true, data.origin, Set.empty, checkerTrue)

    checkerFalse.checkOriginatedData(data) shouldEqual
      OriginatedCheckResult(false, data.origin, Set.empty, checkerFalse)
  }
}