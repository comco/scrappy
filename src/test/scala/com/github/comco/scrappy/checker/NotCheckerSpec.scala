package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData

final class NotCheckerSpec extends FlatSpec with CustomMatchers {
  val simpleChecker = ConstChecker(IntPrimitiveType, true)
  val notChecker = NotChecker(simpleChecker)

  "A NotChecker" should "provide sourceType" in {
    notChecker.sourceType shouldEqual IntPrimitiveType
  }

  it should "checkData" in {
    notChecker.checkData(PrimitiveData(3)).successful shouldEqual false
  }

  it should "checkOriginatedData" in {
    val originated = OriginatedData.fromSelf(PrimitiveData(3))
    val result = notChecker.checkOriginatedData(originated)
    result.successful shouldEqual false
    result.scope shouldEqual originated.origin
    result.checker shouldEqual notChecker
    result.witnesses shouldEqual Set(simpleChecker.checkOriginatedData(originated))
  }
}