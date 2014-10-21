package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.PrimitiveData

class BaseCheckerSpec extends FlatSpec with CustomMatchers {
  object TestChecker extends BaseChecker {
    val sourceType = IntPrimitiveType
    val targetType = IntPrimitiveType
    
    def doCheckData(source: Data) = CheckResult(true)
    
    def doCheckOriginatedData(source: OriginatedData) =
      OriginatedCheckResult(true, null, null, null)
  }
  
  "A BaseChecker" should "check the type of data" in {
    itShouldBeDisallowed calling TestChecker.checkData(PrimitiveData("hi"))
  }
  
  it should "check the type of originated data" in {
    val data = OriginatedData.fromSelf(PrimitiveData("lala"))
    itShouldBeDisallowed calling TestChecker.checkOriginatedData(data)
  }
}