package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData

class PredicateCheckerSpec extends FlatSpec with CustomMatchers {
  val pred: (Int => Boolean) = {a : Int => a % 2 == 0}
  val predChecker = PredicateChecker(pred)
  
  "A PredicateChecker" should "checkData" in {
    predChecker.checkData(PrimitiveData(4)).successful shouldEqual true
    predChecker.checkData(PrimitiveData(3)).successful shouldEqual false
  }
  
  it should "checkOriginatedData" in {
    val data1 = OriginatedData.fromSelf(PrimitiveData(1))
    val data2 = OriginatedData.fromSelf(PrimitiveData(2))
    val result1 = predChecker.checkOriginatedData(data1)
    result1.successful shouldEqual false
    result1.scope shouldEqual data1.origin
    result1.checker shouldEqual predChecker
    result1.witnesses shouldEqual Set.empty
    
    val result2 = predChecker.checkOriginatedData(data2)
    result2.successful shouldEqual true
    result2.scope shouldEqual data2.origin
    result2.checker shouldEqual predChecker
    result2.witnesses shouldEqual Set.empty
  }
}