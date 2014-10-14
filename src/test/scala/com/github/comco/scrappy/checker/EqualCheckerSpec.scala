package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.data.PrimitiveData._
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

class EqualCheckerSpec extends FlatSpec with CustomMatchers {
  val pointType = TupleType(IntPrimitiveType, IntPrimitiveType)
  val firstPicker = CoordinatePicker(pointType, 0)
  val secondPicker = CoordinatePicker(pointType, 1)
  
  val equalChecker = EqualChecker(firstPicker, secondPicker)
  val dataGood = TupleData(1, 1)
  val dataBad = TupleData(1, 2)
  
  "An EqualChecker" should "provide sourceType" in {
    equalChecker.sourceType shouldEqual pointType
  }
  
  it should "checkData" in {
    equalChecker.checkData(dataGood).successful shouldEqual true
    equalChecker.checkData(dataBad).successful shouldEqual false
  }
  
  it should "checkOriginatedData" in {
    val originatedGoodData = OriginatedData.fromSelf(dataGood).asInstanceOf[OriginatedTupleData]
    val result = equalChecker.checkOriginatedData(originatedGoodData)
    result.successful shouldEqual true
    result.scope shouldEqual originatedGoodData.origin
    result.checker shouldEqual equalChecker
    result.witnesses shouldEqual
      Set(WitnessReason(originatedGoodData.coordinate(0).origin), 
          WitnessReason(originatedGoodData.coordinate(1).origin))
  }
}