package com.github.comco.scrappy.originated_data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.pointer.CoordinateStep

class OriginatedTupleDataSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val selfTupleOrigin = OriginalOrigin(SelfPointer(tupleType))
  val tupleData = TupleData(tupleType)(3, "hi")
  val originalTupleData = OriginatedTupleData.original(tupleData, selfTupleOrigin)

  "An Original OriginatedTupleData" should "provide members" in {
    originalTupleData.datatype shouldEqual tupleType
    originalTupleData.data shouldEqual tupleData
    originalTupleData.origin shouldEqual selfTupleOrigin
  }
  
  it should "compute coordinates" in {
    originalTupleData.coordinate(0) shouldEqual
      OriginatedPrimitiveData(3, selfTupleOrigin.append(CoordinateStep(tupleType, 0)))
    
    originalTupleData.coordinate(1) shouldEqual
      OriginatedPrimitiveData("hi", selfTupleOrigin.append(CoordinateStep(tupleType, 1)))
  }
}