package com.github.comco.scrappy

import org.scalatest.FlatSpec
import OriginatedDataDomain._
import PrimitiveType._

class OriginatedDataDomainSpec extends FlatSpec {
  val origin = Original(SelfPointer(IntPrimitiveType))
  
  "A OriginatedDataDomain.PrimitiveData" should "be constructable from raw data and origin" in {
  }
}