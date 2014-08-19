package com.github.comco.scrappy

import com.github.comco.scrappy.PrimitiveType._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

object TestDomain extends Domain {
  abstract class Data extends BaseData

  case class PrimitiveData[T](val value: T)(implicit val datatype: PrimitiveType[T])
    extends Data with BasePrimitiveData[T]

  case class TupleData(val datatype: TupleType, val coordinates: IndexedSeq[Data])
    extends Data with BaseTupleData
  
  case class StructData(val datatype: StructType, val features: Map[String, Data])
    extends Data with BaseStructData
    
  case class SeqData(val datatype: SeqType, val elements: Seq[Data])
    extends Data with BaseSeqData
}

class DomainSpec extends FlatSpec {
  import TestDomain._
  
  val pi = PrimitiveData[Int](3)
  val ps = PrimitiveData[String]("hi")
  val pb = PrimitiveData[Boolean](true)
  
  "A PrimitiveData" should "have the right datatype" in {
    Seq(pi, ps, pb).map(_.datatype) shouldEqual Seq(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  }
  
  it should "have have the right data" in {
    Seq(pi, ps, pb).map(_.value) shouldEqual Seq(3, "hi", true)
  }
  
  val tt = TupleType(IntPrimitiveType, StringPrimitiveType)
  val tp = TupleData(tt, IndexedSeq(pi, ps))
  
  "A TupleData" should "have a TupleType datatype" in {
    tp.datatype shouldEqual tt
  }

  it should "have size" in {
    tp.size shouldEqual 2
  }
  
  it should "support getting coordinates out of it" in {
    (0 to 2).map(tt.hasCoordinate(_)) shouldEqual Seq(true, true, false)
    val nullTuple = TupleData(tt, IndexedSeq(pi, null))
    nullTuple.hasCoordinate(1) shouldEqual false
  }
}