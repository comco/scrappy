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
    Seq(pi, ps, pb).map(_.datatype) shouldEqual
      Seq(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  }

  it should "have have the right data" in {
    Seq(pi, ps, pb).map(_.value) shouldEqual Seq(3, "hi", true)
  }

  val tt = TupleType(IntPrimitiveType, StringPrimitiveType)
  val tp = TupleData(tt, IndexedSeq(pi, ps))
  val nt = TupleData(tt, IndexedSeq(pi, null))
  
  "A TupleData" should "have a TupleType datatype" in {
    tp.datatype shouldEqual tt
  }

  it should "have size" in {
    tp.size shouldEqual 2
  }

  it should "support checking valid positions" in {
    (0 to 2).map(tt.hasCoordinate(_)) shouldEqual Seq(true, true, false)
    nt.hasCoordinate(1) shouldEqual false
  }

  it should "support extracting coordinates one by one" in {
    (0 until 2).map(tp.coordinate(_)) shouldEqual Seq(pi, ps)
  }
  
  it should "support extracting a sequence of the coordinates" in {
    tp.coordinates shouldEqual Seq(pi, ps)
  }
  
  it should "throw an exception when an invalid position is given" in {
    an[IllegalArgumentException] should be thrownBy tp.coordinate(-1)
    an[IllegalArgumentException] should be thrownBy nt.coordinate(1)
  }
  
  val st = StructType("name", "a" -> IntPrimitiveType, "b" -> StringPrimitiveType)
  val struct = StructData(st, Map("a" -> PrimitiveData(3), "b" -> PrimitiveData("ala")))
  val nullStruct = StructData(st, Map("a" -> PrimitiveData(5), "b" -> null))
  
  
  "A StructData" should "have a StructType datatype" in {
    struct.datatype shouldEqual st
  }
  
  it should "support checking feature names" in {
    Seq("a", "b", null, "none") map (struct.hasFeature(_)) shouldEqual
      Seq(true, true, false, false)
  }
  
  it should "support extracting features by a valid name" in {
    Seq("a", "b") map (struct.feature(_)) shouldEqual
      Seq(PrimitiveData(3), PrimitiveData("ala"))
  }
  
  it should "throw exception when an invalid feature name is given" in {
    an[IllegalArgumentException] should be thrownBy struct.feature(null)
    an[IllegalArgumentException] should be thrownBy struct.feature("none")
    an[IllegalArgumentException] should be thrownBy nullStruct.feature("b")
  }
  
  val seqType = SeqType(IntPrimitiveType)
  val elems = Seq(PrimitiveData(3), PrimitiveData(4))
  val seqValue = SeqData(seqType, elems)
  val nullSeqValue = SeqData(seqType, Seq(PrimitiveData(5), null, PrimitiveData(6)))
  
  "A SeqData" should "have a SeqType datatype" in {
    seqValue.datatype shouldEqual seqType
  }
  
  it should "support extracting a sequence of all the elements" in {
    seqValue.elements shouldEqual elems
  }
  
  it should "have the right length" in {
    seqValue.length shouldEqual 2
  }
  
  it should "allow checking for elements" in {
    Seq(0, 1, -1, 2) map(seqValue.hasElement(_)) shouldEqual
      Seq(true, true, false, false)
    
    nullSeqValue.hasElement(1) shouldEqual false
  }
  
  it should "support extracting elements by a valid index" in {
    seqValue.element(0) shouldEqual PrimitiveData(3)
    seqValue.element(1) shouldEqual PrimitiveData(4)
  }
  
  it should "throw an exception when invalid index is given" in {
    an[IllegalArgumentException] should be thrownBy seqValue.element(-1)
    an[IllegalArgumentException] should be thrownBy nullSeqValue.element(1)
  }
}