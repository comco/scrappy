package com.github.comco.scrappy.originated_data

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData.apply
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.origin.OriginalOrigin

class OriginatedDataSpec extends FlatSpec with CustomMatchers {
  "An OriginatedData" should "provide isFilled" in {
    OriginatedData.isFilled(OriginatedData.fromSelf(PrimitiveData(3))) shouldEqual true
    OriginatedData.isFilled(OriginatedData.fromSelf(SomeData(OptionType(IntPrimitiveType), PrimitiveData(3)))) shouldEqual true
    OriginatedData.isFilled(OriginatedData.fromSelf(NoneData(OptionType(IntPrimitiveType)))) shouldEqual false
  }

  it should "provide from with origin" in {
    val primitiveData = PrimitiveData(3)
    OriginatedData.from(primitiveData, OriginalOrigin(SelfPointer(primitiveData.datatype))).data shouldEqual primitiveData
    OriginatedData.from(primitiveData: Data, OriginalOrigin(SelfPointer(primitiveData.datatype))).data shouldEqual primitiveData

    val tupleData = TupleData(1, 2, 3)
    OriginatedData.from(tupleData, OriginalOrigin(SelfPointer(tupleData.datatype))).data shouldEqual tupleData
    OriginatedData.from(tupleData: Data, OriginalOrigin(SelfPointer(tupleData.datatype))).data shouldEqual tupleData

    val structType = StructType("name", "a" -> IntPrimitiveType)
    val structData = StructData(structType)("a" -> 3)
    OriginatedData.from(structData, OriginalOrigin(SelfPointer(structData.datatype))).data shouldEqual structData
    OriginatedData.from(structData: Data, OriginalOrigin(SelfPointer(structData.datatype))).data shouldEqual structData

    val seqData = SeqData(1, 2, 3)
    OriginatedData.from(seqData, OriginalOrigin(SelfPointer(seqData.datatype))).data shouldEqual seqData
    OriginatedData.from(seqData: Data, OriginalOrigin(SelfPointer(seqData.datatype))).data shouldEqual seqData
  }

  it should "provide from with OptionData" in {
    val origin = OriginalOrigin(SelfPointer(OptionType(IntPrimitiveType)))

    OriginatedData.from(SomeData(3): Data, origin) shouldEqual
      OriginatedData.from(SomeData(3), origin)

    OriginatedData.from(NoneData(OptionType(IntPrimitiveType)): Data, origin) shouldEqual
      OriginatedData.from(NoneData(OptionType(IntPrimitiveType)), origin)

    OriginatedData.from(SomeData(3): OptionData, origin) shouldEqual
      OriginatedData.from(SomeData(3), origin)

    OriginatedData.from(NoneData(OptionType(IntPrimitiveType)): OptionData, origin) shouldEqual
      OriginatedData.from(NoneData(OptionType(IntPrimitiveType)), origin)
  }

  it should "provide specialized from-s" in {
    val source = OriginatedData.fromSelf(PrimitiveData(3))
    val tupleData = TupleData(3, 4)
    OriginatedData.from(tupleData, source).origin shouldEqual source.origin.computedWithTargetType(tupleData.datatype)
    OriginatedData.from(tupleData: Data, source).origin shouldEqual source.origin.computedWithTargetType(tupleData.datatype)
    val structType = StructType("name", "a" -> IntPrimitiveType)
    val structData = StructData(structType)("a" -> 2)
    OriginatedData.from(structData, source).origin shouldEqual source.origin.computedWithTargetType(structData.datatype)
    OriginatedData.from(structData: Data, source).origin shouldEqual source.origin.computedWithTargetType(structData.datatype)
    val seqType = SeqType(IntPrimitiveType)
    val seqData = SeqData(seqType)(1, 2, 3)
    OriginatedData.from(seqData, source).origin shouldEqual source.origin.computedWithTargetType(seqData.datatype)
    OriginatedData.from(seqData: Data, source).origin shouldEqual source.origin.computedWithTargetType(seqData.datatype)
    val optionType = OptionType(IntPrimitiveType)
    val someData = SomeData(3)
    OriginatedData.from(someData, source).origin shouldEqual source.origin.computedWithTargetType(someData.datatype)
    OriginatedData.from(someData: Data, source).origin shouldEqual source.origin.computedWithTargetType(someData.datatype)
    val noneData = NoneData(optionType)
    OriginatedData.from(noneData, source).origin shouldEqual source.origin.computedWithTargetType(noneData.datatype)
    OriginatedData.from(noneData: Data, source).origin shouldEqual source.origin.computedWithTargetType(noneData.datatype)

    OriginatedData.from(someData: OptionData, source).origin shouldEqual source.origin.computedWithTargetType(someData.datatype)
    OriginatedData.from(noneData: OptionData, source).origin shouldEqual source.origin.computedWithTargetType(noneData.datatype)
  }

  it should "provide specialized fromSelf-s" in {
    OriginatedData.fromSelf(PrimitiveData(3)).origin shouldEqual OriginalOrigin(SelfPointer(IntPrimitiveType))
    OriginatedData.fromSelf(PrimitiveData(3): Data).origin shouldEqual OriginalOrigin(SelfPointer(IntPrimitiveType))
    val tupleData = TupleData(3, 4)
    OriginatedData.fromSelf(tupleData).origin shouldEqual OriginalOrigin(SelfPointer(tupleData.datatype))
    OriginatedData.fromSelf(tupleData: Data).origin shouldEqual OriginalOrigin(SelfPointer(tupleData.datatype))
    val structType = StructType("name", "a" -> IntPrimitiveType)
    val structData = StructData(structType)("a" -> 3)
    OriginatedData.fromSelf(structData).origin shouldEqual OriginalOrigin(SelfPointer(structType))
    OriginatedData.fromSelf(structData: Data).origin shouldEqual OriginalOrigin(SelfPointer(structType))
    val seqData = SeqData(1, 2, 4)
    OriginatedData.fromSelf(seqData).origin shouldEqual OriginalOrigin(SelfPointer(seqData.datatype))
    OriginatedData.fromSelf(seqData: Data).origin shouldEqual OriginalOrigin(SelfPointer(seqData.datatype))
    val optionType = OptionType(IntPrimitiveType)
    val someData = SomeData(3)
    val noneData = NoneData(optionType)
    OriginatedData.fromSelf(someData).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
    OriginatedData.fromSelf(noneData).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
    OriginatedData.fromSelf(someData: OptionData).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
    OriginatedData.fromSelf(noneData: OptionData).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
    OriginatedData.fromSelf(someData: Data).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
    OriginatedData.fromSelf(noneData: Data).origin shouldEqual OriginalOrigin(SelfPointer(optionType))
  }
}