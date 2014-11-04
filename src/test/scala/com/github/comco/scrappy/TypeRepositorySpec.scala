package com.github.comco.scrappy

import org.scalatest.FlatSpec
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType

final class TypeRepositorySpec extends FlatSpec with CustomMatchers {
  val repo = new TypeRepository.Default()
  
  val line = StructType("line", "number" -> IntPrimitiveType, "text" -> StringPrimitiveType)
  repo.registerStructType(line)
  
  val page = StructType("page", "lines" -> SeqType(line))
  repo.registerStructType(page)
  
  val document = StructType("document", "pages" -> SeqType(page))
  repo.registerStructType(document)
  
  val linePageTuple = TupleType(line, page)
  
  val someLineOption = OptionType(line)
  
  val seqLine = SeqType(line)
  
  val other = StructType("other", "a" -> IntPrimitiveType)
  
  "A TypeRepository" should "provide mkString" in {
    repo.mkString(line) shouldEqual "line"
    repo.mkString(someLineOption) shouldEqual "line?"
    repo.mkString(linePageTuple) shouldEqual "(line, page)"
    repo.mkString(seqLine) shouldEqual "[line]"
    repo.mkString(other) shouldEqual "other"
  }
  
  it should "provide getType" in {
    repo.getType("int") shouldEqual IntPrimitiveType
    repo.getType("string") shouldEqual StringPrimitiveType
    repo.getType("boolean") shouldEqual BooleanPrimitiveType
    repo.getType("line") shouldEqual line
    repo.getType("line?") shouldEqual someLineOption
    repo.getType("[line]") shouldEqual seqLine
    repo.getType("(line, page)") shouldEqual linePageTuple
  }
}