package com.github.comco.scrappy.repository

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.PrimitiveType._
import com.github.comco.scrappy._

final class TypeRepositorySpec extends FlatSpec with CustomMatchers {
  var repo = TypeRepository.empty

  val line = StructType("line", "number" -> IntPrimitiveType, "text" -> StringPrimitiveType)
  val page = StructType("page", "lines" -> SeqType(line))
  val document = StructType("document", "pages" -> SeqType(page))
  repo = repo.addType(document)

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

  val otherLine = StructType("line", "a" -> IntPrimitiveType)
  it should "check for conflicts" in {
    a[TypeConflictException] should be thrownBy repo.addType(otherLine)
  }

  it should "support creating using a dsl" in {
    val textRepo = new TypeRepository.Extension {
      'point is 'coordinates -> tuple(int, int)

      'page is (
        'number -> int,
        'text -> string)

      'document is (
        'title -> string,
        'pages -> seq('page))
    }

    val bookRepo = new TypeRepository.Extension(textRepo) {
      'book is (
        'title -> string,
        'author -> string,
        'chapters -> seq('document))
    }

    textRepo.getNamedType("document").isInstanceOf[StructType] shouldBe true
    textRepo.getNamedType("point").asInstanceOf[StructType].featureType("coordinates") shouldEqual
      TupleType(IntPrimitiveType, IntPrimitiveType)
    bookRepo.getNamedType("book").isInstanceOf[StructType] shouldBe true
  }
}