package com.github.comco.scrappy

import org.scalatest.FlatSpec
import com.github.comco.scrappy.repository.TypeRepository
import com.github.comco.scrappy.data.PrimitiveData

final class ImplicitsSpec extends FlatSpec with CustomMatchers {
  import Implicits._

  // Example of hierarchical type definitions
  val basicRepo = new TypeRepository.Extension {
    'line is ('number -> int, 'contents -> string)
  }

  "A TypeRepository.Extension" should "be usable for definiting types" in {
    basicRepo.getStructType('line).hasFeature("number") shouldBe true
  }

  implicit val repo = new TypeRepository.Extension(basicRepo) {
    'page is ('lines -> seq('line))

    'chapter is (
      'number -> int,
      'title -> string,
      'pages -> seq('page))

    'document is (
      'title -> string,
      'author -> string,
      'chapters -> seq('chapter))
  }

  it should "support extensions" in {
    repo.getStructType('document).featureType("chapters") shouldEqual
      seq('chapter)
  }

  // can freely use repo from now on, since it is implicit

  // Define some data
  val firstPage = 'page of (
    'lines -> seq(
      'line of ('number -> 1, 'contents -> "hello"),
      'line of ('number -> 2, 'contents -> "world")))

  "DataImplicits" should "construct data correctly" in {
    firstPage.feature('lines).element(0).feature('number) shouldEqual PrimitiveData(1)
  }

  val firstChapter = 'chapter of (
    'number -> 1,
    'title -> "Introduction",
    'pages -> seq(firstPage))

  val document = 'document of (
    'title -> "scrappy",
    'author -> "comco",
    'chapters -> seq(firstChapter))

  // Create some pointers to data nodes
  val ptr = 'line.to.feature('number)
  val firstLine = 'line of ('number -> 1, 'contents -> "first")

  "PointerImplicits" should "point to data" in {
    ptr.picker.pickData(firstLine) shouldEqual PrimitiveData(1)
  }

  // Create some pickers
  val k = 'page.pick.feature('lines)
  val mkChapter = seq('page).pick.struct('chapter)(
    'number -> seq('page).const(3),
    'title -> seq('page).const("title"),
    'pages -> seq('page))

  "PickerImplicits" should "pick data" in {
    val doc = mkChapter.pickData(seq(firstPage))
    doc.datatype shouldEqual repo.getStructType('chapter)
  }
}