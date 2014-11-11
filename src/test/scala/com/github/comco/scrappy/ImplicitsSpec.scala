package com.github.comco.scrappy

import org.scalatest.FlatSpec
import com.github.comco.scrappy.repository.TypeRepository

final class ImplicitsSpec extends FlatSpec with CustomMatchers {
  import Implicits._

  // Example of hierarchical type definitions
  val basicRepo = new TypeRepository.Extension {
    'line is ('number -> int, 'contents -> string)
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

  // can freely use repo from now on, since it is implicit

  // Define some data
  val firstPage = 'page of (
    'lines -> seq(
      'line of ('number -> 1, 'contents -> "hello"),
      'line of ('number -> 2, 'contents -> "world")))

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

  // Create some pickers
  val k = 'page.pick.feature('lines)
}