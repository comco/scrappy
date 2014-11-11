package com.github.comco.scrappy.data

import org.scalatest.FlatSpec

import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.TypeImplicits
import com.github.comco.scrappy.repository.TypeRepository

final class DataImplicitsSpec extends FlatSpec with CustomMatchers {
  object TestDataImplicits extends DataImplicits with TypeImplicits
  import TestDataImplicits._

  implicit val repo = new TypeRepository.Extension {
    'line is ('number -> int, 'contents -> string)
    'page is ('number -> int, 'lines -> seq('line))
    'file is ('title -> string, 'pages -> seq('page))
  }

  "DataImplicits" should "support sequence creation" in {
    val d = seq(1, 2, 3)
    val contents = seq("line 1", "line 2", "line 3", "line 4")
  }

  it should "support struct creation" in {
    val lines = seq(
      'line of ('number -> 1, 'contents -> "line 1"),
      'line of ('number -> 2, 'contents -> "line 2"),
      'line of ('number -> 3, 'contents -> "line 3"))

    lines.element(0).feature('number) shouldEqual PrimitiveData(1)
  }

  it should "support some feature extraction" in {
    some(1).some shouldEqual PrimitiveData(1)
  }
}