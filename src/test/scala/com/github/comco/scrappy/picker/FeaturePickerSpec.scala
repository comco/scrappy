package com.github.comco.scrappy.picker

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.comco.scrappy._
import com.github.comco.scrappy.data.basic.BasicFactory

class FeaturePickerSpec extends FlatSpec with Matchers {
  implicit val dataFactory = BasicFactory
  
  val int = Schema.Primitive[Int]
  val string = Schema.Primitive[String]
  val line = Schema.Struct("line", "number" -> int, "contents" -> string)
  
  val lineNumber = Data.Primitive(3)
  
  val lineData = Data.Struct(line, Map(
      "number" -> lineNumber, 
      "contents" -> Data.Primitive("contents")))
}