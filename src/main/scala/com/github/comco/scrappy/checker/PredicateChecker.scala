package com.github.comco.scrappy.checker

import scala.reflect.runtime.universe._
import com.github.comco.scrappy._

case class PredicateChecker[Raw: TypeTag](
  pred: Raw => Boolean)(
    implicit val sourceSchema: Schema.Primitive[Raw])
    extends BaseChecker[Shape.Primitive[Raw]] {
  override def doCheck(source: Data[Shape.Primitive[Raw]]): Result[Shape.Primitive[Raw]] = {
    val successful = pred(source.raw)
    Result(successful, source.origin, Set.empty, this)
  }
}