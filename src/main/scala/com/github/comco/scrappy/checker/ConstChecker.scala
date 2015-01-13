package com.github.comco.scrappy.checker

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Schema
import com.github.comco.scrappy.Result
import com.github.comco.scrappy.Data

/**
 * Returns the same check result every time.
 */
case class ConstChecker[Shape <: Shape.Any](
  val sourceSchema: Schema[Shape],
  val successful: Boolean)
    extends BaseChecker[Shape] {
  override def doCheck(source: Data[Shape]): Result[Shape] = {
    Result(successful, source.origin, Set.empty, this)
  }
}