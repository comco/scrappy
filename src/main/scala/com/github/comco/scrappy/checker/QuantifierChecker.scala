package com.github.comco.scrappy.checker

import com.github.comco.scrappy.Checker
import com.github.comco.scrappy.Data
import com.github.comco.scrappy.Reason
import com.github.comco.scrappy.Schema
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.utils.quantifier.Quantifier
import com.github.comco.scrappy.utils.quantifier.QuantifierFactory
import com.github.comco.scrappy.Result

case class QuantifierChecker[Element <: Shape.Any](
  val quantifierFactory: QuantifierFactory,
  val elementChecker: Checker[Element])(
    implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BaseChecker[Shape.Sequence[Element]] {
  override def sourceSchema = Schema.Sequence(elementChecker.sourceSchema)

  override def doCheck(source: Data.Sequence[Element]): Result[Shape.Sequence[Element]] = {
    val quantifier = quantifierFactory.createEmpty()
    var witnesses = Set.empty[Reason]
    for (element <- source.elements) {
      val result = elementChecker.check(element)
      quantifier.put(result.successful)
      if (quantifier.unusual) {
        witnesses += result
      }
      if (quantifier.state == Quantifier.Done) {
        return Result(quantifier.valid, source.origin, witnesses, this)
      }
    }
    quantifier.finish()
    return Result(quantifier.valid, source.origin, witnesses, this)
  }
}