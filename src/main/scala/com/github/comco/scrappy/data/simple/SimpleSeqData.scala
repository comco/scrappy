package com.github.comco.scrappy.data.simple

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape

case class SimpleSeqData[+ElementShape <: Shape.Any: TypeTag](
  val datatype: Type.Seq[ElementShape], val elements: Seq[Data[ElementShape]])
    extends SeqData[ElementShape] {
  // TODO: Invariants
}