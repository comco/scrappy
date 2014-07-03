package scrappy.pickers

import scrappy.Picker
import scrappy.IntType
import scrappy.SeqType
import scrappy.Value
import scrappy.IntValue
import scrappy.SeqValue

case class IntUnfoldPicker(f: Int => Seq[Int]) extends Picker {
  val sourceType = IntType
  val resultType = SeqType(IntType)
  
  protected def doPick(source: Value): SeqValue = (source: @unchecked) match {
    case IntValue(v) => SeqValue(resultType, f(v).map(IntValue(_)))
  }
}