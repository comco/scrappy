package scrappy.pickers

import scrappy.Picker
import scrappy.SeqType
import scrappy.IntType
import scrappy.Value
import scrappy.SeqValue
import scrappy.IntValue

case class IntFoldPicker(f: Seq[Int] => Int) extends Picker {
  val sourceType = SeqType(IntType)
  val resultType = IntType
  
  protected def doPick(source: Value): IntValue = {
    val elems = source.asInstanceOf[SeqValue].elements
    val intElems = elems.map(_.asInstanceOf[IntValue].data)
    val intResult = f(intElems)
    return IntValue(intResult)
  }
}