package scrappy

abstract class Picker {

  def sourceType: Type
  def resultType: Type

  def pick(source: Value): Value = {
    require(source.datatype == sourceType)
    doPick(source)
  } ensuring(_.datatype == resultType)
  
  protected def doPick(source: Value): Value
}