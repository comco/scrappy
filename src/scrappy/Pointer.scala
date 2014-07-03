package scrappy

import scrappy.pickers.SelfPicker
import scrappy.pickers.IndexPicker
import scrappy.pickers.PositionPicker
import scrappy.pickers.FeaturePicker
import scrappy.pickers.JoinPicker
import scrappy.pickers.MapPicker

sealed abstract class StepPointer {
  def originType: Type
  def targetType: Type

  def picker: Picker
}

case class SelfPointer(val originType: Type) extends Pointer {
  def targetType = originType

  def picker = SelfPicker(originType)
}

case class IndexPointer(val originType: SeqType, val index: Int)
    extends StepPointer {
  require(0 <= index)

  def targetType = originType.elementType

  def picker = IndexPicker(originType, index)
}

case class PositionPointer(val originType: TupleType, val position: Int)
    extends StepPointer {
  require(0 <= position && position < originType.coordinateTypes.length)

  def targetType = originType.coordinateTypes(position)

  def picker = PositionPicker(originType, position)
}

case class FeaturePointer(val originType: StructType, val name: String)
    extends StepPointer {
  require(originType.featureTypes.contains(name))

  def targetType = originType.featureTypes(name)

  def picker = FeaturePicker(originType, name)
}

sealed abstract class Pointer {
  def originType: Type
  def targetType: Type

  def picker: Picker
  
  def joinAppend(step: StepPointer) = JoinPointer(step, this)
  def mapAppend(step: StepPointer) = MapPointer(step, this)
  
  def @:(step: StepPointer) = joinAppend(step)
  def /@:(step: StepPointer) = mapAppend(step)
}

case class JoinPointer(val first: StepPointer, val next: Pointer)
    extends Pointer {

  require(first.targetType == next.originType)
  
  def originType = first.originType
  def targetType = next.targetType
  
  def picker = JoinPicker(first.picker, next.picker)
}

case class MapPointer(val first: StepPointer, val next: Pointer)
    extends Pointer {
	
  val elementType = first.targetType.asInstanceOf[SeqType].elementType
	require(first.targetType.isInstanceOf[SeqType] && next.originType == elementType)
  
  val originType = first.originType
  val targetType = SeqType(next.targetType)
  
  def picker = JoinPicker(first.picker, MapPicker(next.picker))
}
