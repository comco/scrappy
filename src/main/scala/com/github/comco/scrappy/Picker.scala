package com.github.comco.scrappy

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker {
  def sourceType: Type
  def targetType: Type

  def pickData(source: DataDomain.Data): DataDomain.Data
  def pickOriginatedData(source: OriginatedDataDomain.Data): OriginatedDataDomain.Data
}

/**
 * Base class for general pickers.
 */
abstract class BasePicker extends Picker {
  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source)
  } ensuring (_.datatype == targetType)
  
  def doPickData(source: DataDomain.Data): DataDomain.Data
  
  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source)
  } ensuring (_.datatype == targetType)
  
  def doPickOriginatedData(source: OriginatedDataDomain.Data): OriginatedDataDomain.Data
}

/**
 * Base class for primitive types pickers.
 */
abstract class BasePrimitivePicker[T] extends Picker {
  def sourceType: PrimitiveType[T]
  
  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[DataDomain.PrimitiveData[T]])
  } ensuring (_.datatype == targetType)
  
  def doPickData(source: DataDomain.PrimitiveData[T]): DataDomain.Data
  
  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedDataDomain.PrimitiveData[T]])
  } ensuring (_.datatype == targetType)
  
  def doPickOriginatedData(source: OriginatedDataDomain.PrimitiveData[T]): OriginatedDataDomain.Data
}

/**
 * Base class for pickers on tuples.
 */
abstract class BaseTuplePicker extends Picker {
  def sourceType: TupleType

  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[DataDomain.TupleData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: DataDomain.TupleData): DataDomain.Data

  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedDataDomain.TupleData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedDataDomain.TupleData): OriginatedDataDomain.Data
}

/**
 * Base class for pickers on structs.
 */
abstract class BaseStructPicker extends Picker {
  def sourceType: StructType

  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[DataDomain.StructData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: DataDomain.StructData): DataDomain.Data

  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedDataDomain.StructData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedDataDomain.StructData): OriginatedDataDomain.Data
}

/**
 * Base class for pickers on seq-s.
 */
abstract class BaseSeqPicker extends Picker {
  def sourceType: SeqType

  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[DataDomain.SeqData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: DataDomain.SeqData): DataDomain.Data

  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedDataDomain.SeqData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedDataDomain.SeqData): OriginatedDataDomain.Data
}

abstract class BaseOptionPicker extends Picker {
  def sourceType: OptionType
  
  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[DataDomain.OptionData])
  } ensuring (_.datatype == targetType)
  
  def doPickData(source: DataDomain.OptionData): DataDomain.Data
  
  def pickOriginatedData(source: OriginatedDataDomain.Data) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedDataDomain.OptionData])
  } ensuring (_.datatype == targetType)
  
  def doPickOriginatedData(source: OriginatedDataDomain.OptionData): OriginatedDataDomain.Data
}

/**
 * Identity picker - picks itself.
 */
case class SelfPicker(val sourceType: Type) extends Picker {
  def targetType = sourceType

  def pickData(source: DataDomain.Data) = {
    require(source.datatype == sourceType,
        s"SelfPicker: $this doesn't support picking data of type: ${source.datatype}")
    source
  }

  def pickOriginatedData(source: OriginatedDataDomain.Data) = source
}

/**
 * Picker for a coordinate of a tuple.
 */
case class CoordinatePicker(val sourceType: TupleType, val position: Int)
    extends BaseTuplePicker {
  require(sourceType.hasCoordinate(position), s"TupleType: $sourceType does not have a coordinate at position: $position")

  def targetType = sourceType.coordinateType(position)

  def doPickData(source: DataDomain.TupleData) = source.coordinates(position)
  def doPickOriginatedData(source: OriginatedDataDomain.TupleData) = source.coordinates(position)
}

/**
 * Picker for a featur of a struct.
 */
case class FeaturePicker(val sourceType: StructType, val name: String)
    extends BaseStructPicker {
  require(sourceType.hasFeature(name))
  
  def targetType = sourceType.featureType(name)
  
  def doPickData(source: DataDomain.StructData) = source.features(name)
  def doPickOriginatedData(source: OriginatedDataDomain.StructData) = source.features(name)
}

/**
 * Picker for an element of a seq.
 */
case class ElementPicker(val sourceType: SeqType, val index: Int)
    extends BaseSeqPicker {
  require(0 <= index)
  
  def targetType = sourceType.elementType
  
  def doPickData(source: DataDomain.SeqData) = source.elements(index)
  def doPickOriginatedData(source: OriginatedDataDomain.SeqData) = source.elements(index)
}

case class SomePicker(val sourceType: OptionType)
  extends BaseOptionPicker {
  
  def targetType = sourceType.someType
  
  def doPickData(source: DataDomain.OptionData) = source match {
    case source: DataDomain.SomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.OptionData) = source match {
    case source: OriginatedDataDomain.SomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }
}

/**
 * Composes pickers of comparable types together.
 */
case class AndThenPicker(val first: Picker, val next: Picker)
    extends Picker {
  require(first.targetType == next.sourceType)
  
  def sourceType = first.sourceType
  def targetType = next.targetType
  
  def pickData(source: DataDomain.Data) = next.pickData(first.pickData(source))
  
  def pickOriginatedData(source: OriginatedDataDomain.Data) = 
    next.pickOriginatedData(first.pickOriginatedData(source))
}