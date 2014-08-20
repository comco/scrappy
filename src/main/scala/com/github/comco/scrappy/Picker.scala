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
  require(sourceType.hasCoordinate(position))

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