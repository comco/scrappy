package com.github.comco.scrappy

abstract class Picker {
  def sourceType: Type
  def targetType: Type

  def pick(source: DataDomain.Data): DataDomain.Data
  def pickWithOrigin(source: DataWithOriginDomain.Data): DataWithOriginDomain.Data
}

abstract class BaseTuplePicker extends Picker {
  def sourceType: TupleType

  def pick(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPick(source.asInstanceOf[DataDomain.TupleData])
  } ensuring (_.datatype == targetType)

  def doPick(source: DataDomain.TupleData): DataDomain.Data

  def pickWithOrigin(source: DataWithOriginDomain.Data) = {
    require(source.datatype == sourceType)
    doPickWithOrigin(source.asInstanceOf[DataWithOriginDomain.TupleData])
  } ensuring (_.datatype == targetType)

  def doPickWithOrigin(source: DataWithOriginDomain.TupleData): DataWithOriginDomain.Data
}

abstract class BaseStructPicker extends Picker {
  def sourceType: StructType

  def pick(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPick(source.asInstanceOf[DataDomain.StructData])
  } ensuring (_.datatype == targetType)

  def doPick(source: DataDomain.StructData): DataDomain.Data

  def pickWithOrigin(source: DataWithOriginDomain.Data) = {
    require(source.datatype == sourceType)
    doPickWithOrigin(source.asInstanceOf[DataWithOriginDomain.StructData])
  } ensuring (_.datatype == targetType)

  def doPickWithOrigin(source: DataWithOriginDomain.StructData): DataWithOriginDomain.Data
}

abstract class BaseSeqPicker extends Picker {
  def sourceType: SeqType

  def pick(source: DataDomain.Data) = {
    require(source.datatype == sourceType)
    doPick(source.asInstanceOf[DataDomain.SeqData])
  } ensuring (_.datatype == targetType)

  def doPick(source: DataDomain.SeqData): DataDomain.Data

  def pickWithOrigin(source: DataWithOriginDomain.Data) = {
    require(source.datatype == sourceType)
    doPickWithOrigin(source.asInstanceOf[DataWithOriginDomain.SeqData])
  } ensuring (_.datatype == targetType)

  def doPickWithOrigin(source: DataWithOriginDomain.SeqData): DataWithOriginDomain.Data
}

case class SelfPicker(val sourceType: Type) extends Picker {
  def targetType = sourceType

  def pick(source: DataDomain.Data) = source

  def pickWithOrigin(source: DataWithOriginDomain.Data) = source
}

case class CoordinatePicker(val sourceType: TupleType, val position: Int)
    extends BaseTuplePicker {
  require(sourceType.hasCoordinate(position))

  def targetType = sourceType.coordinateType(position)

  def doPick(source: DataDomain.TupleData) = source.coordinates(position)
  def doPickWithOrigin(source: DataWithOriginDomain.TupleData) = source.coordinates(position)
}

case class FeaturePicker(val sourceType: StructType, val name: String)
    extends BaseStructPicker {
  require(sourceType.hasFeature(name))
  
  def targetType = sourceType.featureType(name)
  
  def doPick(source: DataDomain.StructData) = source.features(name)
  def doPickWithOrigin(source: DataWithOriginDomain.StructData) = source.features(name)
}

case class ElementPicker(val sourceType: SeqType, val index: Int)
    extends BaseSeqPicker {
  require(0 <= index)
  
  def targetType = sourceType.elementType
  
  def doPick(source: DataDomain.SeqData) = source.elements(index)
  def doPickWithOrigin(source: DataWithOriginDomain.SeqData) = source.elements(index)
}