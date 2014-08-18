package com.github.comco.scrappy

sealed abstract class Step {
  def sourceType: Type
  def targetType: Type
}

case class CoordinateStep(val sourceType: TupleType, val position: Int)
    extends Step {
  def targetType = sourceType.coordinateType(position)
}

case class FeatureStep(val sourceType: StructType, val name: String)
    extends Step {
  def targetType = sourceType.featureType(name)
}

case class ElementStep(val sourceType: SeqType, val index: Int)
    extends Step {
  def targetType = sourceType.elementType
}