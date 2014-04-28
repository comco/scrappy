package scrappy

sealed trait Type

case object IntType extends Type {
  def apply(data: Int) = Values.IntData(data)
}

case object StringType extends Type {
  def apply(data: String) = Values.StringData(data)
}

case object BooleanType extends Type {
  def apply(data: Boolean) = Values.BooleanData(data)
}

case class TupleType(val coordinateTypes: IndexedSeq[Type]) extends Type {
  def apply(coordinates: Values.Data*) = 
    Values.TupleData(this, coordinates.toIndexedSeq)
  
  def $(coordinate: Int) = CoordinateSelector(this, coordinate)
}

object TupleType {
  def apply(coordinateTypes: Type*) = new TupleType(coordinateTypes.toIndexedSeq)
}

case class StructType(val name: String, val featureTypes: Map[String, Type])
  extends Type {
  def apply(features: (String, Values.Data)*) =
    Values.StructData(this, Map(features : _*))

  def $(name: String) = FeatureSelector(this, name)
}

object StructType {
  def apply(name: String, featureTypes: (String, Type)*) =
    new StructType(name, Map(featureTypes: _*))
}
  
case class SeqType(val elementType: Type) extends Type {
  def apply(elements: Values.Data*) = Values.SeqData(this, elements)
  
  def $(index: Int) = ElementSelector(this, index)
}