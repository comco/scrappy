package scrappy

sealed trait Selector {
  def sourceType: Type
  def targetType: Type
  
  def select[D <: Domain](domain: D)(data: domain.Data): domain.Data 
    = domain.select(this, data)
  
  def >>(that: Selector) = AppSelector(this, that)
}

case class IdSelector(val sourceType: Type) extends Selector {
  def targetType = sourceType
}

case class AppSelector(val parent: Selector, val child: Selector) extends Selector {
  def sourceType = parent.sourceType
  def targetType = child.targetType
}

case class MapSelector(val sourceType: SeqType, val f: Selector) extends Selector {
  def targetType = SeqType(f.targetType)
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData
    = domain.select(this, data).asInstanceOf[domain.SeqData]
}

case class IntLitSelector(val sourceType: Type, val data: Int) extends Selector {
  def targetType = IntType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.IntData
    = domain.select(this, data).asInstanceOf[domain.IntData]
}

case class StringLitSelector(val sourceType: Type, val data: String) extends Selector {
  def targetType = StringType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.StringData
    = domain.select(this, data).asInstanceOf[domain.StringData]
}

case class BooleanLitSelector(val sourceType: Type, val data: Boolean) extends Selector {
  def targetType = BooleanType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.BooleanData
    = domain.select(this, data).asInstanceOf[domain.BooleanData]
}

case class CoordinateSelector(val sourceType: TupleType, val position: Int) extends Selector {
  def targetType = sourceType.coordinateTypes(position)
}

case class FeatureSelector(val sourceType: StructType, val name: String) extends Selector {
  def targetType = sourceType.featureTypes(name)
}

case class ElementSelector(val sourceType: SeqType, val index: Int) extends Selector {
  def targetType = sourceType.elementType
}

case class TupleSelector(val targetType: TupleType, val coordinateSelectors: IndexedSeq[Selector]) 
  extends Selector {
  def sourceType = coordinateSelectors.head.sourceType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.TupleData
    = domain.select(this, data).asInstanceOf[domain.TupleData]
}

case class StructSelector(val targetType: StructType, val featureSelectors: Map[String, Selector]) 
  extends Selector {
  def sourceType = featureSelectors.seq.head._2.sourceType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.StructData
    = domain.select(this, data).asInstanceOf[domain.StructData]
}