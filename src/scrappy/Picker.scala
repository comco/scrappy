package scrappy

import com.sun.xml.internal.bind.unmarshaller.DOMScanner

sealed trait Picker {
  def sourceType: Type
  def targetType: Type
  
  def select[D <: Domain](domain: D)(data: domain.Data): domain.Data =
    domain.select(this, data)
}

// Identity picker
case object IdPicker extends Picker {
  def sourceType = AnyType
  def targetType = AnyType
}

// Picker composition
case class AppPicker(val first: Picker, val second: Picker) extends Picker {
  require(first.targetType == second.sourceType)

  def sourceType = first.sourceType
  def targetType = second.targetType
}

// Primitive lift pickers
case class ValPicker[V](val v: V) extends Picker {
  def sourceType = AnyType
  def targetType = PrimitiveType[V]
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.PrimitiveData[V] =
    super.select(domain)(data).asInstanceOf[domain.PrimitiveData[V]]
}

case class FuncPicker[V](val f: V => V) extends Picker {
  def sourceType = PrimitiveType[V]
  def targetType = PrimitiveType[V]
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.PrimitiveData[V] =
    super.select(domain)(data).asInstanceOf[domain.PrimitiveData[V]]
}

case class FoldPicker[V](val f: Seq[V] => V) extends Picker {
  def sourceType = SeqType(PrimitiveType[V])
  def targetType = PrimitiveType[V]
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.PrimitiveData[V] =
    super.select(domain)(data).asInstanceOf[domain.PrimitiveData[V]]
}

case class UnfoldPicker[V](val f: V => Seq[V]) extends Picker {
  def sourceType = PrimitiveType[V]
  def targetType = SeqType(PrimitiveType[V])
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

// Sequence pickers
case class MapPicker(val f: Picker) extends Picker {
  def sourceType = SeqType(f.sourceType)
  def targetType = SeqType(f.targetType)
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

// TODO: Should'n this be different?
case class FilterPicker(val f: Picker) extends Picker {
  def sourceType = SeqType(f.sourceType)
  def targetType = sourceType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

case class SortPicker(val on: Picker) extends Picker {
  def sourceType = SeqType(on.sourceType)
  def targetType = sourceType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

case class GroupPicker(val by: Picker) extends Picker {
  def sourceType = SeqType(by.sourceType)
  def targetType = SeqType(TupleType(by.targetType, sourceType))
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

case class JoinPicker(val a: Picker, val b: Picker) extends Picker {
  require(a.targetType == b.targetType)
  def sourceType = TupleType(SeqType(a.sourceType), SeqType(b.sourceType))
  def targetType = SeqType(TupleType(a.sourceType, b.sourceType))
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}

case class ZipPicker(val coordinatePickers: IndexedSeq[Picker]) extends Picker {
  require(coordinatePickers.forall(_.sourceType == sourceType))
  
  def sourceType = coordinatePickers.head.sourceType
  def targetType = TupleType(coordinatePickers.map(_.targetType))
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.TupleData =
    super.select(domain)(data).asInstanceOf[domain.TupleData]
}

// Projections
case class CoordinatePicker(val sourceType: TupleType, val position: Int)
  extends Picker {
  require(0 <= position && position < sourceType.coordinateTypes.length)

  def targetType = sourceType.coordinateTypes(position)
}

case class FeaturePicker(val sourceType: StructType, val name: String)
  extends Picker {
  require(sourceType.featureTypes.contains(name))

  def targetType = sourceType.featureTypes(name)
}

case class ElementPicker(val sourceType: SeqType, val index: Int)
  extends Picker {
  def targetType = sourceType.elementType
}

// Constructors
case class TuplePicker(val coordinatePickers: IndexedSeq[Picker])
  extends Picker {
  require(coordinatePickers.forall(_.sourceType == sourceType))
  
  def sourceType = coordinatePickers.head.sourceType
  def targetType = TupleType(coordinatePickers.map(_.targetType))
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.TupleData =
    super.select(domain)(data).asInstanceOf[domain.TupleData]
}

case class StructPicker(val targetType: StructType, 
    val featurePickers: Map[String, Picker])
  extends Picker {
  require(targetType.featureTypes.keySet == featurePickers.keySet)
  require(featurePickers.forall {
    case (name, picker) => picker.targetType == targetType.featureTypes(name)
  })
  
  def sourceType = featurePickers.seq.head._2.sourceType
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.StructData =
    super.select(domain)(data).asInstanceOf[domain.StructData]
}

case class RepeatPicker(val sourceType: Type) extends Picker {
  def targetType = SeqType(sourceType)
  
  override def select[D <: Domain](domain: D)(data: domain.Data): domain.SeqData =
    super.select(domain)(data).asInstanceOf[domain.SeqData]
}