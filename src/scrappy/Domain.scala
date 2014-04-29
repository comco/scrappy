package scrappy

import scala.language.higherKinds

trait Domain {
  trait DomainData {
    def datatype: Type
  }

  type Data <: DomainData

  type PrimitiveData[T] <: Data {
    def data: T
  }

  type TupleData <: Data {
    def coordinates: IndexedSeq[Data]
  }

  type StructData <: Data {
    def features: Map[String, Data]
  }

  type SeqData <: Data {
    def elements: Seq[Data]
  }

  sealed trait Tie

  case class IdTie(data: Data) extends Tie
  case class AppTie(pick: AppPicker, data: Data) extends Tie
  // Primitive ties
  case class ValTie[V](pick: ValPicker[V], data: Data) extends Tie
  case class FuncTie[V](pick: FuncPicker[V], data: PrimitiveData[V]) extends Tie
  case class FoldTie[V](pick: FoldPicker[V], data: SeqData, elements: Seq[PrimitiveData[V]]) extends Tie
  case class UnfoldTie[V](pick: UnfoldPicker[V], data: PrimitiveData[V]) extends Tie
  // Sequence ties
  case class MapTie(pick: MapPicker, data: SeqData) extends Tie
  case class FilterTie(pick: FilterPicker, data: SeqData) extends Tie
  case class SortTie(pick: SortPicker, data: SeqData) extends Tie
  case class GroupTie(pick: GroupPicker, data: SeqData) extends Tie
  case class JoinTie(pick: JoinPicker, data: TupleData) extends Tie
  case class ZipTie(pick: ZipPicker, data: Data) extends Tie
  // Projections
  case class CoordinateTie(pick: CoordinatePicker, data: TupleData) extends Tie
  case class FeatureTie(pick: FeaturePicker, data: StructData) extends Tie
  case class ElementTie(pick: ElementPicker, data: SeqData) extends Tie
  // Constructors
  case class TupleTie(pick: TuplePicker, data: Data) extends Tie
  case class StructTie(pick: StructPicker, data: Data) extends Tie
  case class RepeatTie(pick: RepeatPicker, data: Data) extends Tie

  def tie(pick: Picker, data: Data): Tie = {
    import scala.language.reflectiveCalls
    require(pick.sourceType == data.datatype)
    pick match {
      // Basic
      case IdPicker => IdTie(data)
      case pick: AppPicker => AppTie(pick, data)
      // Primitive
      case pick: ValPicker[v] => ValTie(pick, data)
      case pick: FuncPicker[v] => FuncTie(pick, data.asInstanceOf[PrimitiveData[v]])
      case pick: FoldPicker[v] => FoldTie(pick,
        data.asInstanceOf[SeqData], 
        data.asInstanceOf[SeqData].elements.map(_.asInstanceOf[PrimitiveData[v]]))
      case pick: UnfoldPicker[v] => UnfoldTie(pick, data.asInstanceOf[PrimitiveData[v]])
      // Sequence
      case pick: MapPicker => MapTie(pick, data.asInstanceOf[SeqData])
      case pick: FilterPicker => FilterTie(pick, data.asInstanceOf[SeqData])
      case pick: SortPicker => SortTie(pick, data.asInstanceOf[SeqData])
      case pick: GroupPicker => GroupTie(pick, data.asInstanceOf[SeqData])
      case pick: JoinPicker => JoinTie(pick, data.asInstanceOf[TupleData])
      case pick: ZipPicker => ZipTie(pick, data)
      // Projections
      case pick: CoordinatePicker => CoordinateTie(pick, data.asInstanceOf[TupleData])
      case pick: FeaturePicker => FeatureTie(pick, data.asInstanceOf[StructData])
      case pick: ElementPicker => ElementTie(pick, data.asInstanceOf[SeqData])
      // Constructors
      case pick: TuplePicker => TupleTie(pick, data)
      case pick: StructPicker => StructTie(pick, data)
      case pick: RepeatPicker => RepeatTie(pick, data)
    }
  }
  
  def select(pick: Picker, data: Data): Data
}