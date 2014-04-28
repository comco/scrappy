package scrappy

trait Domain {
  trait DomainData {
    def datatype: Type
  }
  
  type Data <: DomainData
 
  type IntData <: Data {
    def data: Int
  }
  
  type StringData <: Data {
    def data: String
  }
  
  type BooleanData <: Data {
    def data: Boolean
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
  
  def tie(q: Selector, d: Data): Tie = {
    if (d.datatype == q.sourceType) {
      q match {
        case q: IdSelector => IdTie(q, d)
        case q: AppSelector => AppTie(q, d)
        case q: MapSelector => MapTie(q, d.asInstanceOf[SeqData])
        case q: IntLitSelector => IntLitTie(q, d)
        case q: StringLitSelector => StringLitTie(q, d)
        case q: BooleanLitSelector => BooleanLitTie(q, d)
        case q: CoordinateSelector => CoordinateTie(q, d.asInstanceOf[TupleData])
        case q: FeatureSelector => FeatureTie(q, d.asInstanceOf[StructData])
        case q: ElementSelector => ElementTie(q, d.asInstanceOf[SeqData])
        case q: TupleSelector => TupleTie(q, d)
        case q: StructSelector => StructTie(q, d)
      }
    } else {
      throw new IllegalAccessException(s"Data $d is not usable for selection $q");
    }
  }

  sealed trait Tie
  case class IdTie(q: IdSelector, d: Data) extends Tie
  case class AppTie(q: AppSelector, d: Data) extends Tie
  case class MapTie(q: MapSelector, d: SeqData) extends Tie
  case class IntLitTie(q: IntLitSelector, d: Data) extends Tie
  case class StringLitTie(q: StringLitSelector, d: Data) extends Tie
  case class BooleanLitTie(q: BooleanLitSelector, d: Data) extends Tie
  case class CoordinateTie(q: CoordinateSelector, d: TupleData) extends Tie
  case class FeatureTie(q: FeatureSelector, d: StructData) extends Tie
  case class ElementTie(q: ElementSelector, d: SeqData) extends Tie
  case class TupleTie(q: TupleSelector, d: Data) extends Tie
  case class StructTie(q: StructSelector, d: Data) extends Tie
  
  def select(q: Selector, d: Data): Data
}