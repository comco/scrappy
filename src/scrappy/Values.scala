package scrappy

object Values extends Domain {
  sealed trait Data extends DomainData
  
  case class IntData(val data: Int) extends Data {
    def datatype = IntType
  }
  
  case class StringData(val data: String) extends Data {
    def datatype = StringType
  }
  
  case class BooleanData(val data: Boolean) extends Data {
    def datatype = BooleanType
  }
  
  case class TupleData(val datatype: TupleType, val coordinates: IndexedSeq[Data]) 
    extends Data
  
  case class StructData(val datatype: StructType, val features: Map[String, Data])
    extends Data
  
  case class SeqData(val datatype: SeqType, val elements: Seq[Data])
    extends Data
  
  def select(q: Selector, d: Data): Data = tie(q, d) match {
    case IdTie(_, d) => d
    case AppTie(q, d) => select(q.child, select(q.parent, d))
    case MapTie(q, d) => SeqData(q.targetType, d.elements.map(select(q.f, _)))
    case IntLitTie(q, d) => IntData(q.data)
    case StringLitTie(q, d) => StringData(q.data)
    case BooleanLitTie(q, d) => BooleanData(q.data)
    case CoordinateTie(q, d) => d.coordinates(q.position)
    case FeatureTie(q, d) => d.features(q.name)
    case ElementTie(q, d) => d.elements(q.index)
    case TupleTie(q, d) => TupleData(q.targetType, q.coordinateSelectors.map(select(_, d)))
    case StructTie(q, d) => StructData(q.targetType, q.featureSelectors.map {
      case (name, selector) => (name, select(selector, d))
    }) 
  }
}