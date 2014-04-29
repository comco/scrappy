package scrappy

object Values extends Domain {
  sealed trait Data extends DomainData
  
  case class PrimitiveData[T](val data: T) extends Data {
    def datatype = PrimitiveType[T]
  }
  
  case class TupleData(val datatype: TupleType, val coordinates: IndexedSeq[Data]) 
    extends Data
  
  case class StructData(val datatype: StructType, val features: Map[String, Data])
    extends Data
  
  case class SeqData(val datatype: SeqType, val elements: Seq[Data])
    extends Data
  
  def select(pick: Picker, data: Data): Data = tie(pick, data) match {
    // basic
  	case IdTie(data) => data
    case AppTie(pick, data) => select(pick.second, select(pick.first, data))
    // primitive
    case ValTie(pick, _) => PrimitiveData(pick.v)
    case FuncTie(pick, data) => PrimitiveData(pick.f(data.data))
    case FoldTie(pick, _, elems) => PrimitiveData(pick.f(elems.map(_.data)))
    case UnfoldTie(pick, data) => SeqData(pick.targetType, 
        pick.f(data.data).map(PrimitiveData(_)))
    // sequence
    case MapTie(pick, data) => SeqData(pick.targetType, 
        data.elements.map(select(pick.f, _)))
    case FilterTie(pick, data) => ???
    case SortTie(pick, data) => ???
    case GroupTie(pick, data) => ???
    case JoinTie(pick, data) => ???
    case ZipTie(pick, data) => ???
    // projections
    case CoordinateTie(pick, data) => data.coordinates(pick.position)
    case FeatureTie(pick, data) => data.features(pick.name)
    case ElementTie(pick, data) => data.elements(pick.index)
    // constructors
    case TupleTie(pick, data) => ???
    case StructTie(pick, data) => ???
    case RepeatTie(pick, data) => SeqData(pick.targetType, 
        Stream.continually(data))
  }
} 