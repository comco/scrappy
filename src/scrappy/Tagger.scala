package scrappy

object Tagger extends Domain {
  sealed trait Data extends DomainData {
    def source: Values.Data
    def path: String
    def datatype = source.datatype
  }
  
  case class IntData(val source: Values.IntData, val path: String) extends Data {
    def data = source.data
  }
  
  case class StringData(val source: Values.StringData, val path: String) extends Data {
    def data = source.data
  }
  
  case class BooleanData(val source: Values.BooleanData, val path: String) extends Data {
    def data = source.data
  }
  
  case class TupleData(val source: Values.TupleData, val path: String) extends Data {
    lazy val coordinates = source.coordinates.zipWithIndex.map {
      case (d, i) => tag(d, s"$path@$i")
    }
  }
  
  case class StructData(val source: Values.StructData, val path: String) extends Data {
    lazy val features = source.features.map {
      case (name, data) => (name, tag(data, s"$path.$name"))
    }
  }
  
  case class SeqData(val source: Values.SeqData, val path: String) extends Data {
    lazy val elements = source.elements.zipWithIndex.map {
      case (d, i) => tag(d, s"$path[$i]")
    }
  }
  
  def tag(d: Values.Data, path: String): Data = d match {
    case d: Values.IntData => IntData(d, path)
    case d: Values.BooleanData => BooleanData(d, path)
    case d: Values.StringData => StringData(d, path)
    case d: Values.TupleData => TupleData(d, path)
    case d: Values.StructData => StructData(d, path)
    case d: Values.SeqData => SeqData(d, path)
  }
  
  def select(q: Selector, d: Data): Data = tie(q, d) match {
    case IdTie(_, d) => d
    case AppTie(q, d) => select(q.child, select(q.parent, d))
    //case MapTie(q, d) => SeqData(q.select(Values)(d.source), q.f + "/@" + d.path)
    case MapTie(q, d) => SeqData(q.select(Values)(d.source), d.path)
    case IntLitTie(q, d) => IntData(Values.IntData(q.data), "*")
    case StringLitTie(q, d) => StringData(Values.StringData(q.data), "*")
    case BooleanLitTie(q, d) => BooleanData(Values.BooleanData(q.data), "*")
    case CoordinateTie(q, d) => d.coordinates(q.position)
    case FeatureTie(q, d) => d.features(q.name)
    case ElementTie(q, d) => d.elements(q.index)
    case TupleTie(q, d) => TupleData(q.select(Values)(d.source), s"*[${d.datatype}]")
    case StructTie(q, d) => StructData(q.select(Values)(d.source), s"*[${d.datatype}]")
  }
}