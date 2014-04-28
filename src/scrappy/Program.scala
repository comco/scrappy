package scrappy

object Program {

  def main(args: Array[String]) {
    val Pt = TupleType(IntType, IntType)
    val GoodsItem = StructType("GoodsItem",
      "id" -> IntType,
      "at" -> Pt)

    val GoodsItems = SeqType(GoodsItem)

    val Declaration = StructType("declaration",
      "id" -> IntType,
      "goodsItems" -> GoodsItems)

    val at = Pt(IntType(3), IntType(4))
    val gi1 = GoodsItem("id" -> IntType(4),
      "at" -> at)
    val gi2 = GoodsItem("id" -> IntType(6),
      "at" -> at)
    val gis = GoodsItems(gi1, gi2)
    val declaration = Declaration("id" -> IntType(1),
        "goodsItems" -> gis)
        
    val q = (Declaration $ "goodsItems") >> MapSelector(GoodsItems, (GoodsItem $ "id"))
    
    val v = q.select(Tagger)(Tagger.tag(declaration, "declaration"))
    
    val q2 = SeqType(IntType) $ 0
    
    println(q2.select(Tagger)(v))
  }
}