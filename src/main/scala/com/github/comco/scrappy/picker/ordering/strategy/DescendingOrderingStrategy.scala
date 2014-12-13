package com.github.comco.scrappy.picker.ordering.strategy

import com.github.comco.scrappy.picker.ordering.OrderingStrategy

case class DescendingOrderingStrategy(val ascendingStrategy: OrderingStrategy) extends OrderingStrategy {
  def datatype = ascendingStrategy.datatype
  
  lazy val dataOrdering = ascendingStrategy.dataOrdering.reverse
  lazy val originatedDataOrdering = ascendingStrategy.originatedDataOrdering.reverse
}