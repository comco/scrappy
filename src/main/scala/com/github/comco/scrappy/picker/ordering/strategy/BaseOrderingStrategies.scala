package com.github.comco.scrappy.picker.ordering.strategy

import com.github.comco.scrappy.picker.ordering.OrderingStrategy

abstract class BaseOrderingStrategies {
  val Ascending: OrderingStrategy
  
  lazy final val Descending: OrderingStrategy = DescendingOrderingStrategy(Ascending)
}