package com.github.comco.scrappy.pickers.ordering.strategies

import com.github.comco.scrappy.pickers.ordering.OrderingStrategy

abstract class BaseOrderingStrategies {
  val Ascending: OrderingStrategy
  
  lazy final val Descending: OrderingStrategy = DescendingOrderingStrategy(Ascending)
}