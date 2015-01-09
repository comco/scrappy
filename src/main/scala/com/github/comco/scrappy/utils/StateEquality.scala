package com.github.comco.scrappy.utils

trait StateEquality[Type <: StateEquality[Type]] {
  protected def state: Any

  final override def equals(that: Any) = that match {
    case that: StateEquality[Type] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}