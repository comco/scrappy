package com.github.comco.scrappy.checker

import com.github.comco.scrappy.origin.Origin

abstract class CheckReason

case class OriginatedCheckResult(val successful: Boolean,
    val scope: Origin, val witnesses: Set[CheckReason], val checker: Checker) extends CheckReason
    
case class WitnessReason(val witness: Origin) extends CheckReason