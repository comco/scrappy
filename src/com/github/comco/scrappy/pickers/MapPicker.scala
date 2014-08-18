package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class MapPicker(val f: Picker) extends BaseSeqPicker {
  def sourceType = SeqType(f.sourceType)
  def targetType = SeqType(f.targetType)

  def doPick(source: DataDomain.SeqData) =
    DataDomain.SeqData(targetType, source.elements.map(f.pick(_)))

  def doPickWithOrigin(source: DataWithOriginDomain.SeqData) =
    DataWithOriginDomain.ComputedSeqData(
      doPick(source.data),
      source.origin.computed,
      source.elements.map(f.pickWithOrigin(_)))
}