package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Implicits._
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedData

// f : a -> [b] and zipWithIndex(f) : a -> [(b, Int)]
case class ZipWithIndexPicker(f: Picker with Picker.ReturningSeq) extends BasePicker {
  val sourceType = f.sourceType
  val targetType = seq(tuple(f.targetType, int))

  def doPickData(source: Data): SeqData = {
    val fres = f.pickData(source)
    return seq(fres.elements.zipWithIndex.map {
      case (e, i) => tuple(e, PrimitiveData(i))
    }: _*)
  }

  def doPickOriginatedData(source: OriginatedData): OriginatedSeqData = {
    val fres = f.pickOriginatedData(source)
    fres
  }
}