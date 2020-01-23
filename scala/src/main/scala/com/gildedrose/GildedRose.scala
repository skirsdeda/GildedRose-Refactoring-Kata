package com.gildedrose

import scala.math._
import scala.util.matching.Regex

class GildedRose(val items: Array[Item]) {
  def updateQuality(): Unit = GildedRose.updateQuality(items)
}

object GildedRose {
  val MinQuality = 0
  val MaxQuality = 50

  type QualityChangeFunc = (Item, Option[Int]) => Option[Int]

  def atSellInRange(atSellIn: Range)(qualityChangeF: QualityChangeFunc): QualityChangeFunc =
    (item, suggestedChange) =>
      if (atSellIn.contains(item.sellIn)) qualityChangeF(item, suggestedChange) else suggestedChange

  implicit val toFixedQualityChange: Int => QualityChangeFunc = c => (_, _) => Some(c)

  val BackstagePassesR = raw"Backstage passes.*".r
  val NotSulfurasR = raw"(?!Sulfuras)".r

  val qualityClassifier: List[(Regex, QualityChangeFunc)] = List(
    raw"Aged Brie".r -> 1,
    BackstagePassesR -> 1,
    BackstagePassesR -> atSellInRange(5 to 10)(2),
    BackstagePassesR -> atSellInRange(0 to 5)(3),
    BackstagePassesR -> atSellInRange(-1 to 0)(-MaxQuality),
    raw"Conjured.*".r -> -2,
    NotSulfurasR -> ((_, c) => if (c.isEmpty) Some(-1) else c),
    NotSulfurasR -> atSellInRange(Int.MinValue to 0)((_, prevC) => prevC.map(c => if (c > 0) c else c * 2))
  )

  def updateQuality(items: Array[Item]): Unit =
    for (item <- items)
      qualityClassifier
        .collect { case (regex, changeF) if regex.findPrefixMatchOf(item.name).nonEmpty => changeF }
        .foldLeft(None: Option[Int])((prev, func) => func(item, prev))
        .foreach { change =>
          item.sellIn -= 1
          item.quality = max(MinQuality, min(MaxQuality, item.quality + change))
        }
}