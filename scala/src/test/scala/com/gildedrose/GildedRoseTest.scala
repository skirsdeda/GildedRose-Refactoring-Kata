package com.gildedrose

import org.scalatest._

class GildedRoseTest  extends FlatSpec with Matchers {
  it should "Normal Item" in {
    val item = itemUpdate(new Item("+5 Dexterity Vest", 10, 20))
    item.name should equal ("+5 Dexterity Vest")
    item.sellIn should equal (9)
    item.quality should equal (19)
  }

  it should "Normal Item to 0" in {
    val item = itemUpdate(new Item("+5 Dexterity Vest", 10, 12), 13)
    item.name should equal ("+5 Dexterity Vest")
    item.sellIn should equal (-3)
    item.quality should equal (0)
  }

  it should "Normal Item degrades twice as fast after sellIn date" in {
    val item = itemUpdate(new Item("+5 Dexterity Vest", 10, 20), 13)
    item.name should equal ("+5 Dexterity Vest")
    item.sellIn should equal (-3)
    item.quality should equal (4)
  }

  it should "Brie" in {
    val item = itemUpdate(new Item("Aged Brie", 2, 0))
    item.sellIn should equal (1)
    item.quality should equal (1)
  }

  it should "Brie through sellIn" in {
    val item = itemUpdate(new Item("Aged Brie", 2, 0), 3)
    item.sellIn should equal (-1)
    item.quality should equal (3)
  }

  it should "Sulfuras" in {
    val item = itemUpdate(new Item("Sulfuras, Hand of Ragnaros", 0, 80))
    item.sellIn should equal (0)
    item.quality should equal (80)
  }

  it should "Backstage passes" in {
    val item = itemUpdate(new Item("Backstage passes to a TAFKAL80ETC concert", 11, 10))
    item.sellIn should equal (10)
    item.quality should equal (11)
  }

  it should "Backstage passes 10/49" in {
    val item = itemUpdate(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49))
    item.sellIn should equal (9)
    item.quality should equal (50)
  }

  it should "Backstage passes 10/48" in {
    val item = itemUpdate(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 48))
    item.sellIn should equal (9)
    item.quality should equal (50)
  }

  it should "Backstage passes 5/10 * 3" in {
    val item = itemUpdate(new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10), 3)
    item.sellIn should equal (2)
    item.quality should equal (19)
  }

  it should "Backstage passes 1/47 * 2" in {
    val item = itemUpdate(new Item("Backstage passes to a TAFKAL80ETC concert", 1, 47), 2)
    item.sellIn should equal (-1)
    item.quality should equal (0)
  }

  it should "Conjured" in {
    val item = itemUpdate(new Item("Conjured", 2, 20), 2)
    item.sellIn should equal (0)
    item.quality should equal (16)
  }

  it should "Conjured degrades twice as fast after sellIn" in {
    val item = itemUpdate(new Item("Conjured", 2, 20), 3)
    item.sellIn should equal (-1)
    item.quality should equal (12)
  }

  private def itemUpdate(item: Item, iterations: Int = 1): Item = {
    val items = Array[Item](item)
    val app = new GildedRose(items)
    for (_ <- 0 until iterations) app.updateQuality()
    app.items(0)
  }
}