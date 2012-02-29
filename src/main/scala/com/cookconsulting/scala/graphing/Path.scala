package com.cookconsulting.scala.graphing

/**
 * @author todd
 * @since 2/26/12 2:53 PM
 */


class Path(val point: Node) extends Ordered[Path] {
  var f = 0D
  var g = 0D
  var parent: Path = null;

  def setF(score: Double) {
    f = score
  }

  def setG(score: Double) {
    g = score
  }

  def setParent(p: Path) {
    parent = p
  }

  def compare(that: Path) = (f - that.f).intValue()

  override def toString(): String = {
    if (parent != null) {
      return point.vertex.toString() + " -> " + parent.toString()
    }
    else {
      return point.vertex.toString()
    }
  }
}
