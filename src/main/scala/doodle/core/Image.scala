package doodle.core

import doodle.backend.Canvas
import doodle.jvm.Java2DCanvas


sealed trait Image {
  def on(that: Image): Image = On(this, that)

  def beside(that: Image): Image = Beside(this, that)

  def above(that: Image): Image = Above(this, that)

  def draw(implicit canvas: Canvas, originX: Double, originY: Double): Unit = {
    canvas.setSize(100, 100)

    this match {
      case Circle(r) => canvas.circle(0.0, 0.0, r)
      case Rectangle(w, h) => canvas.rectangle(-w / 2, h / 2, w / 2, -h / 2)
      case Above(a, b) =>
        val box = this.boundingBox
        val aBox = a.boundingBox
        val bBox = b.boundingBox
        val aboveOriginY = originY + box.top - (aBox.height / 2)
        val belowOriginY = originY + box.bottom + (bBox.height / 2)
        a.draw(canvas, originX, aboveOriginY)
        b.draw(canvas, originX, belowOriginY)
      case On(o, u) =>
        o.draw(canvas, originX, originY)
        u.draw(canvas, originX, originY)
      case Beside(l, r) =>
        val box = this.boundingBox
        val lBox = l.boundingBox
        val rBox = r.boundingBox
        val leftOriginX = originX + box.left + (lBox.width / 2)
        val rightOriginX = originX + box.right - (rBox.width / 2)
        l.draw(canvas, leftOriginX, originY)
        r.draw(canvas, rightOriginX, originY)
    }
    canvas.setStroke(Stroke(1.0, Color.darkBlue, Line.Cap.Round, Line.Join.Round))
    canvas.stroke()
  }


  val boundingBox: BoundingBox =
    this match {
      case Triangle(l,h) =>
        BoundingBox(-l / 2, h / 2, l / 2, -h / 2)
      case Circle(r) =>
        BoundingBox(-r, r, r, -r)
      case Rectangle(w, h) =>
        BoundingBox(-w / 2, h / 2, w / 2, -h / 2)
      case Above(a, b) =>
        a.boundingBox above b.boundingBox
      case On(o, u) =>
        o.boundingBox on u.boundingBox
      case Beside(l, r) =>
        l.boundingBox beside r.boundingBox
    }
}

case class Circle(val radius: Double) extends Image

case class Rectangle(val width: Double, val height: Double) extends Image

case class Triangle(l: Double, b: Double) extends Image

case class On(a: Image, b: Image) extends Image

case class Beside(a: Image, b: Image) extends Image

case class Above(a: Image, b: Image) extends Image

object testImage {
  def main(args: Array[ String ]): Unit = {
    implicit val canvas = Java2DCanvas.canvas
    Circle(10) above Circle(10) draw(canvas, 10, 10)
  }
}


