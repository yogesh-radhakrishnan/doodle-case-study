package doodle

object Main extends App {
  import doodle.core._
  import doodle.jvm.Java2DCanvas
  import doodle.example._

  val canvas = Java2DCanvas.canvas

  Spiral.draw(canvas)
}
