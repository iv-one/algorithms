object Main {

  val size = 11
  var spiral = Array.tabulate(size, size){ (x, y) => y + 1}

  def show(matrix:Array[Array[Int]]) = println(matrix.deep.mkString("\n"))

  def fill(pos: Pos, length:Int, vector: (Int, Int), factor: Int, value: Int): Unit = {
    if (length == 0) return;

    var x, y = 0
    0 to length - 1 foreach { n =>
      x = pos.x + n * vector._1 * factor
      y = pos.y + n * vector._2 * factor
      spiral(x)(y) = value + n
    } 

    var nextLength = if (vector._2 == 1) length - 1 else length
    var nextFactor = if (vector._1 == 1) factor * -1 else factor

    fill(Pos(x + vector._2 * nextFactor, y + vector._1 * nextFactor), nextLength, (vector._2, vector._1), nextFactor, value + length)
  }

  def main(args: Array[String]): Unit = {
    fill(Pos(1, size - 1), size - 1, (1, 0), 1, size + 1)
    show(spiral)
  }

}

case class Pos(x: Int, y: Int)