object App {

    def show(matrix:Array[Array[Int]]) = println(matrix.deep.mkString("\n"))

    def spiral(n:Int):Array[Array[Int]] = {
        if (n <= 1) return Array(Array(0))

        var index:Int = n * n
        def next:Int = { index -= 1; index }
        def rotate(n: Int): Array[Array[Int]] = n match {
            case 0 => Array(Array(next, next).reverse)
            case _ =>
                val matrix = rotate(n-1)
                (Array.fill(matrix(0).length)(next) +: matrix).transpose.reverse
        }
        rotate(n * 2 - 3).transpose
    }

    def main(args:Array[String]) {
        println(show(spiral(5)))
    }
}