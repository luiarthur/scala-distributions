package distribution
import scala.annotation.tailrec

object SpecialFunctions {
  def eye(n:Int):Array[Array[Double]] = {
    require(n >= 0)
    Array.tabulate(n)( row => Array.tabulate(n)( col => if (col == row) 1 else 0) )
  }

  def logFactorial(n:Int): Double = {
    @tailrec
    def engine(_n:Int, _out:Double=0):Double = _n match {
      case 0 => _out
      case _ => engine(_n - 1, math.log(_n) + _out)
    }

    engine(n)
  }

  def vvMult(v1:Array[Double], v2:Array[Double]): Double = {
    require(v1.size == v2.size)
    val n = v1.size
    def engine(out:Double=0.0, i:Int=0): Double = {
      if (i == n) out else {
        engine(out + v1(i) * v2(i), i + 1)
      }
    }
    engine()
  }

  def vvAdd(v1:Array[Double], v2:Array[Double]): Array[Double] = {
    require(v1.size == v2.size)
    val n = v1.size
    Array.tabulate(n){ i => v1(i) + v2(i) }
  }

  def mvMult(mat:Array[Array[Double]], vec:Array[Double]): Array[Double] = {
    mat.map{ row => vvMult(row, vec) }
  }

  def mmMult(m1:Array[Array[Double]], m2:Array[Array[Double]]):Array[Array[Double]] = {
    ???
  }

  def isSquare(A:Array[Array[Double]]): Boolean = {
    val nrow = A.size
    def engine(row:Array[Double], remaining:Array[Array[Double]]): Boolean = {
      if (remaining.size == 0) true else if (row.size != nrow) false else {
        engine(remaining.head, remaining.tail)
      }
    }
    engine(A.head, A.tail)
  }

  /* @return L, where LL' = A
   * @todo: Need to make this more efficient.
   * @see: https://rosettacode.org/wiki/Cholesky_decomposition   */
  def choleskyL(A: Array[Array[Double]],
                checkDim:Boolean=false,
                useJava:Boolean=false): Array[Array[Double]] = {

    if (checkDim) { require(isSquare(A)) }

    if (useJava) Linalg.choleskyL(A) else {
      val n = A.size
      val L = Array.ofDim[Double](n,n)
      
      for (i <- 0 until n; j <- 0 to i) {
        val x = {
          def engine(k:Int=0, out:Double=0):Double = k match {
            case d if d == j  => out
            case _ => engine(k + 1, out + L(i)(k) * L(j)(k))
          }
          engine()
        }

        L(i)(j) = if (i == j) math.sqrt(A(i)(i) - x) else (A(i)(j) - x) / L(j)(j)
      }

      return(L)
    }
  }

  def printMat(x:Array[Array[Double]]): Unit = {
    println()
    x.foreach(row => {
      row.foreach(col => {
        print("%4.4f ".format(col))
      })
      println()
    })
  }

  def logChoose(n:Int, k:Int): Double = {
    logFactorial(n) - logFactorial(n - k) - logFactorial(k)
  }

  def choose(n:Int, k:Int): Int = {
    math.round(math.exp(logChoose(n, k)).toFloat)
  }

  def sech(x:Double): Double = {
    1 / math.cosh(x)
  }

  def sigmoid(x:Double, a:Double=0, b:Double=1): Double = {
    require(a <= b)
    (a, b) match {
      case (0, 1) => 1 / (1 + math.exp(-x))
      case _ => {
        val ex = math.exp(x)
        (ex * b + a) / (1 + ex)
      }
    }
  }

  def logit(p: Double, a:Double=0, b:Double=1): Double = {
    require(a <= b)
    (a, b) match {
      case (0, 1) => math.log(p) - math.log(1 - p)
      case _ => math.log(p - a) - math.log(b - b)
    }
  }
}
