package distributions

object SpecialFunctions {
  def eye(n:Int):Array[Array[Double]] = {
    require(n >= 0)
    Array.tabulate(n)( row => Array.tabulate(n)( col => if (col == row) 1 else 0) )
  }

  def logFactorial(n:Int, _out:Double=0): Double = n match {
    case 0 => _out
    case _ => logFactorial(n - 1, math.log(n) + _out)
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
   */
  def choleskyL(A: Array[Array[Double]], checkDim:Boolean=false): Array[Array[Double]] = {
    val n = A.size
    if (checkDim) {
      require(isSquare(A)) 
    }

    val L = Array.ofDim[Double](n,n)
    
    for (i <- 0 until n; j <- 0 to i) {
      (i,j) match {
        case (a,b) if a==b => {
          val x = L(i).take(i).map(lij => lij*lij).sum
          L(i)(i) = math.sqrt(A(i)(i) - x)
        }
        case _ => {
          var x = 0.0
          for (k <- 0 until j) {
            x += L(i)(k) * L(j)(k)
          }
          L(i)(j) =  (A(i)(j) - x) / L(j)(j)
        }
      }
    }

    return(L)
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
}
