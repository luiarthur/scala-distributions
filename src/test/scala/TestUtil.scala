import org.scalatest.FunSuite
import distribution.helper.timer

trait TestUtil extends FunSuite {

  def assertApprox(x:Double, y:Double, eps:Double=1E-4, debug:Boolean=false) = {
    val valid = math.abs(x - y) < eps
    if (debug) {
      if (!valid) {
        println(s"invalid -- x: $x; y:$y")
      } else println(s"valid -- x: $x; y:$y")
    }
    assert(valid)
  }

  def testWithMsg[R](msg:String)(block: => R) = {
    test(msg) {
      print(msg + " -- ")
      timer {
        block
      }
    }
  }

  def round(x:Double, d:Int) = {
    require(d >= 0)
    val dMax = x.toString.split('.').last.size
    val factor = math.pow(10, if(d>dMax) dMax else d)
    (x * factor).toInt / factor
  }


  def mean(x:Vector[Double]) = x.sum / x.size

  def sd(x: Vector[Double]) = {
    val m = mean(x)
    val ss = x.map(xi => math.pow(xi - m, 2)).sum
    math.sqrt(ss / x.size)
  }

  def variance(x:Vector[Double]) = math.pow(sd(x), 2)

  def assertApproxArray(a:Array[Double], b:Array[Double], eps:Double=1E-3, debug:Boolean=false): Unit = {
    a.zip(b).foreach{ case(ai, bi) =>
      assertApprox(ai, bi, eps)
    }
  }

  def assertApproxMat(a:Array[Array[Double]], b:Array[Array[Double]], eps:Double=1E-3, debug:Boolean=false): Unit = {
    import distribution.SpecialFunctions.printMat

    val matA = a.flatten
    val matB = b.flatten
    matA.zip(matB).foreach{ case (ma,mb) => 
      assertApprox(ma, mb, eps)
    }
  }
  

}
