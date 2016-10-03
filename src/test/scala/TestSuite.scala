import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  import distributions.Random._
  import distributions.helper.timer
  
  val n = 1E5.toInt
  println

  test("Random Uniform") {
    import scala.collection.parallel.immutable.ParVector
    val (xmin, xmax) = (2,20)
    print("Uniform: ")
    val x = timer { List.fill(n)(runif(2,20)) }
    assert( x.max < xmax && x.min > xmin)
  }
  test("Random Normal") {
    print("Normal: ")
    val x = timer { List.fill(n)(rnorm(5,2)) }
  }
  test("Random Exponential") {
    print("Exponential: ")
    val x = timer { List.fill(n)(rexp(3)) }
  }
  test("Random Gamma") {
  }
  test("Random Inverse Gamma") {
  }
  test("Random Beta") {
  }
  test("Random F") {
  }
  test("Random t") {
  }
  test("Random Weibull") {
  }
  test("Random Chi-sq") {
  }

  println 
}
