import org.scalatest.FunSuite
import distribution.helper.{timer, timerWithTime}
import org.apache.commons.math3.random.RandomDataGenerator
import distribution.continuous._
import distribution.{RandomSeq, RandomPar}


class TestDistribution2 extends TestUtil {
  val rng = new RandomSeq(new scala.util.Random(0))
  //test("Normal") {
  //  val truth:UnivariateTruth = ???
  //  (new UnivariateTester(Normal(3,2), rng=rng, truth=truth)).test()
  //}
}


