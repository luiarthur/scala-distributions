package distribution.discrete

// TODO: Test
case class Geometric(probSuccess: Double) extends NegativeBinomialBase(1, probSuccess)
