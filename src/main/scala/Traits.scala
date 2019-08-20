trait Reproducible[E] {
  def reproduce: E
}

trait Mutable[E] {
  def mutate: E
}

trait Fitness {
  def calculate(value: String): Int
}

class CharFitness(target: String) extends Fitness {
  override def calculate(value: String): Int = {
    value.zip(target).map {
      case (a, b) =>
        charDistance(a, b)
    }.sum
  }

  private def charDistance(a: Char, b: Char): Int = Math.abs(a - b)
}
