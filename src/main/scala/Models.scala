import scala.util.Random

case class Environment(maxPopulation: Int, mutationFactor: Double)

case class Element(value: String)(implicit environment: Environment) extends Mutable[Element] {
  override def mutate: Element = {
    val nextValue = value.toCharArray.map { c =>
      val factor = Random.nextDouble()
      if (factor > environment.mutationFactor && factor < (1-environment.mutationFactor)) c
      else if (factor < environment.mutationFactor) c-1
      else c+1
    }.map(_.toChar).mkString

    Element(nextValue)
  }
}

case class Generation(population: List[Element], fitnessFn: Fitness, number: Int = 0)
                     (implicit environment: Environment) extends Reproducible[Generation] {
  override def reproduce: Generation = {
    val offspring = population.map(_.mutate)

    val fitnesses = (population ++ offspring).map { element =>
      element -> fitnessFn.calculate(element.value)
    }

    if (number % 10000 == 0) println(s"Generation #$number")
    val wholePopulation = fitnesses
      .sortBy(_._2)
      .map(x => {
        if (number % 10000 == 0)
          println(x)
        x
      })
    if (number % 10000 == 0) println(s"Average fitness ${wholePopulation.map(_._2).sum / wholePopulation.size}\n----")

    val nextGeneration = wholePopulation
      .distinct
      .take(environment.maxPopulation)
      .map(_._1)

    Generation(nextGeneration, fitnessFn, number+1)
  }

  def bestIndividual: Element = population.head

  override def toString: String = {
    s"Generation #{$number}\n(${population.mkString("\n")})"
  }
}
