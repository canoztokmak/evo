import scala.io.StdIn
import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = {
    val target = StdIn.readLine("Input target string ?")

//    if (target.length > 25) throw new IllegalArgumentException("Input too long")

    println()
    val mutationFactor: Double = StdIn.readLine("Mutation factor? (0.0 < x < 1.0) \n(Lower the mutation factor, higher the probability of keeping a successful mutation in place)\n").toDouble
    println()
    val maxPopulation: Int = StdIn.readLine("Max population? (1 < x < 50) \n(Higher the max population size, higher the possibility of having individuals with successful mutations)\n").toInt

    implicit val environment: Environment = Environment(maxPopulation, mutationFactor)

    val fitnessFn = new CharFitness(target)
    val initial = generateRandom(target.length)
    println()
    println(s"initial value: $initial")
    val primaryIndividual = Element(initial)
    var generation = Generation(List(primaryIndividual), fitnessFn)

    while (generation.bestIndividual.value != target) {
      generation = generation.reproduce
    }

    println(s"Generation #${generation.number} -- initial value: `$initial` -- final value: `${generation.bestIndividual.value}`")
  }

  private def generateRandom(length: Int): String = Random.alphanumeric.take(length).mkString("")

}
