package it.codingjam.lagioconda.population

import com.typesafe.scalalogging.{LazyLogging, Logger}

import scala.util.Random

case class Population(generation: Int, individuals: List[Individual], hillClimbedGene: Int, lastResults: List[Double])
    extends LazyLogging {

  override lazy val logger = Logger(classOf[Population])

  def bestIndividual: Individual = individuals.head

  def randomIndividual: Individual = individuals(Random.nextInt(individuals.size))

  def randomNotElite: Individual = individuals(Population.EliteCount + Random.nextInt(Population.Size - Population.EliteCount))

  def randomElite: Individual = individuals(Random.nextInt(Population.EliteCount))

  def randomPositionAndIndividual: (Int, Individual) = {
    val pos = Random.nextInt(individuals.size)
    (pos, individuals(pos))
  }

  def randomIndividualInRange(position: Int) = {

    def normalizedPosition(i: Int) = {
      if (i < 0) 0
      else if (i > individuals.size - 1) individuals.size - 1
      else i
    }

    val range = 12
    val pos = position - (range / 2) + Random.nextInt(range)
    individuals(normalizedPosition(pos))
  }

  def randomIndividualByWeight: Individual = {
    val total = (individuals.size * (individuals.size + 1)) / 2
    var r = Random.nextInt(total)
    var x = individuals.size
    while (r > 0) {
      r = r - x
      x = x - 1
    }
    val position = individuals.size - x - 1
    individuals(if (position >= 0) position else 0)
  }

}

object Population {

  val Size = 16
  val EliteCount = 4
  val MaxRotate = 500

}
