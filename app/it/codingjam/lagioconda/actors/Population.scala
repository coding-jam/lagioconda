package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.{
  CrossoverPointLike,
  Chromosome,
  RandomChromosome
}

import scala.util.Random

case class Population(individuals: List[IndividualState]) {

  def runAGeneration()(implicit fitnessFunction: FitnessFunction,
                       dimension: ImageDimensions,
                       crossover: CrossoverPointLike): Population = {
    val steps = individuals.size / 2

    var newIndividuals = individuals

    Range(0, steps).foreach { step =>
      val r = Random.nextInt(200)
      if (r < 20) {
        // Mutation
        val chromosome = randomIndividual.chromosome.mutate
        val im = chromosome.toBufferedImage
        val fitness = fitnessFunction.fitness(im.toMat)
        newIndividuals =
          newIndividuals.filterNot(x => x.chromosome == chromosome)
        newIndividuals = newIndividuals :+ IndividualState(chromosome, fitness)
      } else {

// Crossover
        val c1 = randomIndividualByWeight.chromosome
        val c2 = randomIndividualByWeight.chromosome

        val newChromosomes: (Chromosome, Chromosome) =
          c1.crossover(c2)
        val list = List(newChromosomes._1, newChromosomes._2)
        list.foreach { c =>
          val im = c.toBufferedImage
          val mat = im.toMat
          val fitness = fitnessFunction.fitness(mat)
          mat.release()
          mat._deallocate()

          val individual = IndividualState(c, fitness)
          newIndividuals = newIndividuals :+ individual
        }
      }
    }
    val l = newIndividuals.sorted(Ordering[IndividualState]).reverse
    Population(l.take(1000))
  }

  private def randomIndividual: IndividualState =
    individuals(Random.nextInt(individuals.size))

  private def randomIndividualByWeight: IndividualState = {
    val total = individuals.size * (individuals.size + 1) / 2
    var r = Random.nextInt(total)
    var x = individuals.size
    while (r > 0) {
      r = r - x
      x = x - 1
    }
    individuals(individuals.size - x - 1)
  }

  def meanFitness: Double = individuals.map(_.fitness).sum / individuals.size

}

object Population {

  def randomGeneration()(implicit fitnessFunction: FitnessFunction,
                         dimension: ImageDimensions): Population = {
    val initialPopulation = 500
    var list: List[IndividualState] = List()

    Range(0, initialPopulation).foreach { i =>
      val c: Chromosome = RandomChromosome.generate()
      val im = c.toBufferedImage
      val mat = im.toMat
      val fitness = fitnessFunction.fitness(mat)
      mat.release()
      mat._deallocate()
      val individual = IndividualState(c, fitness)
      list = list :+ individual
    }
    Population(list.sorted(Ordering[IndividualState].reverse))
  }

}
