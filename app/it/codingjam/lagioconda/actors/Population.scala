package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.{CrossoverPointLike, Chromosome, RandomChromosome}

import scala.util.Random

case class Population(individuals: List[IndividualState]) {

  def runAGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions, crossover: CrossoverPointLike): Population = {
    val steps = individuals.size

    var newIndividuals = List[IndividualState]()

    Range(0, steps).foreach { step =>
      val r = Random.nextInt(100)
      if (r < 5) {
        // Mutation
        val chromosome = randomIndividual.chromosome.mutate
        val im = chromosome.toBufferedImage
        val fitness = fitnessFunction.fitness(im.toMat)
        newIndividuals = newIndividuals.filterNot(x => x.chromosome == chromosome)
        newIndividuals = newIndividuals :+ IndividualState(chromosome, fitness)
      } else {

// Crossover
        val r1: (Int, IndividualState) = randomPositionAndIndividual
        val c1: Chromosome = r1._2.chromosome
        val p1 = r1._1
        val c2 = randomIndividualInRange(p1).chromosome

        val newChromosomes: (Chromosome, Chromosome) = c1.crossover(c2)
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
    Population(l.take(2000))
  }

  def randomIndividual: IndividualState =
    individuals(Random.nextInt(individuals.size))

  def randomPositionAndIndividual: (Int, IndividualState) = {
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

  def randomIndividualByWeight: IndividualState = {
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

  def meanFitness: Double = individuals.map(_.fitness).sum / individuals.size

  def addIndividuals(list: List[IndividualState]) = {
    Population((this.individuals ++ list).sorted(Ordering[IndividualState]).reverse)
  }

}

object Population {

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions): Population = {
    val initialPopulation = 1000
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
