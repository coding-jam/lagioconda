package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.domain.{Configuration, ImageDimensions}
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.{Chromosome, CrossoverPointLike, MutationPointLike, RandomChromosome}

import scala.util.Random

case class Population(generation: Int, individuals: List[IndividualState], trend: String = "") {

  def runAGeneration()(implicit fitnessFunction: FitnessFunction,
                       dimension: ImageDimensions,
                       crossover: CrossoverPointLike,
                       mutation: MutationPointLike): Population = {

    val i = individuals.splitAt((Population.Size * Population.EliteRatio).toInt)

    var newIndividuals = i._1 // start with elite

    Range(0, Population.Size - newIndividuals.size + Population.IncrementBeforeCut).foreach { step =>
      val r = Random.nextInt(100)
      if (r < 5) {
        // Mutation
        val chromosome = this.randomIndividual.chromosome.mutate(Population.NumberOfMutating)
        val fitness = fitnessFunction.fitness(chromosome)
        newIndividuals = newIndividuals :+ IndividualState(chromosome, fitness)
      } else {

// Crossover
        val c1 = randomIndividual.chromosome
        val c2 = randomIndividual.chromosome

        val newChromosomes: (Chromosome, Chromosome) = c1.onePointCrossover(c2)
        val list = List(newChromosomes._1, newChromosomes._2)
        list.foreach { c =>
          val fitness = fitnessFunction.fitness(c)
          val individual = IndividualState(c, fitness)
          newIndividuals = newIndividuals :+ individual
        }
      }
    }
    val l = newIndividuals.sorted(Ordering[IndividualState]).reverse
    val selectedIndividual = l.take(Population.Size)

    //hillClimb(
    Population(generation + 1, selectedIndividual)
    //)

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
    val individuals = (this.individuals ++ list).sorted(Ordering[IndividualState]).reverse
    Population(generation, individuals.take(Population.Size))
  }

  def hillClimb(pop: Population)(implicit fitnessFunction: FitnessFunction,
                                 mutationPointLike: MutationPointLike,
                                 dimensions: ImageDimensions): Population = {
    var hillClimber = pop.randomIndividual
    val firstHillClimber = hillClimber
    val firstFitness = firstHillClimber.fitness
    val r = Random.nextInt(Chromosome.numberOfGenes)

    Range(0, 10).foreach { i =>
      val neighbour = hillClimber.chromosome.neighbour(r)
      val fitness = fitnessFunction.fitness(neighbour)
      if (fitness > hillClimber.fitness) {
        hillClimber = IndividualState(neighbour, fitness)
      }
    }
    if (firstFitness < hillClimber.fitness) {
      val list = pop.individuals.filterNot(is => is == firstHillClimber)
      val newPop = Population(generation, list)
      newPop.addIndividuals(List(hillClimber))
    } else {
      pop
    }
  }

}

object Population {

  val Size = 40
  val EliteRatio = 20.0 / 100.0
  val IncrementBeforeCut = (Size * 10.0 / 100.0).toInt
  val NumberOfMutating = 10

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions, configuration: Configuration): Population = {

    var list: List[IndividualState] = List()

    Range(0, Size).foreach { i =>
      val c: Chromosome = RandomChromosome.generate()
      val fitness = fitnessFunction.fitness(c)
      val individual = IndividualState(c, fitness)
      list = list :+ individual
    }
    Population(0, list.sorted(Ordering[IndividualState].reverse))
  }

}
