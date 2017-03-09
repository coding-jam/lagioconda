package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.{Chromosome, CrossoverPointLike, MutationPointLike, RandomChromosome}

import scala.util.Random

case class Population(generation: Int, individuals: List[IndividualState]) {

  def runAGeneration()(implicit fitnessFunction: FitnessFunction,
                       dimension: ImageDimensions,
                       crossover: CrossoverPointLike,
                       mutation: MutationPointLike): Population = {
    val steps = individuals.size / 2

    val i = individuals.splitAt(Population.Size / 2)

    var newIndividuals = i._1 // elite
    val worstIndividuals = Population(generation, i._2)

    Range(0, steps).foreach { step =>
      val r = Random.nextInt(100)
      if (r < 5) {
        // Mutation
        val chromosome = worstIndividuals.randomIndividual.chromosome.mutate(3)
        val fitness = fitnessFunction.fitness(chromosome)
        newIndividuals = newIndividuals :+ IndividualState(chromosome, fitness)
      } else {

// Crossover
        val c1 = worstIndividuals.randomIndividual.chromosome
        val c2 = worstIndividuals.randomIndividual.chromosome

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

  val Size = 200

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions): Population = {

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
