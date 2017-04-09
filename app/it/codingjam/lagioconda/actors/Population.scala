package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.domain.{Configuration, ImageDimensions}
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga._
import it.codingjam.lagioconda.selection.SelectionFunction

import scala.util.Random

case class Population(generation: Int,
                      individuals: List[IndividualState],
                      totalFitness: Double,
                      newBestAtGeneration: Int,
                      bestReason: String,
                      trend: String = "") {

  def nextGeneration(implicit fitnessFunction: FitnessFunction,
                     selection: SelectionFunction,
                     mutation: MutationPointLike,
                     dimension: ImageDimensions,
                     crossover: CrossoverPointLike): Population = {
    var temp = this.crossOver().mutation()
    val oldBest = this.individuals.head
    val newBest = temp.individuals.head

    if (generation - newBestAtGeneration > 0) {
      temp = Population.hillClimb(temp, generation % Chromosome.numberOfGenes, 10)
    }

    if (oldBest.fitness < newBest.fitness)
      Population(generation + 1, temp.individuals, temp.totalFitness, generation + 1, newBest.generatedBy)
    else
      Population(generation + 1, temp.individuals, temp.totalFitness, this.newBestAtGeneration, oldBest.generatedBy)

  }

  def crossOver()(implicit fitnessFunction: FitnessFunction,
                  selection: SelectionFunction,
                  dimension: ImageDimensions,
                  crossover: CrossoverPointLike): Population = {

    val splitted = individuals.splitAt(Population.EliteCount)
    var newIndividuals = splitted._1 // start with elite
    Range(Population.EliteCount, Population.Size).foreach { i =>
      val selected = selection.select(this)

      val offspring = individuals(i).chromosome.uniformCrossover(selected.chromosome)
      val fitness = fitnessFunction.fitness(offspring)

      val newIndividual = IndividualState(offspring, fitness, "crossover")
      newIndividuals = newIndividuals :+ newIndividual

    }
    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation, sort(newIndividuals), totalFitness, newBestAtGeneration, bestReason)
  }

  def mutation()(implicit fitnessFunction: FitnessFunction, mutation: MutationPointLike): Population = {
    val splitted = individuals.splitAt(Population.EliteCount)
    var newIndividuals = splitted._1 // start with elite

    val chanceOfMutation = 5

    Range(Population.EliteCount, Population.Size).foreach { i =>
      val individual = individuals(i)
      val r = Random.nextInt(100)
      if (r < chanceOfMutation) {
        val mutated = individual.chromosome.mutate(mutation, 40)
        val fitness = fitnessFunction.fitness(mutated)
        newIndividuals = newIndividuals :+ IndividualState(mutated, fitness, "mutation")
      } else {
        newIndividuals = newIndividuals :+ individual
      }
    }
    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation, sort(newIndividuals), totalFitness, newBestAtGeneration, bestReason)

  }

  def runAGeneration()(implicit fitnessFunction: FitnessFunction,
                       dimension: ImageDimensions,
                       crossover: CrossoverPointLike,
                       mutation: MutationPointLike): Population = {

    // standard genetic algorithm
    val bestFitness = individuals.head.fitness

    val i = individuals.splitAt(Population.EliteCount)

    var newIndividuals = i._1 // start with elite

    val chanceOfMutation = (generation - newBestAtGeneration).min(70).max(5)
    val sizeOfMutation = (50 * (1 - (individuals.head.fitness - 0.70))).toInt
    val numberOfMutating = (Chromosome.numberOfGenes * (1 - (individuals.head.fitness - 0.70))).toInt
    println("size of mut " + sizeOfMutation + " over genes " + numberOfMutating)
    Range(0, Population.Size + 5).foreach { step =>
      val r = Random.nextInt(100)

      if (r < chanceOfMutation) {
        // Mutation
        val chromosome: Chromosome = this.randomIndividual.chromosome.mutate(numberOfMutating)(mutation, sizeOfMutation)
        val fitness = fitnessFunction.fitness(chromosome)
        // newIndividuals = newIndividuals :+ IndividualState(chromosome, fitness, "mutation")
        newIndividuals = addIfNotClone(newIndividuals, IndividualState(chromosome, fitness, "mutation"))
      } else {

// Crossover
        val c1 = randomElite.chromosome
        val c2 = randomIndividual.chromosome

        val newChromosomes: (Chromosome, Chromosome) = c1.fullCrossover(c2)
        val list = List(newChromosomes._1, newChromosomes._2)
        list.foreach { c =>
          val fitness = fitnessFunction.fitness(c)
          val individual = IndividualState(c, fitness, "crossover")
          newIndividuals = addIfNotClone(newIndividuals, individual)
        }
      }
    }
    val l = newIndividuals.sorted(Ordering[IndividualState]).reverse
    val selectedIndividual = l.take(Population.Size)

    val newBestFitness = selectedIndividual.head.fitness

    val bestGeneration = if (newBestFitness > bestFitness) generation + 1 else newBestAtGeneration
    val total = selectedIndividual.map(_.fitness).sum

    val newPopulation = Population(generation + 1, selectedIndividual, total, bestGeneration, selectedIndividual.head.generatedBy)

    if ((generation + 1) - newBestAtGeneration > 10)
      Population.hillClimb(newPopulation, newPopulation.generation % Chromosome.numberOfGenes, (generation - newBestAtGeneration).min(50))
    else
      newPopulation

  }

  private def sort(list: List[IndividualState]) = list.sorted(Ordering[IndividualState]).reverse

  def addIfNotClone(list: List[IndividualState], newIndividual: IndividualState) = {
    val l: Seq[IndividualState] = list.filter(i => i.fitness == newIndividual.fitness)
    val m: Seq[Set[Gene]] = l.map(i => i.chromosome.genes.toSet)
    val n = m.find(set => set.equals(newIndividual.chromosome.genes.toSet))
    if (n.isDefined)
      list
    else list :+ newIndividual
  }

  def bestIndividual = individuals.head

  def randomIndividual: IndividualState =
    individuals(Random.nextInt(individuals.size))

  def randomElite: IndividualState = individuals(Random.nextInt(Population.EliteCount))

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

  def meanFitness: Double = totalFitness / individuals.size

  def addIndividuals(list: List[IndividualState]) = {
    val individuals = (this.individuals ++ list).sorted(Ordering[IndividualState]).reverse
    Population(generation, individuals.take(Population.Size), 0.0, newBestAtGeneration, bestReason)
  }

}

object Population {

  val Size = 30
  val EliteCount = 2
  val IncrementBeforeCut = (Size * 10.0 / 100.0).toInt
  //val NumberOfMutatingGenes: Int = (Size * 50.0 / 100.0).toInt

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions, configuration: Configuration): Population = {

    var list: List[IndividualState] = List()

    Range(0, Size).foreach { i =>
      val c: Chromosome = RandomChromosome.generate()
      val fitness = fitnessFunction.fitness(c)
      val individual = IndividualState(c, fitness, "random")
      list = list :+ individual
    }

    Population(0, list.sorted(Ordering[IndividualState].reverse), list.map(_.fitness).sum, 0, "random")
  }

  def hillClimb(pop: Population, gene: Int, lenght: Int)(implicit fitnessFunction: FitnessFunction,
                                                         mutationPointLike: MutationPointLike,
                                                         dimensions: ImageDimensions): Population = {
    var hillClimber = pop.bestIndividual
    val firstHillClimber = hillClimber
    val firstFitness = firstHillClimber.fitness
    val r = gene // Random.nextInt(Chromosome.numberOfGenes)

    // doing a hill climbing phase last like a single population generation
    Range(0, lenght).foreach { i =>
      val neighbour = hillClimber.chromosome.neighbour(r)
      val fitness = fitnessFunction.fitness(neighbour)
      if (fitness > hillClimber.fitness) {
        hillClimber = IndividualState(neighbour, fitness, "hillclimb")
      }
    }

    if (firstFitness < hillClimber.fitness) {
      val list = (List(hillClimber) ++ pop.individuals).take(Population.Size)
      val total = list.map(_.fitness).sum
      println("Hill clim OK at " + (pop.generation + 1))
      Population(pop.generation, list, total, pop.newBestAtGeneration, bestReason = "hillclimb")
    } else {
      println("hill climb failed at " + (pop.generation + 1))
      pop
    }
  }

}
