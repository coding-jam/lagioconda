package it.codingjam.lagioconda

import com.typesafe.scalalogging.{LazyLogging, Logger}
import it.codingjam.lagioconda.config.Config
import it.codingjam.lagioconda.ga.{Chromosome, Gene, RandomChromosome}
import it.codingjam.lagioconda.population.{Individual, Population}

import scala.util.Random

object PopulationOps extends LazyLogging {

  override lazy val logger = Logger(classOf[Population])

  private def sort(list: List[Individual]) = list.sorted(Ordering[Individual]).reverse

  def crossover(population: Population)(implicit fitnessCalculator: FitnessCalculator, config: Config): Population = {

    val newGeneration = population.generation + 1

    val splitted = population.individuals.splitAt(config.population.eliteCount)
    var newIndividuals = splitted._1 // start with elite
    val offsprings = Range(0, config.population.size).map { _ =>
      val selected1: Individual = config.selection.select(population)
      var selected2 = config.selection.select(population)
      while (selected1 == selected2) selected2 = config.selection.select(population)
      config.algorithm.crossover(selected1.chromosome, selected2.chromosome, config.algorithm.crossoverPoint)
    }.toList

    val mutationConfig = config.algorithm.mutation

    val mutationList: List[(Chromosome, String)] = offsprings.map { individual =>
      val r = Random.nextInt(100)

      if (r < config.algorithm.mutation.chance)
        (individual.mutate(mutationConfig.times)(mutationConfig.strategy, mutationConfig.size), "mutation")
      else
        (individual, "crossover")
    }

    val l = fitnessCalculator.calculate(mutationList, newGeneration)

    newIndividuals = newIndividuals ++ l

    Population(newGeneration, sort(newIndividuals), population.hillClimbedGene, population.lastResults)
  }

  def addGene(population: Population)(implicit fitnessCalculator: FitnessCalculator, config: Config): Population = {
    logger.debug("Adding gene")

    val bestIndividual = population.bestIndividual
    val clist: List[(Chromosome, String)] =
      Range(0, population.individuals.length).map(i => (bestIndividual.chromosome.addRandomGene, "gene")).toList
    val newIndividuals: List[Individual] = fitnessCalculator.calculate(clist, population.generation)

    population.copy(individuals = sort(newIndividuals))

  }

  def hillClimb(population: Population, gene: Int)(implicit fitnessCalculator: FitnessCalculator, config: Config): Population = {

    def rotate(list: List[Double], double: Double) = (list :+ double).takeRight(Population.MaxRotate)

    def neighbour(chromosome: Chromosome, gene: Int): List[Chromosome] = {
      it.codingjam.lagioconda.conversions.neigh(chromosome.genes(gene)).map { g =>
        Chromosome(chromosome.genes.slice(0, gene) ++ List(g) ++ chromosome.genes.slice(gene + 1, chromosome.genes.length))
      }
    }

    logger.debug(
      s"Hill climb with gene ${gene} at generation ${population.generation}, fitness ${population.bestIndividual.fitness.toString}")

    def hc(chosen: Individual): Individual = {
      val l: List[(Chromosome, String)] = neighbour(chosen.chromosome, gene).map(x => (x, "hillClimb"))

      val newIndividuals = fitnessCalculator.calculate(l, population.generation)
      sort(newIndividuals).head
    }

    val startingPoint = population.bestIndividual
    val hillClimber = hc(startingPoint)
    val bestSoFar = if (startingPoint.fitness > hillClimber.fitness) startingPoint else hillClimber

    if (population.bestIndividual.fitness < bestSoFar.fitness) {
      val selected = (List(bestSoFar) ++ population.individuals).dropRight(1)
      logger.debug(s"Successfully HillClimb with gene ${gene}")
      val lastI = bestSoFar.fitness - population.bestIndividual.fitness

      Population(population.generation, selected, gene, rotate(population.lastResults, lastI))
    } else {
      logger.debug(s"Failed* HillClimb with gene ${gene}")
      population.copy(hillClimbedGene = (population.hillClimbedGene + 1) % population.bestIndividual.chromosome.genes.length,
                      lastResults = rotate(population.lastResults, 0.0))
    }

  }

  private def addIfNotClone(list: List[Individual], newIndividual: Individual): List[Individual] = {
    val l: Seq[Individual] = list.filter(i => i.fitness == newIndividual.fitness)
    val m: Seq[Set[Gene]] = l.map(i => i.chromosome.genes.toSet)
    val n = m.find(set => set.equals(newIndividual.chromosome.genes.toSet))
    if (n.isDefined)
      list
    else list :+ newIndividual
  }

  def randomGeneration()(implicit fitnessCalculator: FitnessCalculator, config: Config): Population = {

    val r = Range(0, Population.Size)
    println(config)
    println(config.population)
    println(config.population.numberOfGenes)
    val g = RandomChromosome.generate(Gene.Size, config.population.numberOfGenes)
    val chromosomeList: List[(Chromosome, String)] =
      r.map(i => (RandomChromosome.generate(Gene.Size, config.population.numberOfGenes), "gene")).toList
    val newIndividuals: List[Individual] = fitnessCalculator.calculate(chromosomeList, 0)

    Population(0, sort(newIndividuals), 0, List())
  }

}
