package it.codingjam.lagioconda

import it.codingjam.lagioconda.config.Config
import it.codingjam.lagioconda.ga.Chromosome

import scala.util.Random

package object population {

  private def sort(list: List[Individual]) = list.sorted(Ordering[Individual]).reverse


  def crossover(population: Population)(implicit fitnessCalculator: FitnessCalculator, config: Config) = {

    val newGeneration = population.generation + 1

    val splitted = population.individuals.splitAt(config.population.eliteCount)
    var newIndividuals = splitted._1 // start with elite
    val offsprings = Range(0, config.population.size).map { i =>
      val selected1: Individual = config.selection.select(population)
      var selected2 = config.selection.select(population)
      while (selected1 == selected2) selected2 = config.selection.select(population)
      selected1.chromosome.uniformCrossover(selected2.chromosome)
    }.toList

    val mutationConfig = config.algorithm.mutation

    val mutationList: List[(Chromosome, String)] = offsprings.map { individual =>
      val r = Random.nextInt(100)

      if (r < config.algorithm.mutation.chanceOfMutation)
        (individual.mutate(mutationConfig.mutationTimes)(mutationConfig.mutation, mutationConfig.mutationSize), "mutation")
      else
        (individual, "crossover")
    }

    val l = fitnessCalculator.calculate(mutationList, newGeneration)

    newIndividuals = newIndividuals ++ l

    Population(newGeneration, sort(newIndividuals), population.hillClimbedGene, population.lastResults)
  }


}
