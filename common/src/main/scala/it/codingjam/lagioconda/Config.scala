package it.codingjam.lagioconda.config

import it.codingjam.lagioconda.ga.{CrossoverPointLike, MutationPointLike, RandomCrossoverPoint, RandomMutationPoint}
import it.codingjam.lagioconda.population.Population
import it.codingjam.lagioconda.selection.{SelectionFunction, WheelSelection}

case class PopulationConfig(size: Int, eliteCount: Int)

case class MutationConfig(chance: Int, strategy: MutationPointLike, size: Int, times: Int)

case class AlgorithmConfig(mutation: MutationConfig, crossover: CrossoverPointLike)

case class Config(population: PopulationConfig, algorithm: AlgorithmConfig, selection: SelectionFunction)

object PopulationConfig {
  val Default = PopulationConfig(Population.Size, Population.EliteCount)
}

object AlgorithmConfig {
  val Default = AlgorithmConfig(MutationConfig.Default, new RandomCrossoverPoint)
}

object MutationConfig {
  val Default = MutationConfig(chance = 15, strategy = new RandomMutationPoint, 1, 1)
}

object Config {
  val Default = Config(PopulationConfig.Default, AlgorithmConfig.Default, new WheelSelection)
}
