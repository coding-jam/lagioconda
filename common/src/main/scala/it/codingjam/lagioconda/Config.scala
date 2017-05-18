package it.codingjam.lagioconda.config

import it.codingjam.lagioconda.ChromosomeOps
import it.codingjam.lagioconda.ChromosomeOps.CombineChromosome
import it.codingjam.lagioconda.ga.{CrossoverPointLike, MutationPointLike, RandomCrossoverPoint, RandomMutationPoint}
import it.codingjam.lagioconda.population.Population
import it.codingjam.lagioconda.selection.{SelectionFunction, WheelSelection}

case class PopulationConfig(size: Int, eliteCount: Int, numberOfGenes: Int)

case class MutationConfig(chance: Int, strategy: MutationPointLike, size: Int, times: Int)

case class AlgorithmConfig(mutation: MutationConfig, crossoverPoint: CrossoverPointLike, crossover: CombineChromosome)

case class Config(population: PopulationConfig, algorithm: AlgorithmConfig, selection: SelectionFunction, hillClimb: HillClimbConfig)

case class HillClimbConfig(active: Boolean, slopeHeight: Double, slopeSize: Int, addGene: Boolean, fullGeneHillClimbChange: Int)

object PopulationConfig {
  val Default = PopulationConfig(Population.Size, Population.EliteCount, 1)
}

object AlgorithmConfig {
  val Default = AlgorithmConfig(MutationConfig.Default, new RandomCrossoverPoint, ChromosomeOps.uniformCrossover)
}

object MutationConfig {
  val Default = MutationConfig(chance = 15, strategy = new RandomMutationPoint, 1, 1)
}

object Config {
  val Default = Config(PopulationConfig.Default, AlgorithmConfig.Default, new WheelSelection, HillClimb.Default)
}

object HillClimb {
  val Default =
    HillClimbConfig(active = true, slopeHeight = 0.0001, slopeSize = Population.MaxRotate, addGene = true, fullGeneHillClimbChange = 5)
}
