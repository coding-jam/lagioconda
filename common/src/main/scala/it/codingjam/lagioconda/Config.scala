package it.codingjam.lagioconda.config

import it.codingjam.lagioconda.ChromosomeOps
import it.codingjam.lagioconda.ChromosomeOps.CombineChromosome
import it.codingjam.lagioconda.ga.{CrossoverPointLike, MutationPointLike, RandomCrossoverPoint, RandomMutationPoint}
import it.codingjam.lagioconda.population.Population
import it.codingjam.lagioconda.selection.{SelectionFunction, WheelSelection}

case class PopulationConfig(size: Int, eliteCount: Int, numberOfGenes: Int)

case class MutationConfig(chance: Int, strategy: MutationPointLike, size: Int, times: Int)

case class AlgorithmConfig(mutation: MutationConfig, crossoverPoint: CrossoverPointLike, crossover: CombineChromosome)

case class Config(population: PopulationConfig,
                  alpha: Int,
                  algorithm: AlgorithmConfig,
                  selection: SelectionFunction,
                  hillClimb: HillClimbConfig)

case class HillClimbConfig(active: Boolean, slopeHeight: Double, slopeSize: Int, addGene: Boolean, fullGeneHillClimbChange: Int)

object PopulationConfig {
  val Default = PopulationConfig(Population.Size, Population.EliteCount, 250)

  val VecGen = PopulationConfig(Population.Size, Population.EliteCount, 1)
}

object AlgorithmConfig {
  val Default = AlgorithmConfig(MutationConfig.Default, new RandomCrossoverPoint, ChromosomeOps.uniformCrossover)
}

object MutationConfig {
  val Default = MutationConfig(chance = 5, strategy = new RandomMutationPoint, 1, 1)
}

object HillClimb {
  val Default =
    HillClimbConfig(active = true, slopeHeight = 0.0001, slopeSize = Population.MaxRotate, addGene = false, fullGeneHillClimbChange = 5)

  val Off = Default.copy(active = false)

  val VecGenLike = Default.copy(addGene = true)
}

object Config {

  val VanillaGa = Config(PopulationConfig.Default, 128, AlgorithmConfig.Default, new WheelSelection, HillClimb.Off)

  val GaWithHillClimb = Config(PopulationConfig.Default, 128, AlgorithmConfig.Default, new WheelSelection, HillClimb.Default)

  val VecGenLike = Config(PopulationConfig.VecGen, 220, AlgorithmConfig.Default, new WheelSelection, HillClimb.VecGenLike)

}
