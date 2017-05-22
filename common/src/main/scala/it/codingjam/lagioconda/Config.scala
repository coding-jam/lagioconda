package it.codingjam.lagioconda.config

import it.codingjam.lagioconda.{ChromosomeOps, GeneMapping}
import it.codingjam.lagioconda.ChromosomeOps.CombineChromosome
import it.codingjam.lagioconda.ga.{CrossoverPointLike, MutationPointLike, RandomCrossoverPoint, RandomMutationPoint}
import it.codingjam.lagioconda.population.Population
import it.codingjam.lagioconda.selection.{SelectionFunction, WheelSelection}

case class PopulationConfig(size: Int, eliteCount: Int, numberOfGenes: Int, geneMapping: GeneMapping)

case class MutationConfig(chance: Int, strategy: MutationPointLike, size: Int, times: Int)

case class AlgorithmConfig(mutation: MutationConfig, crossoverPoint: CrossoverPointLike, crossover: CombineChromosome)

case class Config(population: PopulationConfig,
                  alpha: Int,
                  algorithm: AlgorithmConfig,
                  selection: SelectionFunction,
                  hillClimb: HillClimbConfig)

case class HillClimbConfig(active: Boolean,
                           slopeHeight: Double,
                           slopeSize: Int,
                           addGene: Boolean,
                           fullGeneHillClimbChance: Int,
                           randomGene: Boolean,
                           lastGene: Boolean)

object GeneMappingConfig {
  val Default = GeneMapping(8, 16, 24, 32, 40, 48)
  val SmallRadius = GeneMapping(8, 16, 20, 28, 36, 44)
}

object PopulationConfig {
  val Default = PopulationConfig(Population.Size, Population.EliteCount, 300, GeneMappingConfig.SmallRadius)

  val VecGen = PopulationConfig(Population.Size, Population.EliteCount, 1, GeneMappingConfig.Default)
}

object AlgorithmConfig {
  val Default = AlgorithmConfig(MutationConfig.Default, new RandomCrossoverPoint, ChromosomeOps.uniformCrossover)
  val GaWithHillClimb = AlgorithmConfig(MutationConfig.GaWithHillClimb, new RandomCrossoverPoint, ChromosomeOps.uniformCrossover)
  val GeneCrossover = AlgorithmConfig(MutationConfig.Default, new RandomCrossoverPoint, ChromosomeOps.genesCrossover)
}

object MutationConfig {
  val Default = MutationConfig(chance = 5, strategy = new RandomMutationPoint, 1, 4)
  val GaWithHillClimb = MutationConfig(chance = 5, strategy = new RandomMutationPoint, 1, 3)
}

object HillClimb {
  val Default =
    HillClimbConfig(active = true,
                    slopeHeight = 0.001,
                    slopeSize = 200,
                    addGene = false,
                    fullGeneHillClimbChance = 5,
                    randomGene = true,
                    lastGene = false)

  val Off = Default.copy(active = false)

  val VecGenLike =
    Default.copy(addGene = true, slopeHeight = 0.0001, slopeSize = 500, fullGeneHillClimbChance = 5, randomGene = false, lastGene = true)
}

object Config {

  val VanillaGa = Config(PopulationConfig.Default, 220, AlgorithmConfig.Default, new WheelSelection, HillClimb.Off)

  val GaWithHillClimb = Config(PopulationConfig.Default, 255, AlgorithmConfig.GaWithHillClimb, new WheelSelection, HillClimb.Default)

  val VecGenLike = Config(PopulationConfig.VecGen, 220, AlgorithmConfig.GeneCrossover, new WheelSelection, HillClimb.VecGenLike)

}
