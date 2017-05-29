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

case object GeneMappingConfig {
  val Default = GeneMapping(mX = 8, mY = 16, mR = 24, mRed = 32, mGreen = 40, mBlue = 48)
  val SmallRadius = GeneMapping(mX = 8, mY = 16, mR = 20, mRed = 28, mGreen = 36, mBlue = 44)
}

case object PopulationConfig {
  val Ga = PopulationConfig(size = Population.Size,
                            eliteCount = Population.EliteCount,
                            numberOfGenes = 250,
                            geneMapping = GeneMappingConfig.SmallRadius)

  val VecGen = PopulationConfig(size = Population.Size,
                                eliteCount = Population.EliteCount,
                                numberOfGenes = 1,
                                geneMapping = GeneMappingConfig.Default)

}

case object AlgorithmConfig {
  val Default = AlgorithmConfig(mutation = MutationConfig.Default,
                                crossoverPoint = new RandomCrossoverPoint,
                                crossover = ChromosomeOps.uniformCrossover)
  val GaWithHillClimb = AlgorithmConfig(mutation = MutationConfig.GaWithHillClimb,
                                        crossoverPoint = new RandomCrossoverPoint,
                                        crossover = ChromosomeOps.uniformCrossover)
  val GeneCrossover =
    AlgorithmConfig(mutation = MutationConfig.Default, crossoverPoint = new RandomCrossoverPoint, crossover = ChromosomeOps.genesCrossover)
}

case object MutationConfig {
  val Default = MutationConfig(chance = 5, strategy = new RandomMutationPoint, size = 1, times = 4)
  val GaWithHillClimb = MutationConfig(chance = 5, strategy = new RandomMutationPoint, size = 1, times = 3)
}

case object HillClimb {
  val Default =
    HillClimbConfig(active = true,
                    slopeHeight = 0.0001,
                    slopeSize = 200,
                    addGene = false,
                    fullGeneHillClimbChance = 0,
                    randomGene = true,
                    lastGene = false)

  val Off = Default.copy(active = false)

  val VecGenLike =
    Default.copy(addGene = true, slopeHeight = 0.0001, slopeSize = 500, fullGeneHillClimbChance = 5, randomGene = false, lastGene = true)
}

case object Config {

  val VanillaGa = Config(population = PopulationConfig.Ga,
                         alpha = 255,
                         algorithm = AlgorithmConfig.Default,
                         selection = new WheelSelection,
                         hillClimb = HillClimb.Off)

  val GaWithHillClimb = Config(population = PopulationConfig.Ga,
                               alpha = 255,
                               algorithm = AlgorithmConfig.GaWithHillClimb,
                               selection = new WheelSelection,
                               hillClimb = HillClimb.Default)

  val VecGenLike = Config(population = PopulationConfig.VecGen,
                          alpha = 200,
                          algorithm = AlgorithmConfig.GeneCrossover,
                          selection = new WheelSelection,
                          hillClimb = HillClimb.VecGenLike)

}
