package it.codingjam.lagioconda.config

import it.codingjam.lagioconda.ga.{CrossoverPointLike, MutationPointLike}
import it.codingjam.lagioconda.selection.SelectionFunction

case class PopulationConfig(size: Int, eliteCount: Int)

case class MutationConfig(chanceOfMutation: Int, mutation: MutationPointLike, mutationSize: Int, mutationTimes: Int)

case class AlgorithmConfig(mutation: MutationConfig,
                           crossover: CrossoverPointLike)

case class Config(population: PopulationConfig, algorithm: AlgorithmConfig, selection: SelectionFunction) {

}


