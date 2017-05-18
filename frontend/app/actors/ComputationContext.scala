package actors

import it.codingjam.lagioconda.ga.Temperature
import it.codingjam.lagioconda.population.Individual

case class ComputationContext(best: Option[Individual] = None,
                              hillClimbedGene: Int = 0,
                              lastResults: List[Double] = List(),
                              temperature: Temperature)
