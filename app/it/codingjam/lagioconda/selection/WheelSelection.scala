package it.codingjam.lagioconda.selection
import it.codingjam.lagioconda.actors.{IndividualState, Population}

import scala.util.Random

class WheelSelection extends SelectionFunction {

  override def select(population: Population): IndividualState = {
    val d = Random.nextDouble() * population.totalFitness
    val list = population.individuals.map(_.fitness).scanLeft(0.0)(_ + _)
    val p = population.individuals.size - list.takeWhile(d > _).size
    population.individuals(p)
  }
}
