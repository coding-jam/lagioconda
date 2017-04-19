package it.codingjam.ga

import it.codingjam.lagioconda.models.IndividualState

import scala.util.Random

class WheelSelection extends SelectionFunction {

  override def select(population: Population): IndividualState = {
    val d = Random.nextDouble() * population.totalFitness
    val list = population.individuals.map(_.fitness).scanLeft(0.0)(_ + _)
    val p = population.individuals.size - list.takeWhile(d > _).size
    population.individuals(p)
  }
}
