package it.codingjam.lagioconda

import it.codingjam.lagioconda.population.Individual

import scala.util.Random

class WheelSelection extends SelectionFunction {

  override def select(population: Population): Individual = {
    val totalFitness = population.individuals.map(_.fitness).sum
    val d = Random.nextDouble() * totalFitness
    val list = population.individuals.map(_.fitness).scanLeft(0.0)(_ + _)
    val p = population.individuals.size - list.takeWhile(d > _).size
    population.individuals(if (p < population.individuals.length) p else population.individuals.length - 1)
  }
}
