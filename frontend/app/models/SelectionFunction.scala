package it.codingjam.lagioconda

import it.codingjam.lagioconda.population.Individual

trait SelectionFunction {
  def select(population: Population): Individual
}
