package it.codingjam.lagioconda.selection

import it.codingjam.lagioconda.population.{Individual, Population}

trait SelectionFunction {
  def select(population: Population): Individual
}
