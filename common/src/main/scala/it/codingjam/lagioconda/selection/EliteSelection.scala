package it.codingjam.lagioconda.selection

import it.codingjam.lagioconda.population.{Individual, Population}

class EliteSelection extends SelectionFunction {
  override def select(population: Population): Individual = population.randomElite
}
