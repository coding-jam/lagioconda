package models

import it.codingjam.lagioconda.population.Individual
import it.codingjam.lagioconda.{Population, SelectionFunction}

class EliteSelection extends SelectionFunction {
  override def select(population: Population): Individual = population.randomElite
}
