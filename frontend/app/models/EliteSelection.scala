package models

import it.codingjam.lagioconda.{Population, SelectionFunction}
import it.codingjam.lagioconda.models.Individual

class EliteSelection extends SelectionFunction {
  override def select(population: Population): Individual = population.randomElite
}
