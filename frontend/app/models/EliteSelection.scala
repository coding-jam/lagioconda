package models

import it.codingjam.ga.{Population, SelectionFunction}
import it.codingjam.lagioconda.models.IndividualState

class EliteSelection extends SelectionFunction {
  override def select(population: Population): IndividualState = population.randomElite
}
