package models

import it.codingjam.lagioconda.{Population, SelectionFunction}
import it.codingjam.lagioconda.models.IndividualState

class EliteSelection extends SelectionFunction {
  override def select(population: Population): IndividualState = population.randomElite
}
