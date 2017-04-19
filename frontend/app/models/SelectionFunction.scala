package it.codingjam.ga

import it.codingjam.lagioconda.models.IndividualState

trait SelectionFunction {
  def select(population: Population): IndividualState
}
