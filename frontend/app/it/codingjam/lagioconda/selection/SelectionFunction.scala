package it.codingjam.lagioconda.selection

import it.codingjam.lagioconda.actors.{IndividualState, Population}

trait SelectionFunction {

  def select(population: Population): IndividualState

}
