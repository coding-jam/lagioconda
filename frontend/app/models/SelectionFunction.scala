package it.codingjam.lagioconda

import it.codingjam.lagioconda.models.Individual

trait SelectionFunction {
  def select(population: Population): Individual
}
