package it.codingjam.ga.protocol

import it.codingjam.lagioconda.ga.Chromosome

trait Message {}

object Messages {

  case class CalculateFitness(chromosome: Chromosome) extends Message

  case class CalculatedFitness(fitness: Double) extends Message

}
