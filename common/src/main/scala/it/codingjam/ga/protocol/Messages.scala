package it.codingjam.ga.protocol

import it.codingjam.lagioconda.ga.Chromosome

trait Message {}

object Messages {

  case class CalculateFitness(chromosome: Chromosome) extends Message

  case class CalculatedFitness(chromosome: Chromosome, fitness: Double) extends Message

}
