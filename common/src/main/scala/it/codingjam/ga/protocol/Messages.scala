package it.codingjam.ga.protocol

import it.codingjam.lagioconda.ga.Chromosome

trait Message {}

object Messages {

  case class CalculateFitness(chromosome: Chromosome, generation: Int, reason: String) extends Message

  case class CalculatedFitness(chromosome: Chromosome, reason: String, fitness: Double) extends Message

}
