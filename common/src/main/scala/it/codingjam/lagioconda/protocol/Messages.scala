package it.codingjam.lagioconda.protocol

import it.codingjam.lagioconda.ga.Chromosome

trait Message

object Message {

  case class CalculateFitness(chromosome: Chromosome, generation: Int, reason: String) extends Message

  case class CalculatedFitness(chromosome: Chromosome, reason: String, fitness: Double) extends Message

}
