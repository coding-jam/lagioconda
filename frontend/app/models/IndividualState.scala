package it.codingjam.lagioconda.models

import it.codingjam.lagioconda.ga.Chromosome

case class IndividualState(chromosome: Chromosome, fitness: Double, generatedBy: String) extends Ordered[IndividualState] {

  def compare(that: IndividualState): Int = fitness.compareTo(that.fitness)

}
