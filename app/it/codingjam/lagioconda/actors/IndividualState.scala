package it.codingjam.lagioconda.actors

import it.codingjam.lagioconda.ga.Chromosome

case class IndividualState(chromosome: Chromosome, fitness: Double) extends Ordered[IndividualState] {

  def compare(that: IndividualState): Int = fitness.compareTo(that.fitness)

}
