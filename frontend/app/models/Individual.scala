package it.codingjam.lagioconda.models

import it.codingjam.lagioconda.ga.Chromosome

case class Individual(chromosome: Chromosome, fitness: Double, generatedBy: String) extends Ordered[Individual] {

  def compare(that: Individual): Int = fitness.compareTo(that.fitness)

}
