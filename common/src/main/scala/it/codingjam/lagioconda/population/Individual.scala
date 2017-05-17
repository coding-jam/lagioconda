package it.codingjam.lagioconda.population

import it.codingjam.lagioconda.ga.Chromosome

case class Individual(chromosome: Chromosome, fitness: Double, generatedBy: String, age: Int) extends Ordered[Individual] {

  def compare(that: Individual): Int = fitness.compareTo(that.fitness)

}
