package it.codingjam.lagioconda.fitness

import it.codingjam.lagioconda.ga.Chromosome

trait FitnessFunction {

  def fitness(chromosome: Chromosome): Double
}
