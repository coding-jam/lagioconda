package it.codingjam.lagioconda

import it.codingjam.lagioconda.ga.{Chromosome, CrossoverPointLike, Gene}

import scala.util.Random

object ChromosomeOps {

  type CombineChromosome = (Chromosome, Chromosome, CrossoverPointLike) => Chromosome

  val uniformCrossover = (one: Chromosome, other: Chromosome, crossoverPoint: CrossoverPointLike) => {
    val l = one.genes
      .zip(other.genes)
      .map { case (g1, g2) => shuffle(g1, g2) }
      .unzip

    Chromosome(l._1, one.geneMapping)
  }

  private def shuffle(g1: Gene, g2: Gene): (Gene, Gene) = {
    val b = Random.nextBoolean()
    if (b)
      (g1, g2)
    else
      (g2, g1)
  }

}
