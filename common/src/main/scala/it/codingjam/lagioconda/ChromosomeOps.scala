package it.codingjam.lagioconda

import it.codingjam.lagioconda.ga.{Chromosome, CrossoverPointLike, Gene, RandomGene}

import scala.util.Random

object ChromosomeOps {

  type CombineChromosome = (Chromosome, Chromosome, CrossoverPointLike) => Chromosome

  val genesCrossover = (one: Chromosome, other: Chromosome, crossoverPoint: CrossoverPointLike) => {
    val l: List[Gene] = one.genes
      .zip(other.genes)
      .map {
        case (g1, g2) => g1.crossOver(g2)(crossoverPoint)
      }

    Chromosome(l)
  }

  val uniformCrossover = (one: Chromosome, other: Chromosome, crossoverPoint: CrossoverPointLike) => {
    val l = one.genes
      .zip(other.genes)
      .map { case (g1, g2) => shuffle(g1, g2) }
      .unzip

    Chromosome(l._1)
  }

  private def shuffle(g1: Gene, g2: Gene): (Gene, Gene) = {
    val b = Random.nextBoolean()
    if (b)
      (g1, g2)
    else
      (g2, g1)
  }

}
