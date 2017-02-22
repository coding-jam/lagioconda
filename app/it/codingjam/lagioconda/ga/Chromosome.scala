package it.codingjam.lagioconda.ga

import scala.util.Random

case class Chromosome(genes: List[Gene]) {
  require(genes.length == 150)

  def mutate: Chromosome = {
    val position = Random.nextInt(genes.size)
    Chromosome(
      (genes.slice(0, position) :+ genes(position)) ++ genes
        .slice(position + 1, genes.length))
  }

  def crossover(other: Chromosome)(
      implicit crossover: CrossoverPointLike): (Chromosome, Chromosome) = {
    val l: (List[Gene], List[Gene]) = genes
      .zip(other.genes)
      .map {
        case (g1, g2) => g1.crossover(g2)
      }
      .unzip

    (Chromosome(l._1), Chromosome(l._2))
  }

}
