package it.codingjam.lagioconda.ga

import scala.util.Random

case class Chromosome(genes: List[Gene]) {

  def mutate(implicit mutationPoint: MutationPointLike, mutationSize: Int): Chromosome = {
    val position = Random.nextInt(genes.size)
    Chromosome(
      (genes.slice(0, position) :+ genes(position).mutation(mutationSize)) ++ genes
        .slice(position + 1, genes.length))
  }

  def mutate(times: Int)(implicit mutationPoint: MutationPointLike, mutationSize: Int): Chromosome = {

    def mut(c: Chromosome) = c.mutate

    val f = Function.chain(List.fill(times)(mut(_)))
    f(this)
  }

  def genesCrossover(other: Chromosome)(implicit crossover: CrossoverPointLike): (Chromosome, Chromosome) = {
    val l: (List[Gene], List[Gene]) = genes
      .zip(other.genes)
      .map {
        case (g1, g2) => g1.crossover(g2)
      }
      .unzip

    (Chromosome(l._1), Chromosome(l._2))
  }

  def fullCrossover(other: Chromosome)(implicit crossover: CrossoverPointLike): (Chromosome, Chromosome) = {
    val l = genes
      .zip(other.genes)
      .map { case (g1, g2) => shuffle(g1, g2) }
      .unzip

    (Chromosome(l._1), Chromosome(l._2))
  }

  def uniformCrossover(other: Chromosome)(implicit crossover: CrossoverPointLike): Chromosome = {
    val l = genes
      .zip(other.genes)
      .map { case (g1, g2) => shuffle(g1, g2) }
      .unzip

    Chromosome(l._1)
  }

  def neighbour(gene: Int, position: Int)(implicit mutationPointLike: MutationPointLike) = {
    Chromosome((genes.slice(0, gene) :+ genes(gene).neighbour(position)) ++ genes.slice(gene + 1, genes.length))
  }

  private def shuffle(g1: Gene, g2: Gene): (Gene, Gene) = {
    val b = Random.nextBoolean()
    if (b)
      (g1, g2)
    else
      (g2, g1)
  }

  def addRandomGene() = {
    this.copy(genes = this.genes :+ RandomGene.generate(Gene.Size))
  }

}

object Chromosome {

  val numberOfGenes = 3

}
