package it.codingjam.lagioconda.ga

import scala.util.Random

case class Chromosome(genes: List[Gene]) {

  require(genes.length == Chromosome.numberOfGenes)

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

  def onePointCrossover(other: Chromosome)(implicit crossover: CrossoverPointLike): (Chromosome, Chromosome) = {
    val r = Random.nextInt(Chromosome.numberOfGenes)
    val c1 = this.genes.splitAt(r)
    val c2 = other.genes.splitAt(r)
    val l1 = c1._1 ++ c2._2
    val l2 = c2._1 ++ c1._2
    (Chromosome(l1), Chromosome(l2))
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

}

object Chromosome {

  val numberOfGenes = 100

}
