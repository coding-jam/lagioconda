package it.codingjam.lagioconda.ga

import scala.util.Random

case class Chromosome(genes: List[Gene]) {

  private def mutate(implicit mutationPoint: MutationPointLike, mutationSize: Int): Chromosome = {
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

  private def neighbour(gene: Int, position: Int)(implicit mutationPointLike: MutationPointLike) = {
    Chromosome((genes.slice(0, gene) :+ genes(gene).neighbour(position)) ++ genes.slice(gene + 1, genes.length))
  }

  def addRandomGene() = {
    this.copy(genes = this.genes :+ RandomGene.generate(Gene.Size))
  }

}
