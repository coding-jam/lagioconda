package it.codingjam.lagioconda.ga

object RandomChromosome {

  def generate(length: Int, genes: Int): Chromosome =
    Chromosome(
      Range(0, genes)
        .map(_ => RandomGene.generate(length))
        .toList)

}
