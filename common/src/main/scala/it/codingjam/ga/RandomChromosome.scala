package it.codingjam.lagioconda.ga

object RandomChromosome {

  def generate(length: Int): Chromosome =
    Chromosome(
      Range(0, Chromosome.numberOfGenes)
        .map(_ => RandomGene.generate(length))
        .toList)

}
