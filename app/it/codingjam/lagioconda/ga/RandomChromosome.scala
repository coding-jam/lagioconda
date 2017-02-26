package it.codingjam.lagioconda.ga

object RandomChromosome {

  def generate(): Chromosome =
    Chromosome(
      Range(0, Chromosome.numberOfGenes)
        .map(_ => RandomGene.generate())
        .toList)

}
