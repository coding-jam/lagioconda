package it.codingjam.lagioconda.ga

import it.codingjam.lagioconda.domain.Configuration

object RandomChromosome {

  def generate()(implicit configuration: Configuration): Chromosome =
    Chromosome(
      Range(0, Chromosome.numberOfGenes)
        .map(_ => RandomGene.generate())
        .toList)

}
