package it.codingjam.lagioconda.ga

import it.codingjam.lagioconda.GeneMapping

object RandomChromosome {

  def generate(length: Int, genes: Int, geneMapping: GeneMapping): Chromosome =
    Chromosome(Range(0, genes)
                 .map(_ => RandomGene.generate(length))
                 .toList,
               geneMapping)

}
