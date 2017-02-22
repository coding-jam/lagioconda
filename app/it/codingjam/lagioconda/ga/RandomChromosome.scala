package it.codingjam.lagioconda.ga

object RandomChromosome {

  def generate(): Chromosome =
    Chromosome(Range(0, 150).map(_ => RandomGene.generate()).toList)

}
