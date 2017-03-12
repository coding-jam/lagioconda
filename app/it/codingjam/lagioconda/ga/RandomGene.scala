package it.codingjam.lagioconda.ga

import scala.util.Random
import it.codingjam.lagioconda.ga.Gene

object RandomGene {

  def generate(): Gene =
    Gene(
      Range(0, Gene.Size)
        .map(_ => Random.nextBoolean())
        .map(if (_) "1" else "0")
        .mkString(""))
}
