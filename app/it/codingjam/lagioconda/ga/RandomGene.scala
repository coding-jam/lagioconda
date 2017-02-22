package it.codingjam.lagioconda.ga

import scala.util.Random

object RandomGene {

  def generate(): Gene =
    Gene(
      Range(0, 60)
        .map(_ => Random.nextBoolean())
        .map(if (_) "1" else "0")
        .mkString(""))
}
