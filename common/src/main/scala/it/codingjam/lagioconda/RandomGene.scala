package it.codingjam.lagioconda.ga

import scala.util.Random

object RandomGene {

  def generate(length: Int): Gene =
    Gene(
      Range(0, length)
        .map(_ => Random.nextBoolean())
        .map(if (_) "1" else "0")
        .mkString(""))
}
