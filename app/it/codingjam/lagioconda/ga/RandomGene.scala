package it.codingjam.lagioconda.ga

import it.codingjam.lagioconda.domain.Configuration

import scala.util.Random

object RandomGene {

  def generate()(implicit configuration: Configuration): Gene =
    Gene(
      Range(0, configuration.length)
        .map(_ => Random.nextBoolean())
        .map(if (_) "1" else "0")
        .mkString(""))
}
