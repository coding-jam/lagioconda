package it.codingjam.lagioconda.ga

import scala.util.Random

class RandomCrossoverPoint extends CrossoverPointLike {

  override def crossoverPoint(int: Int): Int = Random.nextInt(int)
}
