package it.codingjam.lagioconda.ga

import scala.util.Random

class RandomMutationPoint extends MutationPointLike {
  override def mutationPoint(int: Int): Int = Random.nextInt(int)
}

class FixedGeneMutationPoint(fixed: Int) extends MutationPointLike {
  override def mutationPoint(int: Int): Int = fixed
}
