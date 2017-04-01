package it.codingjam.lagioconda.ga

import it.codingjam.lagioconda.domain.Configuration

import scala.collection.immutable.Seq

case class Gene(binaryString: String) {

  require(binaryString.split("").forall(s => s.equals("1") || s.equals("0")))

  private def flip(s: Char) = if (s == '1') "0" else "1"

  def crossover(other: Gene)(implicit crossover: CrossoverPointLike): (Gene, Gene) = {
    require(binaryString.length == other.binaryString.length)
    val cp = crossover.crossoverPoint(binaryString.length)
    val newGene1 = binaryString.substring(0, cp) + other.binaryString
        .substring(cp)
    val newGene2 = other.binaryString.substring(0, cp) + binaryString
        .substring(cp)
    (Gene(newGene1), Gene(newGene2))
  }

  def mutation(implicit mutationPoint: MutationPointLike): Gene = {
    val mp = mutationPoint.mutationPoint(binaryString.length)
    Gene(
      binaryString.substring(0, mp) + flip(binaryString(mp)) + binaryString
        .substring(mp + 1))
  }

  def fold: Gene = {
    val partition = binaryString.splitAt(binaryString.length / 2)
    val p: Seq[Char] = partition._1
    val q: Seq[Char] = partition._2.reverse
    val l = p.zipAll(q, "", "").flatMap(_.productIterator.toList).filter(_ != "").mkString("")
    Gene(l)
  }

  def unfold: Gene = {
    val p = binaryString.zipWithIndex.partition(_._2 % 2 == 0)
    val p1 = p._1.map(_._1)
    val p2 = p._2.map(_._1).reverse
    val string = (p1 ++ p2).mkString("")
    Gene(string)
  }

}
