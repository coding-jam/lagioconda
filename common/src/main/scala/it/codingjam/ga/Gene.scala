package it.codingjam.lagioconda.ga

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

  def neighbour(position: Int): Gene = {
    val n = binaryString.toCharArray.zipWithIndex
      .map { i =>
        if (position == i._2 || position + 1 == i._2) flip(i._1) else (i._1).toString
      }
      .mkString("")
    Gene(n)
  }

  def neighbour(position: Int, newChunck: String): Gene = {
    val s = binaryString.splitAt(position)
    val left = s._1
    val right = s._2.drop(newChunck.size)
    val ss = (left + newChunck + right).take(Gene.Size)
    require(ss.length == Gene.Size, s"binary $binaryString, position $position, chunk $newChunck, left $left, right $right")
    Gene(ss)
  }

  def mutation(bitsToMutate: Int)(implicit mutationPoint: MutationPointLike): Gene = {
    val mp = mutationPoint.mutationPoint(binaryString.length)

    val g = this.fold(5)
    val range = Range(mp, mp + bitsToMutate)
    val mutated = g.binaryString.toCharArray.zipWithIndex
      .map { i =>
        if (range.contains(i._2)) flip(i._1) else (i._1).toString
      }
      .mkString("")

    Gene(mutated).unfold(5)
  }

  def fold: Gene = {
    val partition = binaryString.splitAt(binaryString.length / 2)
    val p: Seq[Char] = partition._1
    val q: Seq[Char] = partition._2.reverse
    val l = p.zipAll(q, "", "").flatMap(_.productIterator.toList).filter(_ != "").mkString("")
    Gene(l)
  }

  def fold(times: Int): Gene = if (times <= 0) this else fold.fold(times - 1)

  def unfold(times: Int): Gene = if (times <= 0) this else unfold.unfold(times - 1)

  def unfold: Gene = {
    val p = binaryString.zipWithIndex.partition(_._2 % 2 == 0)
    val p1 = p._1.map(_._1)
    val p2 = p._2.map(_._1).reverse
    val string = (p1 ++ p2).mkString("")
    Gene(string)
  }

}

object Gene {
  var Size = 37
}
