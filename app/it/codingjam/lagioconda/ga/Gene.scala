package it.codingjam.lagioconda.ga

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

}

object Gene {
  val Size = 58
}