package it.codingjam.lagioconda.ga

import org.scalatest.{MustMatchers, WordSpecLike}

class FixedCrossoverPoint(fixedPoint: Int) extends CrossoverPointLike {
  override def crossoverPoint(int: Int): Int = fixedPoint
}

class GeneTests extends WordSpecLike with MustMatchers {

  "Gene" should {

    val binaryString1 = "00011001000100101100000000101001111000000010100001111001100100"
    val sampleGene1 = Gene(binaryString1)
    val binaryString2 = "01001000011000001110111010011001110111000011001011011111001000"
    val sampleGene2 = Gene(binaryString2)

    "crossover with fixed crossover point" in {
      implicit val cp = new FixedCrossoverPoint(10)

      val newGenes = sampleGene1.crossover(sampleGene2)

      newGenes._1.binaryString.substring(0, 10) mustBe sampleGene1.binaryString
        .subSequence(0, 10)
      newGenes._1.binaryString.substring(10) mustBe sampleGene2.binaryString
        .substring(10)

      newGenes._2.binaryString.substring(0, 10) mustBe sampleGene2.binaryString
        .subSequence(0, 10)
      newGenes._2.binaryString.substring(10) mustBe sampleGene1.binaryString
        .substring(10)

    }
  }

}
