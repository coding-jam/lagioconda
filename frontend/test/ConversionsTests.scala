package it.codingjam.lagioconda.domain

import it.codingjam.lagioconda.{Center, Circle, Color}
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.ga.Gene
import org.scalatest.{MustMatchers, WordSpecLike}

class ConversionsTests extends WordSpecLike with MustMatchers {

  //helper functions
  def fold(gene: Gene, times: Int): Gene = {
    if (times <= 0) gene
    else
      fold(gene.fold, times - 1)
  }

  def unfold(gene: Gene, times: Int): Gene = {
    if (times <= 0) gene
    else
      unfold(gene.unfold, times - 1)
  }

  "Conversion" should {

    def toBinary(s: String): String = {
      s.toCharArray.map(_.toInt).map(_ % 2).map(_.toString).mkString("")
    }


    val sampleCircle = Circle(Center(100, 300), 10, Color(120, 10, 30, 150))

    val binarySequence = "00110010010010110000001010011110000000101000011110"
    implicit val length = binarySequence.length

    val sampleGene = Gene(binarySequence)

    "convert a Circle to a Gene" in {
      val gene: Gene = sampleCircle.toGene
      gene mustBe (sampleGene)
    }

    "convert a Gene to a Circle" in {
      val circle = sampleGene.toCircle

      circle mustBe sampleCircle

    }

    "to binary" in {
      toBinary("aaabcd1") mustBe ("1110101")
    }

    "fold a simple gene" in {
      val binaryString = "0000000011111111"
      val gene = Gene(binaryString)

      val folded = gene.fold

      folded mustBe (Gene("0101010101010101"))
    }

    "unfold simple folded gene" in {
      val folded = Gene("0101010101010101")
      val unfolded = folded.unfold
      unfolded mustBe (Gene("0000000011111111"))
    }

    "fold a gene" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)

      val folded = gene.fold

      folded mustBe (Gene(toBinary("1H2G3F4E5D6C7B8A")))
    }

    "fold a gene twice" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)

      val folded = gene.fold(2)

      folded mustBe (Gene(toBinary("1AH82BG73CF64DE5")))
    }

    "unfold a gene folded twice" in {
      val folded = Gene(toBinary("1AH82BG73CF64DE5"))
      val unfolded = folded.unfold(2)

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene three times" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)

      val folded = gene.fold(3)

      folded mustBe (Gene(toBinary("15AEHD8426BFGC73")))
    }

    "unfold a gene folded three times" in {
      val folded = Gene(toBinary("15AEHD8426BFGC73"))
      val unfolded = folded.unfold(3)

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene four times" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)

      val folded = gene.fold(4)

      folded mustBe (Gene(toBinary("1357ACEGHFDB8642")))
    }

    "unfold a gene folded four times" in {
      val folded = Gene(toBinary("1357ACEGHFDB8642"))
      val unfolded = folded.unfold(4)

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene five times" in {
      val binaryString = toBinary("1234567890abcdABCDEFGHILMNOP")
      val gene = Gene(binaryString)

      val folded = gene.fold(5)

      folded mustBe (Gene(toBinary("1cMF86DOA3aIH04BPC59GLb2dNE7")))
    }

    "unfold a gene five times" in {
      val folded = Gene(toBinary("1cMF86DOA3aIH04BPC59GLb2dNE7"))
      val unfolded = folded.unfold(5)

      unfolded mustBe Gene(toBinary("1234567890abcdABCDEFGHILMNOP"))
    }

  }
}
