package it.codingjam.lagioconda.domain

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.ga.Gene
import org.scalatest.{MustMatchers, WordSpecLike}

class ConversionsTests extends WordSpecLike with MustMatchers {

  "Conversion" should {

    def toBinary(s: String): String = {
      s.toCharArray.map(_.toInt).map(_ % 2).map(_.toString).mkString("")
    }

    val sampleCircle = Circle(Center(100, 300), 10, Color(120, 10, 30, 100))

    val binarySequence = "0011001001001011000000101001111000000010100001111001100100"
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
      val gene = Gene(binaryString)(binaryString.length)

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
      val gene = Gene(binaryString)(binaryString.length)

      val folded = gene.fold

      folded mustBe (Gene(toBinary("1H2G3F4E5D6C7B8A")))
    }

    "fold a gene twice" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)(binaryString.length)

      val folded = gene.fold.fold

      folded mustBe (Gene(toBinary("1AH82BG73CF64DE5")))
    }

    "unfold a gene folded twice" in {
      val folded = Gene(toBinary("1AH82BG73CF64DE5"))
      val unfolded = folded.unfold.unfold

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene three times" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)(binaryString.length)

      val folded = gene.fold.fold.fold

      folded mustBe (Gene(toBinary("15AEHD8426BFGC73")))
    }

    "unfold a gene folded three times" in {
      val folded = Gene(toBinary("15AEHD8426BFGC73"))
      val unfolded = folded.unfold.unfold.unfold

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene four times" in {
      val binaryString = toBinary("12345678ABCDEFGH")
      val gene = Gene(binaryString)(binaryString.length)

      val folded = gene.fold.fold.fold.fold

      folded mustBe (Gene(toBinary("1357ACEGHFDB8642")))
    }

    "unfold a gene folded four times" in {
      val folded = Gene(toBinary("1357ACEGHFDB8642"))
      val unfolded = folded.unfold.unfold.unfold

      unfolded mustBe Gene(toBinary("12345678ABCDEFGH"))
    }

    "fold a gene five times" in {
      val binaryString = toBinary("1234567890abcdABCDEFGHILMNOP")
      val gene = Gene(binaryString)(binaryString.length)

      val folded = gene.fold.fold.fold.fold.fold

      folded mustBe (Gene(toBinary("1cMF86DOA3aIH04BPC59GLb2dNE7")))
    }

  }
}
