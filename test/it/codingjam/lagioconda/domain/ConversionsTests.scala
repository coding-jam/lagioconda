package it.codingjam.lagioconda.domain

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.ga.Gene
import org.scalatest.{MustMatchers, WordSpecLike}

class ConversionsTests extends WordSpecLike with MustMatchers {

  "Conversion" should {

    val sampleCircle = Circle(Center(100, 300), 10, Color(120, 10, 30, 100))
    val sampleGene =
      Gene("00011001000100101100000000101001111000000010100001111001100100")

    "convert a Circle to a Gene" in {
      val gene: Gene = sampleCircle.toGene
      gene mustBe (sampleGene)
    }

    "convert a Gene to a Circle" in {
      val circle = sampleGene.toCircle

      circle mustBe sampleCircle

    }

  }
}
