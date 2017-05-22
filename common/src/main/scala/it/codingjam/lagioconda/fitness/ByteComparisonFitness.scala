package it.codingjam.lagioconda.fitness

import java.awt.image.{BufferedImage, DataBufferByte}

import it.codingjam.lagioconda.{GeneMapping, ImageDimensions}
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.ga.Chromosome

class ByteComparisonFitness(baseImage: BufferedImage, imageDimension: ImageDimensions) extends FitnessFunction {

  val baseImageAsBytes = baseImage.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

  override def fitness(chromosome: Chromosome, alpha: Int): Double = {

    implicit val id = imageDimension
    val bi = new ChromosomeToBufferedImage(chromosome).toBufferedImage(alpha)(id)

    val imageInByte: Array[Byte] = bi.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

    var sum = 0.0
    var i = 0

    while (i < imageInByte.length) {

      val b1 = baseImageAsBytes(i) + 128
      val g1 = baseImageAsBytes(i + 1) + 128
      val r1 = baseImageAsBytes(i + 2) + 128

      val b2 = imageInByte(i) + 128
      val g2 = imageInByte(i + 1) + 128
      val r2 = imageInByte(i + 2) + 128

      val d = distance(r1, g1, b1, r2, g2, b2) / (255.0 * 3)

      sum = sum + d
      i = i + 3
    }

    val d = 1 - (sum * 3 / (imageInByte.length.toDouble))
    require(d > 0)
    d
  }

  def distance(c1r: Int, c1g: Int, c1b: Int, c2r: Int, c2g: Int, c2b: Int) = {
    val r = (c1r + c2r) / 2
    Math.sqrt(
      ((2.0 + r / 255.0) * (c1r - c2r) * (c1r - c2r)) + (4 * (c1g - c2g) * (c1g - c2g)) + ((2.0 + (255.0 - r) / 255.0)) * (c1b - c2b) * (c1b - c2b))
  }
}
