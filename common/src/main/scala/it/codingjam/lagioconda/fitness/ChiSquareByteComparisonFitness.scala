package it.codingjam.lagioconda.fitness

import java.awt.image.{BufferedImage, DataBufferByte}

import it.codingjam.lagioconda.ImageDimensions
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.ga.Chromosome

class ChiSquareByteComparisonFitness(baseImage: BufferedImage, imageDimension: ImageDimensions) extends FitnessFunction {

  val baseImageAsBytes = baseImage.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

  override def fitness(chromosome: Chromosome): Double = {

    implicit val id = imageDimension
    val bi = new ChromosomeToBufferedImage(chromosome).toBufferedImage()(id)

    val imageInByte: Array[Byte] = bi.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

    var sum = 0.0
    var i = 0

    while (i < imageInByte.length) {

      val b1 = (baseImageAsBytes(i) + 128) / 255.0d
      val g1 = (baseImageAsBytes(i + 1) + 128) / 255.0d
      val r1 = (baseImageAsBytes(i + 2) + 128) / 255.0d

      val b2 = (imageInByte(i) + 128) / 255.0d
      val g2 = (imageInByte(i + 1) + 128) / 255.0d
      val r2 = (imageInByte(i + 2) + 128) / 255.0d

      require(b1 <= 1.0)
      require(g1 <= 1.0)
      require(r1 <= 1.0)
      require(b2 <= 1.0)
      require(g2 <= 1.0)
      require(r2 <= 1.0)

      val d = distance(r1, g1, b1, r2, g2, b2)

      sum = sum + d
      i = i + 3
    }

    val d = 1 - (sum / ((bi.getHeight * bi.getWidth).toDouble * 1.5))
    require(d >= 0)
    d
  }

  private def distance(c1r: Double, c1g: Double, c1b: Double, c2r: Double, c2g: Double, c2b: Double): Double = {

    val t1 = c1r + c2r
    val t2 = c1g + c2g
    val t3 = c1b + c2b
    val r1 = if (t1 != 0) ((c1r - c2r) * (c1r - c2r)) / t1 else 0.0
    val r2 = if (t2 != 0) ((c1b - c2b) * (c1b - c2b)) / t2 else 0.0
    val r3 = if (t3 != 0) ((c1g - c2g) * (c1g - c2g)) / t3 else 0.0

    (r1 + r2 + r3) * 0.5
  }

  private def chiSquareHistogram(histogram1: List[Double], histogram2: List[Double]): Double = {
    var r: Double = 0.0d
    var i = 0
    while (i < histogram1.length) {
      val t: Double = histogram1(i) + histogram2(i)
      if (t != 0) {
        r += ((histogram1(i) - histogram2(i)) * (histogram1(i) - histogram2(i))) / t
      }
      i += 1
    }
    0.5 * r
  }

}
