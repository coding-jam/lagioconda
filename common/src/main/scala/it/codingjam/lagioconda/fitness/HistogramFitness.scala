package it.codingjam.lagioconda.fitness

import java.awt.image.{BufferedImage, DataBufferByte}

import it.codingjam.lagioconda.ImageDimensions
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.ga.Chromosome

case class ImageHistogram(red: Array[Double], blue: Array[Double], green: Array[Double]) {

  private def chiSquareHistogram(histogram1: Array[Double], histogram2: Array[Double]): Double = {
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

  def chiSquare(other: ImageHistogram): Double = {
    val d = (chiSquareHistogram(red, other.red) + chiSquareHistogram(blue, other.blue) + chiSquareHistogram(green, other.green)) / 3.0
    d
  }

}

class HistogramFitness(baseImage: BufferedImage, imageDimension: ImageDimensions) extends FitnessFunction {

  val baseHistogram = histogram(baseImage, imageDimension)

  override def fitness(chromosome: Chromosome): Double = {
    println("calculate fitness")

    implicit val id = imageDimension
    val bi = new ChromosomeToBufferedImage(chromosome).toBufferedImage()(id)

    val testHistogram = histogram(bi, id)
    println("calculate fitness ended")
    1.0d - baseHistogram.chiSquare(testHistogram)

  }

  def histogram(bufferedImage: BufferedImage, imageDimensions: ImageDimensions) = {
    implicit val id = imageDimension

    val count: Double = bufferedImage.getWidth * bufferedImage.getHeight

    val imageInByte: Array[Byte] = bufferedImage.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
    var i = 0

    val r = Array.fill(32)(0)
    val g = Array.fill(32)(0)
    val b = Array.fill(32)(0)

    while (i < imageInByte.length) {

      val b2 = imageInByte(i) + 128
      val g2 = imageInByte(i + 1) + 128
      val r2 = imageInByte(i + 2) + 128

      r(r2 / 8) = r(r2 / 8) + 1
      g(g2 / 8) = g(g2 / 8) + 1
      b(b2 / 8) = b(b2 / 8) + 1

      i = i + 3
    }

    val rd = r.map(_ / count)
    val gd = r.map(_ / count)
    val bd = r.map(_ / count)

    ImageHistogram(rd, gd, bd)
  }

}
