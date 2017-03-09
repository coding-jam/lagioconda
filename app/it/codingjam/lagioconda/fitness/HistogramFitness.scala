package it.codingjam.lagioconda.fitness

import it.codingjam.lagioconda.ga.Chromosome
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc._
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions

import scala.util.Random

class HistogramFitness(val baseImage: Mat)(implicit dimensions: ImageDimensions) extends FitnessFunction {

  private val histogram = new ColorHistogram()
  //TODO: put this number in configuration
  histogram.numberOfBins = 50

  private val baseHistogram = histogram.getHistogram(baseImage)

  def fitness(chromosome: Chromosome): Double = {

    val image = chromosome.toBufferedImage().toMat

    val inputH: Mat = histogram.getHistogram(image)
    val v = compareHist(baseHistogram, inputH, HISTCMP_INTERSECT)
    v / (image.cols() * image.rows())
  }
}
