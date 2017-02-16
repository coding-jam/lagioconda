package it.codingjam.lagioconda.fitness

import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc._

class HistogramFitness(val baseImage: Mat) extends FitnessFunction {

  private val histogram = new ColorHistogram()
  //TODO: put this number in configuration
  histogram.numberOfBins = 10

  private val baseHistogram = histogram.getHistogram(baseImage)

  def fitness(image: Mat): Double = {
    val inputH = histogram.getHistogram(image)
    compareHist(baseHistogram, inputH, HISTCMP_INTERSECT)
  }
}
