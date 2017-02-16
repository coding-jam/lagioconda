package it.codingjam.lagioconda.fitness

import org.bytedeco.javacpp.opencv_core.Mat

trait FitnessFunction {

  def fitness(image: Mat): Double
}
