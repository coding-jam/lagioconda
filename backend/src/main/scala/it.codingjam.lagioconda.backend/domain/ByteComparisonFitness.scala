package it.codingjam.lagioconda.backend.domain

import java.awt.image.DataBufferByte

import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.Chromosome
import it.codingjam.lagioconda.backend.conversions.ChromosomeToBufferedImage

class ByteComparisonFitness(val baseImage: Array[Byte])(implicit dimension: ImageDimensions, configuration: Configuration)
    extends FitnessFunction {

  override def fitness(chromosome: Chromosome): Double = {
    val bi = chromosome.toBufferedImage

    val imageInByte: Array[Byte] = bi.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

    var sum = 0
    var i = 0

    while (i < imageInByte.length) {
      if (baseImage(i) > imageInByte(i))
        sum += (baseImage(i) - imageInByte(i))
      else
        sum += (imageInByte(i) - baseImage(i))
      i = i + 1
    }

    1 - (sum.toDouble / (baseImage.length * 256))
  }
}
