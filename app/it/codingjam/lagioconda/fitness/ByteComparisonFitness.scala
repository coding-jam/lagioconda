package it.codingjam.lagioconda.fitness

import java.awt.image.{DataBufferByte, DataBufferInt}

import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.ga.Chromosome

class ByteComparisonFitness(val baseImage: Array[Byte])(implicit dimension: ImageDimensions) extends FitnessFunction {

  override def fitness(chromosome: Chromosome): Double = {
    val bi = chromosome.toBufferedImage

    val imageInByte: Array[Byte] = bi.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

    var sum = 0
    0.to(imageInByte.length - 1).foreach { i =>
      if (baseImage(i) > imageInByte(i))
        sum += (baseImage(i) - imageInByte(i))
      else
        sum += (imageInByte(i) - baseImage(i))
    }
    1 - (sum.toDouble / (baseImage.length * 256))
  }
}
