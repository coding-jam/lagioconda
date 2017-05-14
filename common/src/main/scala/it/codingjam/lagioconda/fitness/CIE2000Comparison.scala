package it.codingjam.lagioconda.fitness

import java.awt.image.BufferedImage

import it.codingjam.lagioconda.ImageDimensions
import it.codingjam.lagioconda.ga.Chromosome
import org.openimaj.image.analysis.colour.CIEDE2000
import org.openimaj.image.{FImage, ImageUtilities}
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import org.openimaj.image.colour.ColourSpace

class CIE2000Comparison(baseImage: BufferedImage, imageDimension: ImageDimensions) extends FitnessFunction {

  private[this] val baseMBF = ImageUtilities.createMBFImage(baseImage, false)
  private[this] val base =
    if (baseMBF.colourSpace ne ColourSpace.CIE_Lab)
      ColourSpace.convert(baseMBF, ColourSpace.CIE_Lab)
    else
      baseMBF

  override def fitness(chromosome: Chromosome): Double = {
    implicit val id = imageDimension
    val bi = new ChromosomeToBufferedImage(chromosome).toBufferedImage()(id)

    val test = ImageUtilities.createMBFImage(bi, false)

    val disparity: FImage = CIEDE2000.makeDisparityMap(base, test)
    val f = disparity.pixels.flatten

    val d: Float = 1 - (f.sum / (base.getWidth * base.getHeight * 100))
    d.toDouble

  }

}
