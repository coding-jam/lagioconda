package it.codingjam.lagioconda.fitness

import java.awt.image.{BufferedImage, DataBufferByte}

import it.codingjam.lagioconda.ImageDimensions
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.ga.Chromosome
import org.openimaj.image.ImageUtilities
import org.openimaj.image.analysis.colour.CIEDE2000
import org.openimaj.image.colour.ColourSpace

class CIE2000Comparison(baseImage: BufferedImage, imageDimension: ImageDimensions) extends FitnessFunction {

  private[this] val baseMBF = ImageUtilities.createMBFImage(baseImage, false)
  private[this] val base =
    if (baseMBF.colourSpace ne ColourSpace.CIE_Lab)
      ColourSpace.convert(baseMBF, ColourSpace.CIE_Lab)
    else
      baseMBF

  var cache: Map[Rgb, Lab] = Map[Rgb, Lab]()
  var cacheHit = 0
  var lastSize = 0

  override def fitness(chromosome: Chromosome): Double = {

    implicit val id = imageDimension
    val bi = new ChromosomeToBufferedImage(chromosome).toBufferedImage()(id)
    val biInInt = bi.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

    var sum = 0.0d
    var i = 0
    while (i < biInInt.length) {
      // bytes are in BGR
      val testLab: Lab = rgb2lab((biInInt(i + 2) + 128) / 255.0f, (biInInt(i + 1) + 128) / 255.0f, (biInInt(i + 1) + 128) / 255.0f)

      val j = i / 3
      val x = j / id.width
      val y = i % id.width

      val baseLab = base.getPixel(x, y).map(_.toFloat)

      val delta = CIEDE2000.calculateDeltaE(baseLab(0), baseLab(1), baseLab(2), testLab.l, testLab.a, testLab.b)
      require(delta >= 0)

      sum = sum + delta
      i = i + 3
    }
    val fitness = 1.0d - (sum / (base.getWidth * base.getHeight * 100.0d).toDouble)
    require(fitness > 0)
    fitness
  }

  def rgb2lab(r: Float, g: Float, b: Float): Lab = {
    val key = Rgb(r, g, b)
    if (cache.isDefinedAt(key)) {
      cache(key)
    } else {
      val xyz: Xyz = Rgb(r, g, b).toXyz
      val value = xyz.toLab
      cache = cache + (key -> value)
      value
    }
  }

}
