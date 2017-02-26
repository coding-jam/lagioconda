package it.codingjam.lagioconda

import java.awt.{RenderingHints}
import java.awt.geom.Ellipse2D
import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, IntBuffer}

import it.codingjam.lagioconda.domain.{Center, Circle, Color, ImageDimensions}
import it.codingjam.lagioconda.ga.{Gene, _}
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc._

package object conversions {

  implicit class CircleToGene(circle: Circle) {

    private def to8bits(i: Int) = {
      require(i >= 0 && i < 256)
      "%08d".format(i.toBinaryString.toInt)
    }

    private def to10bits(i: Int) = {
      require(i >= 0 && i < 1024)
      "%010d".format(i.toBinaryString.toInt)
    }

    def toGene: Gene = {
      val list = List(
        to10bits(circle.center.x),
        to10bits(circle.center.y),
        to8bits(circle.radius),
        to8bits(circle.color.red),
        to8bits(circle.color.green),
        to8bits(circle.color.blue),
        to8bits(circle.color.alpha)
      )
      Gene(list.mkString(""))
    }

  }

  implicit class GeneToCircle(gene: Gene) {

    private def parse(s: String) = Integer.parseInt(s, 2).toInt

    def toCircle: Circle = {
      val x = parse(gene.binaryString.substring(0, 10))
      val y = parse(gene.binaryString.substring(10, 20))
      val radius = parse(gene.binaryString.substring(20, 28))
      val red = parse(gene.binaryString.substring(28, 36))
      val green = parse(gene.binaryString.substring(36, 44))
      val blue = parse(gene.binaryString.substring(44, 52))
      val alpha = parse(gene.binaryString.substring(52, 60))
      Circle(Center(x, y), radius, Color(red, green, blue, alpha))
    }
  }

  implicit class ChromosomeToBufferedImage(chromosome: Chromosome) {

    def toBufferedImage()(
        implicit dimensions: ImageDimensions): BufferedImage = {
      val circles: List[Circle] = chromosome.genes.map(_.toCircle)

      val image = new BufferedImage(dimensions.width,
                                    dimensions.height,
                                    BufferedImage.TYPE_INT_ARGB);

      val g2 = image.createGraphics()

      val qualityHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                            RenderingHints.VALUE_ANTIALIAS_ON)
      qualityHints.put(RenderingHints.KEY_RENDERING,
                       RenderingHints.VALUE_RENDER_QUALITY)
      g2.setRenderingHints(qualityHints)

      circles.foreach { circle =>
        val transparent = new java.awt.Color(circle.color.red,
                                             circle.color.blue,
                                             circle.color.green,
                                             circle.color.alpha)
        g2.setColor(transparent)
        g2.fill(
          new Ellipse2D.Float(circle.center.x - circle.radius,
                              circle.center.y - circle.radius,
                              circle.center.x + circle.radius,
                              circle.center.y + circle.radius))
      }

      g2.dispose()
      image.flush()

      image
    }

  }

  implicit class BufferedImageToMat(bi: BufferedImage) {
    def toMat: Mat = {
      val raster = bi.getRaster()

      val dataBuffer = raster.getDataBuffer()

      val pixelData: Array[Int] =
        dataBuffer.asInstanceOf[DataBufferInt].getData
      val byteBuffer = ByteBuffer.allocate(pixelData.length * 4)
      byteBuffer.asIntBuffer().put(IntBuffer.wrap(pixelData))

      val width = bi.getWidth()
      val height = bi.getHeight()

      val colors = bi.getColorModel().getNumComponents()
      val mat = new Mat(byteBuffer.array(), false).reshape(colors, width)

      val mat3c = new Mat(width, height, CV_8UC3)
      cvtColor(mat, mat3c, COLOR_BGRA2BGR)

      mat3c
    }

  }

}
