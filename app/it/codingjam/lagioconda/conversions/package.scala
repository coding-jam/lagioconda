package it.codingjam.lagioconda

import java.awt.{Graphics2D, GraphicsEnvironment, RenderingHints}
import java.awt.geom.Ellipse2D
import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, IntBuffer}

import it.codingjam.lagioconda.domain._
import it.codingjam.lagioconda.ga.{Gene, _}
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc._

package object conversions {

  implicit class CircleToGene(circle: Circle) {

    private def to6bits(i: Int) = {
      require(i >= 0 && i < 64)
      "%07d".format(i.toBinaryString.toInt)
    }

    private def to7bits(i: Int) = {
      require(i >= 0 && i < 128)
      "%07d".format(i.toBinaryString.toInt)
    }

    private def to8bits(i: Int) = {
      require(i >= 0 && i < 256)
      "%08d".format(i.toBinaryString.toInt)
    }

    private def to10bits(i: Int) = {
      require(i >= 0 && i < 1024)
      "%010d".format(i.toBinaryString.toInt)
    }

    private def to9bits(i: Int) = {
      require(i >= 0 && i < 512)
      "%09d".format(i.toBinaryString.toInt)
    }

    def toGene(implicit configuration: Configuration): Gene = {
      val list = List(
        to8bits(circle.center.x),
        to8bits(circle.center.y),
        to6bits(circle.radius),
        to8bits(circle.color.red),
        to8bits(circle.color.green),
        to8bits(circle.color.blue)
      )
      Gene(list.mkString(""))
    }

  }

  implicit class GeneToCircle(gene: Gene) {

    private def parse(s: String) = Integer.parseInt(s, 2).toInt

    def toCircle(implicit configuration: Configuration): Circle = {
      val x = parse(gene.binaryString.substring(0, 8))
      val y = parse(gene.binaryString.substring(8, 16))
      val radius = parse(gene.binaryString.substring(16, 22))
      val red = parse(gene.binaryString.substring(22, 30))
      val green = parse(gene.binaryString.substring(30, 38))
      val blue = parse(gene.binaryString.substring(38, 46))
      Circle(Center(x, y), radius, Color(red, green, blue, configuration.alpha))
    }
  }

  implicit class ChromosomeToBufferedImage(chromosome: Chromosome)(implicit configuration: Configuration) {

    def toBufferedImage()(implicit dimensions: ImageDimensions): BufferedImage = {
      val circles: List[Circle] = chromosome.genes.map(_.toCircle)

      val image = new BufferedImage(dimensions.width, dimensions.height, BufferedImage.TYPE_3BYTE_BGR);

      /*
      val ge =
        GraphicsEnvironment.getLocalGraphicsEnvironment();
      val gd = ge.getDefaultScreenDevice();
      val gc = gd.getDefaultConfiguration();
      val image = gc.createCompatibleVolatileImage(16, 16);
      image.validate(gc);
       */

      val g2: Graphics2D = image.createGraphics()

      val qualityHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      qualityHints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED)
      g2.setRenderingHints(qualityHints)

      circles.foreach { circle =>
        val transparent = new java.awt.Color(circle.color.red, circle.color.blue, circle.color.green, circle.color.alpha)
        g2.setColor(transparent)
        g2.fill(
          new Ellipse2D.Float(circle.center.x - circle.radius, circle.center.y - circle.radius, circle.radius * 2, circle.radius * 2))
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
