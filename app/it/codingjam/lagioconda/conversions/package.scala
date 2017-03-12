package it.codingjam.lagioconda

import java.awt.{Graphics2D, RenderingHints}
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

    private def to9bits(i: Int) = {
      require(i >= 0 && i < 512)
      "%09d".format(i.toBinaryString.toInt)
    }

    def toGene: Gene = {
      val list = List(
        to9bits(circle.center.x),
        to9bits(circle.center.y),
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
      val x = parse(gene.binaryString.substring(0, 9))
      val y = parse(gene.binaryString.substring(9, 18))
      val radius = parse(gene.binaryString.substring(18, 26))
      val red = parse(gene.binaryString.substring(26, 34))
      val green = parse(gene.binaryString.substring(34, 42))
      val blue = parse(gene.binaryString.substring(42, 50))
      val alpha = parse(gene.binaryString.substring(50, 58))
      Circle(Center(x, y), radius, Color(red, green, blue, alpha))
    }
  }

  implicit class ChromosomeToBufferedImage(chromosome: Chromosome) {

    def toBufferedImage()(implicit dimensions: ImageDimensions): BufferedImage = {
      val circles: List[Circle] = chromosome.genes.map(_.toCircle)

      val image = new BufferedImage(dimensions.width, dimensions.height, BufferedImage.TYPE_3BYTE_BGR);

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




  implicit class ChromosomeToMat(chromosome: Chromosome) {

    private[this] def withColorOf(circle: Circle)(f: Color => Unit) = {
      f(circle.color)
    }

    private[this] def zeroIfNeg(n: Int) = if (n < 0) 0 else n

    def toMat2()(implicit dimensions: ImageDimensions): Mat = {
      val circles: List[Circle] = chromosome.genes.map(_.toCircle)

      val mat = new Mat(dimensions.width, dimensions.height, CV_8UC3)
      val background = new Scalar(255, 255, 255, 0)

      mat.put(background)

      circles.foreach { c =>
        val overlay = new Mat(c.radius, c.radius, CV_8UC3)

        mat
          .adjustROI(zeroIfNeg(c.center.x - c.radius), zeroIfNeg(c.center.y - c.radius), zeroIfNeg(c.radius * 2), zeroIfNeg(c.radius * 2))
          .copyTo(overlay)

        withColorOf(c) { color =>
          val foreground = new Scalar(color.blue, color.green, color.red, 0)
          val alpha = color.alpha
          circle(overlay, new Point(c.radius, c.radius), c.radius, foreground, FILLED, CV_AA, 0)
          addWeighted(overlay, alpha, mat, 1 - alpha, 0, mat)
        }
      }
      mat
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
