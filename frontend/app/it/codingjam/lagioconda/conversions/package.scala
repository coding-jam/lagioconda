package it.codingjam.lagioconda

import java.awt.{Graphics2D, GraphicsEnvironment, RenderingHints}
import java.awt.geom.Ellipse2D
import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, IntBuffer}

import it.codingjam.lagioconda.domain._
import it.codingjam.lagioconda.ga.{Gene, _}

package object conversions {

  implicit class CircleToGene(circle: Circle) {

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
        to7bits(circle.radius),
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
      val radius = parse(gene.binaryString.substring(16, 23))
      val red = parse(gene.binaryString.substring(23, 31))
      val green = parse(gene.binaryString.substring(31, 39))
      val blue = parse(gene.binaryString.substring(39, 47))
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

}
