package it.codingjam.lagioconda.backend

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, RenderingHints}

import it.codingjam.ga.converter
import it.codingjam.lagioconda.backend.domain._
import it.codingjam.lagioconda.ga.{Gene, _}

package object conversions {

  implicit class CircleToGene(circle: Circle) {

    private def to5bits(i: Int) = {
      require(i >= 0 && i < 32)
      "%05d".format(i.toBinaryString.toInt)
    }

    private def to6bits(i: Int) = {
      require(i >= 0 && i < 64)
      "%06d".format(i.toBinaryString.toInt)
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
        to5bits(circle.radius),
        to6bits(circle.color.red),
        to6bits(circle.color.green),
        to6bits(circle.color.blue)
      )
      Gene(list.mkString(""))
    }

  }

  implicit class GeneToCircle(gene: Gene) {

    def toCircle(implicit configuration: Configuration): Circle = {
      val c = converter.toComponents(gene)
      Circle(Center(c._1, c._2), c._3, Color(c._4, c._5, c._6, c._7))
    }
  }

  implicit class ChromosomeToBufferedImage(chromosome: Chromosome)(implicit configuration: Configuration) {

    def toBufferedImage()(implicit dimensions: ImageDimensions): BufferedImage = {
      val circles: List[Circle] = chromosome.genes.map(_.toCircle)

      val image = new BufferedImage(dimensions.width, dimensions.height, BufferedImage.TYPE_3BYTE_BGR);

      val g2: Graphics2D = image.createGraphics()

      val qualityHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
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
