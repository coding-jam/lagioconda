package it.codingjam.lagioconda

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, RenderingHints}

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

    def toGene: Gene = {
      val list = List(
        to8bits(circle.center.x),
        to8bits(circle.center.y),
        to5bits(circle.radius),
        to5bits(circle.color.red),
        to5bits(circle.color.green),
        to5bits(circle.color.blue)
      )
      Gene(list.mkString(""))
    }

  }

  implicit class GeneToCircle(gene: Gene) {

    def toCircle(alpha: Int)(implicit gm: GeneMapping): Circle = {
      val c = gm.toComponents(gene)
      Circle(Center(c._1, c._2), c._3, Color(c._4, c._5, c._6, alpha))
    }
  }

  def neigh(gene: Gene)(implicit geneMapping: GeneMapping): List[Gene] = {

    def to2bits(i: Int) = {
      val k = (i + 4) % 4
      "%05d".format(k.toBinaryString.toInt)
    }

    def to4bits(i: Int) = {
      val k = (i + 16) % 16
      "%04d".format(k.toBinaryString.toInt)
    }

    def to5bits(i: Int) = {
      val k = (i + 32) % 32
      "%05d".format(k.toBinaryString.toInt)
    }

    def to6bits(i: Int) = {
      val k = (i + 64) % 64
      "%06d".format(k.toBinaryString.toInt)
    }

    def to7bits(i: Int) = {
      val k = (i + 128) % 128
      "%07d".format(k.toBinaryString.toInt)
    }

    def to8bits(i: Int, min: Int = 0): String = {
      var k = (i + 256) % 256
      if (k < min) k = min
      "%08d".format(k.toBinaryString.toInt)
    }

    def pow(i: Int): Int =
      if (i == 4) 16 else if (i == 5) 32 else if (i == 6) 64 else if (i == 7) 128 else if (i == 8) 256 else 0

    def toBits(value: Int, numberOfBits: Int): String = {
      require(numberOfBits >= 4 && numberOfBits <= 8, "Number of bits " + numberOfBits)
      val pow2 = pow(numberOfBits)
      val k = (value + pow2) % pow2
      val s = s"%0${numberOfBits}d"
      s.format(k.toBinaryString.toInt)
    }

    val t = geneMapping.toComponents(gene)
    val s = geneMapping.sizes

    val u = for {
      i <- List(0, 2, -2)
      j <- List(0, 2, -2)
      k <- List(0, 2, -2)
      l <- List(0, 31, -31)
      m <- List(0, 31, -31)
      n <- List(0, 31, -31)
    } yield (t._1 + i, t._2 + j, t._3 + k, t._4 + l, t._5 + m, t._6 + n)

    val ii = u.map(e =>
      toBits(e._1, s(0)) + toBits(e._2, s(1)) + toBits(e._3, s(2)) + toBits(e._4, s(3)) + toBits(e._5, s(4)) + toBits(e._6, s(5)))
    scala.concurrent.duration.Deadline
    ii.distinct.map(Gene(_))

  }

  implicit class ChromosomeToBufferedImage(chromosome: Chromosome) {

    def toBufferedImage(alpha: Int)(implicit dimensions: ImageDimensions): BufferedImage = {

      val circles: List[Circle] = chromosome.genes.map(_.toCircle(alpha)(chromosome.geneMapping))

      val image = new BufferedImage(dimensions.width, dimensions.height, BufferedImage.TYPE_3BYTE_BGR)

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
