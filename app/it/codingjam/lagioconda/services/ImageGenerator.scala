package it.codingjam.lagioconda.services

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import javax.inject.Singleton

import org.apache.commons.codec.binary.Base64OutputStream

import scala.util.Random

@Singleton
class ImageGenerator {

  def create(width: Int, height: Int): String = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

    val g2 = image.createGraphics()

    // g2.setPaint(gp)

    val qualityHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                          RenderingHints.VALUE_ANTIALIAS_ON)
    qualityHints.put(RenderingHints.KEY_RENDERING,
                     RenderingHints.VALUE_RENDER_QUALITY)
    g2.setRenderingHints(qualityHints)

    Range(1, 100).foreach { i =>
      val transparent = new Color(Random.nextInt(256),
                                  Random.nextInt(256),
                                  Random.nextInt(256),
                                  100)
      g2.setColor(transparent)
      g2.fill(
        new Ellipse2D.Float(Random.nextInt(width),
                            Random.nextInt(height),
                            Random.nextInt(500),
                            Random.nextInt(500)))
    }

    g2.dispose()

    val os = new ByteArrayOutputStream();
    val b64 = new Base64OutputStream(os);

    ImageIO.write(image, "png", b64);
    os.toString("UTF-8");

  }

}
