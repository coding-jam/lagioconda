package it.codingjam.ga

import it.codingjam.lagioconda.ga.Gene

package object converter {

  def toComponents(gene: Gene): (Int, Int, Int, Int, Int, Int) = {

    def parse(s: String) = Integer.parseInt(s, 2)

    val x = parse(gene.binaryString.substring(0, 8))
    val y = parse(gene.binaryString.substring(8, 16))
    val radius = (parse(gene.binaryString.substring(16, 24)))
    val red = parse(gene.binaryString.substring(24, 32))
    val green = parse(gene.binaryString.substring(32, 40))
    val blue = parse(gene.binaryString.substring(40, 48))

    (x, y, radius, red, green, blue)
  }

}
