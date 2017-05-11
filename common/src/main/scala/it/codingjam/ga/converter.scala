package it.codingjam.ga

import it.codingjam.lagioconda.ga.Gene

package object converter {

  def toComponents(gene: Gene): (Int, Int, Int, Int, Int, Int) = {

    def parse(s: String) = Integer.parseInt(s, 2)

    val x = parse(gene.binaryString.substring(0, 8))
    val y = parse(gene.binaryString.substring(8, 16))
    val radius = (parse(gene.binaryString.substring(16, 22)) * 2 + 2).toInt
    val red = parse(gene.binaryString.substring(22, 28)) * 4
    val green = parse(gene.binaryString.substring(28, 34)) * 4
    val blue = parse(gene.binaryString.substring(34, 40)) * 4

    (x, y, radius, red, green, blue)
  }

}
