package it.codingjam.ga

import it.codingjam.lagioconda.ga.Gene

package object converter {

  def toComponents(gene: Gene): (Int, Int, Int, Int, Int, Int, Int) = {

    def parse(s: String) = Integer.parseInt(s, 2)

    val x = parse(gene.binaryString.substring(0, 8))
    val y = parse(gene.binaryString.substring(8, 16))
    val radius = (parse(gene.binaryString.substring(16, 21)) * 1.2).toInt
    val red = parse(gene.binaryString.substring(21, 26)) * 8
    val green = parse(gene.binaryString.substring(26, 31)) * 8
    val blue = parse(gene.binaryString.substring(31, 36)) * 8
    val alpha = parse(gene.binaryString.substring(36, 38))

    (x, y, radius, red, green, blue, 64 + (alpha * 30))
  }

}
