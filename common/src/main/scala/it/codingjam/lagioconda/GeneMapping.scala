package it.codingjam.lagioconda

import it.codingjam.lagioconda.ga.Gene

case class GeneMapping(mX: Int, mY: Int, mR: Int, mRed: Int, mGreen: Int, mBlue: Int) {

  def toComponents(gene: Gene): (Int, Int, Int, Int, Int, Int) = {

    def parse(s: String) = Integer.parseInt(s, 2)

    val x = parse(gene.binaryString.substring(0, mX))
    val y = parse(gene.binaryString.substring(mX, mY))
    val radius = (parse(gene.binaryString.substring(mY, mR)))
    val red = parse(gene.binaryString.substring(mR, mRed))
    val green = parse(gene.binaryString.substring(mRed, mGreen))
    val blue = parse(gene.binaryString.substring(mGreen, mBlue))

    (x, y, radius, red, green, blue)
  }

  def toList = List(mX, mY, mR, mRed, mGreen, mBlue)

  def sizes = toList.zip(0 :: toList).map(x => x._1 - x._2)
}
