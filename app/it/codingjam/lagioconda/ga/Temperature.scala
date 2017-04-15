package it.codingjam.lagioconda.ga

case class Temperature(degrees: Double = 1.0) {

  def decrease = copy(degrees - 0.00001)

}
