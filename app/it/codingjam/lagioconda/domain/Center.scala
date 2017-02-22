package it.codingjam.lagioconda.domain

case class Color(red: Int, green: Int, blue: Int, alpha: Int)

case class Center(x: Int, y: Int)

case class Circle(center: Center, radius: Int, color: Color)

case class ImageDimensions(width: Int, height: Int)
