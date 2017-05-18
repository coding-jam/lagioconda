package it.codingjam.lagioconda.fitness

case class Rgb(r: Float, g: Float, b: Float) {

  def toXyz = {
    val r1 =
      if (r <= 0.04045) r / 12.92
      else Math.pow((r + 0.055) / 1.055, 2.4)
    val g1 =
      if (g <= 0.04045) g / 12.92
      else Math.pow((g + 0.055) / 1.055, 2.4)
    val b1 =
      if (b <= 0.04045) b / 12.92
      else Math.pow((b + 0.055) / 1.055, 2.4)

    // XYZ linear transform
    Xyz((r1 * 0.4124564 + g1 * 0.3575761 + b1 * 0.1804375).toFloat,
        (r1 * 0.2126729 + g1 * 0.7151522 + b1 * 0.0721750).toFloat,
        (r1 * 0.0193339 + g1 * 0.1191920 + b1 * 0.9503041).toFloat)
  }

}
case class Lab(l: Float, a: Float, b: Float)

case class Xyz(x: Float, y: Float, z: Float) {

  def toLab: Lab = {

    val epsilon = 0.008856
    // actual CIE standard
    val kappa = 903.3

    val Xr = 0.950456
    // reference white
    val Yr = 1.0
    val Zr = 1.088754

    val xr = x / Xr
    val yr = y / Yr
    val zr = z / Zr

    val norm = false

    val fx =
      if (xr > epsilon) Math.pow(xr, 1.0 / 3.0)
      else (kappa * xr + 16.0) / 116.0
    val fy =
      if (yr > epsilon) Math.pow(yr, 1.0 / 3.0)
      else (kappa * yr + 16.0) / 116.0
    val fz =
      if (zr > epsilon) Math.pow(zr, 1.0 / 3.0)
      else (kappa * zr + 16.0) / 116.0

    val Lscale =
      if (norm) 1f / 100f
      else 1
    val ascale =
      if (norm) 1f / 256f
      else 1
    val bscale =
      if (norm) 1f / 256f
      else 1
    val abdelta =
      if (norm) 127
      else 0

    Lab((116.0 * fy - 16.0).toFloat * Lscale,
        ((500.0 * (fx - fy)).toFloat + abdelta) * ascale,
        ((200.0 * (fy - fz)).toFloat + abdelta) * bscale)
  }

}
