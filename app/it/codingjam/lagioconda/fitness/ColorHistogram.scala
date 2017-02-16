/*
 * Copyright (c) 2011-2015 Jarek Sacha. All Rights Reserved.
 *
 * Author's e-mail: jpsacha at gmail.com
 */

// class taken from package opencv2_cookbook.chapter04
// TODO: ask how to proper include it.

package it.codingjam.lagioconda.fitness

import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacpp.{FloatPointer, IntPointer, PointerPointer}

/**
  * Helper class that simplifies usage of OpenCV `calcHist` function for color images.
  *
  * See OpenCV [[http://opencv.itseez.com/modules/imgproc/doc/histograms.html?highlight=histogram]]
  * documentation to learn backend details.
  */
class ColorHistogram(var numberOfBins: Int = 256) {

  private val _minRange = 0.0f
  private val _maxRange = 255.0f

  /**
    * Computes histogram of an image.
    *
    * @param image input image
    * @return OpenCV histogram object
    */
  def getHistogram(image: Mat): Mat = {

    require(image != null)
    require(image.channels == 3, "Expecting 3 channel (color) image")

    // Compute histogram
    val hist = new Mat()

    // Since C++ `calcHist` is using arrays of arrays we need wrap to do some wrapping
    // in `IntPointer` and `PointerPointer` objects.
    val intPtrChannels = new IntPointer(0, 1, 2)
    val intPtrHistSize =
      new IntPointer(numberOfBins, numberOfBins, numberOfBins)
    val histRange = Array(_minRange, _maxRange)
    val ptrPtrHistRange =
      new PointerPointer[FloatPointer](histRange, histRange, histRange)
    calcHist(
      image,
      1, // histogram of 1 image only
      intPtrChannels, // the channel used
      new Mat(), // no mask is used
      hist, // the resulting histogram
      3, // it is a 3D histogram
      intPtrHistSize, // number of bins
      ptrPtrHistRange, // pixel value range
      true, // uniform
      false
    ) // no accumulation
    hist
  }
}
