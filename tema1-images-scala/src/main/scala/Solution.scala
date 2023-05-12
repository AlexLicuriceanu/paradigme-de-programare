import util.Pixel
import util.Util

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]
  type Str = List[List[Char]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val lines = image.mkString.split("\n")

    val format = lines(0)
    val width = lines(1).split(" ")(0).toInt
    val height = lines(1).split(" ")(1).toInt
    val maxColor = lines(2).toInt

    val pixels = lines.slice(3, lines.length).grouped(width).map(
      _.map(pixel => {
        val colors = pixel.split(" ").map(_.toInt)
        Pixel(colors(0), colors(1), colors(2))
      }).toList
    )

    pixels.toList
  }

  def toStringPPM(image: Image): List[Char] = {
    val width = image.head.length
    val height = image.length
    val header = s"P3\n${width} ${height}\n255\n"
    val pixels = image.flatMap(_.map(pixel => s"${pixel.red} ${pixel.green} ${pixel.blue}\n"))

    header.toList ++ pixels.mkString
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    image1.zip(image2).map(_ ++ _)
  }

  // ex 3
  def transpose(image: Image): Image = {
    image match {
      case Nil :: _ => Nil
      case _ => image.map(_.head) :: transpose(image.map(_.tail))
    }
  }

  def rotate(image: Image, degrees: Int): Image = {
    degrees match {
      case 90 => transpose(image).reverse
      case 180 => rotate(rotate(image, 90), 90)
      case 270 => rotate(rotate(rotate(image, 90), 90), 90)
      case _ => image
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def combine(image1: GrayscaleImage, image2: GrayscaleImage): GrayscaleImage = {
    image1.zip(image2).map(_.zip(_).map((x, y) => math.abs(x) + math.abs(y)))
  }

  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayscaleImage = image.map(_.map(pixel => Util.toGrayScale(pixel)))
    val blurredImage = applyConvolution(grayscaleImage, gaussianBlurKernel)

    val Mx = applyConvolution(blurredImage, Gx)
    val My = applyConvolution(blurredImage, Gy)
    val combinedImage = combine(Mx, My)

    combinedImage.map(_.map(pixel => if (pixel < threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighborPixels = Util.getNeighbors(image, kernel.size/2)
    neighborPixels.map(row =>
      row.map(neighbors => {
        val convolution = (neighbors).zip(kernel)
          .map((neighborRow, kernelRow) => (neighborRow).zip(kernelRow).map(_ * _).sum
          ).sum

        convolution
      }
      )
    )
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def addPadding(triangle: List[List[Int]]): List[List[Int]] = {
      val size = triangle.length
      triangle.map(line => line ++ List.fill(size - line.size)(-1))
    }

    @tailrec
    def generateTriangle(triangle: List[List[Int]], size: Int): List[List[Int]] = {
      def generateLine(previousLine: List[Int]): List[Int] = {
        (previousLine ++ List(0)).zip(List(0) ++ previousLine)
          .map((x, y) => (x + y) % m)
      }

      size match {
        case 0 => addPadding(triangle)
        case _ => generateTriangle(triangle :+ generateLine(triangle.last), size-1)
      }
    }

    val pascalsTriangle = generateTriangle(List(List(1)), size-1)
    pascalsTriangle.map(_.map(funct(_)))
    //pascalsTriangle.map(_.map(pixel => if (pixel != -1) funct(pixel) else Pixel(0, 0, 0)))
  }
}
