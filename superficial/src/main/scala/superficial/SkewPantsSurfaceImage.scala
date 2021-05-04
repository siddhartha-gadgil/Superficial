package superficial
import doodle.core._
import doodle.image.Image
import doodle.syntax._
import doodle.image.syntax._
import PathElement._
import cats.syntax._
import PolygonImageGen._
import Polygon.Index

final case class SkewPantsSurfaceImage(skp: SkewPantsSurface, radius: Double) {
  val edgeNum = skp.positiveEdges.size
  val colSpin = Angle(Angle.TwoPi / edgeNum.toDouble)
  val edgeColours =
    skp.positiveEdges
      .scanLeft(Color.hsl(0.degrees, 0.8, 0.6)) {
        case (colour, _) => colour.spin(colSpin)
      }
      .init
  val colourMap: Map[Edge, Color] =
    (skp.positiveEdges.zip(edgeColours) ++ skp.positiveEdges
      .map(_.flip)
      .zip(edgeColours)).toMap

  val faceImageGen = skp.faceVector.map(SkewHexImageGen(_, colourMap, radius))

  val faceToImageGen =
    faceImageGen.map(gon => gon.hex -> gon).toMap

  val faceToImage: Map[SkewPantsHexagon, Image] = faceToImageGen.map {
    case (hex, gen) => (hex, gen.polygon)
  }

  def facesWithPLPath(
      curve: PLPath,
      colour: Color = Color.azure
  ): Map[SkewPantsHexagon, Image] = {
    val groupedArcs = curve.plArcs.groupBy(_.base.face)
    groupedArcs.map {
      case (hex, plArcs) =>
        hex -> faceToImageGen(hex).plArcs(plArcs.map(arc => arc -> colour))
    }
  }

  def facesWithPLPaths(
      curveColours: Seq[(PLPath, Color)]
  ): Map[SkewPantsHexagon, Image] = {
    val arcColours =
      curveColours.flatMap {
        case (path, colour) => path.plArcs.map(_ -> colour)
      }
    val groupedArcs = arcColours.groupBy(_._1.base.face)
    groupedArcs.map {
      case (hex, arcCols) => hex -> faceToImageGen(hex).plArcs(arcCols)
    }
  }

  def imageGrid(images: Map[SkewPantsHexagon, Image] = faceToImage) = {
    skp.indices
      .map { pants =>
        (images(SkewPantsHexagon(pants, true, skp.cs))
          .on(Image.square(radius * 2.2)))
          .above(
            (images(SkewPantsHexagon(pants, false, skp.cs)))
              .on(Image.square(radius * 2.2))
          )
      }
      .foldLeft(Image.empty)(_ beside _)
  }
}

case class SkewHexImageGen(
    hex: SkewPantsHexagon,
    colourMap: Map[Edge, Color],
    radius: Double = 100
) extends PolygonImageGen(hex.boundary.size, radius) {
  val edgeImages: Vector[Image] =
    hex.boundary.zipWithIndex.map {
      case (edge, j) => edgePaths(j).strokeColor(colourMap(edge))
    }

  val polygon: Image = edgeImages.foldLeft(Image.empty)(_ on _)
}
