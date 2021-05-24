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

  val edgeThickness = skp.positiveEdges.zipWithIndex.map {
    case (edge, j) => (edge, 1 + (2 * j) % 6)
  }

  val thicknessMap: Map[Edge, Int] = (edgeThickness ++ (edgeThickness.map {
    case (edge, j) => (edge.flip, j)
  })).toMap

  val colourMap: Map[Edge, Color] =
    (skp.positiveEdges.zip(edgeColours) ++ skp.positiveEdges
      .map(_.flip)
      .zip(edgeColours)).toMap

  val faceImageGen =
    skp.faceVector.map(SkewHexImageGen(_, colourMap, thicknessMap, radius))

  val faceToImageGen =
    faceImageGen.map(gon => gon.hex -> gon).toMap

  val faceToImage: Map[SkewPantsHexagon, Image] = faceToImageGen.map {
    case (hex, gen) => (hex, gen.polygon)
  }

  def facesWithPLPath(
      curve: PLPath,
      colour: Color = Color.red
  ): Map[SkewPantsHexagon, Image] = {
    val groupedArcs = curve.plArcs.groupBy(_.base.face)
    faceToImage ++ groupedArcs.map {
      case (hex, plArcs) =>
        val gen = faceToImageGen(hex)
        hex -> gen.plArcs(plArcs.map(arc => arc -> colour)).on(gen.polygon)
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
    faceToImage ++ groupedArcs.map {
      case (hex, arcCols) =>
        val gen = faceToImageGen(hex)
        hex -> gen.plArcs(arcCols).on(gen.polygon)
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
    thicknessMap: Map[Edge, Int],
    radius: Double = 100
) extends PolygonImageGen(hex.boundary.size, radius) {
  val edgeImages: Vector[Image] =
    hex.boundary.zipWithIndex.map {
      case (edge, j) =>
        edgePaths(j)
          .strokeWidth(thicknessMap(edge))
          .strokeColor(colourMap(edge))
    }

  val polygon: Image = edgeImages.foldLeft(Image.empty)(_ on _)
}
