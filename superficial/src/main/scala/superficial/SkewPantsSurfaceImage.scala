package superficial
import doodle.core._
// import doodle.image.Image
import doodle.syntax._
import doodle.image.syntax._
import PathElement._
import cats.syntax._
import PolygonImageGen._
import Polygon.Index
import doodle.java2d._
import cats.implicits._

final case class SkewPantsSurfaceImage(skp: SkewPantsSurface, radius: Double) {
  val edgeNum = skp.positiveEdges.size
  val colSpin = Angle(Angle.TwoPi / edgeNum.toDouble)
  val edgeColours =
    skp.positiveEdges
      .scanLeft(Color.hsl(0.degrees, 0.8, 0.6)) {
        case (colour, _) => colour.spin(colSpin)
      }
      .init

  val edgeThickness = skp.positiveEdges.map {
    case edge: SkewCurveEdge => (edge, 1)
    case edge                => (edge, 3)
  }

  val thicknessMap: Map[Edge, Int] = (edgeThickness ++ (edgeThickness.map {
    case (edge, j) => (edge.flip, j)
  })).toMap

  val colourMap: Map[Edge, Color] =
    (skp.positiveEdges.zip(edgeColours) ++ skp.positiveEdges
      .map(_.flip)
      .zip(edgeColours)).toMap

  val labelMap: Map[Edge, Int] = skp.positiveEdges.zipWithIndex.flatMap {
    case (e, j) => Vector(e -> (j + 1), e.flip -> (j + 1))
  }.toMap

  val faceImageGen =
    skp.faceVector.map(
      SkewHexImageGen(_, colourMap, thicknessMap, labelMap, radius)
    )

  val faceToImageGen =
    faceImageGen.map(gon => gon.hex -> gon).toMap

  val faceToImage: Map[SkewPantsHexagon, Picture[Unit]] = faceToImageGen.map {
    case (hex, gen) => (hex, gen.polygon)
  }

  def facesWithPLPath(
      curve: PLPath,
      colour: Color = Color.red
  ): Map[SkewPantsHexagon, Picture[Unit]] = {
    val groupedArcs = curve.plArcs.groupBy(_.base.face)
    faceToImage ++ groupedArcs.map {
      case (hex, plArcs) =>
        val gen = faceToImageGen(hex)
        hex -> gen.plArcs(plArcs.map(arc => arc -> colour)).on(gen.polygon)
    }
  }

  def facesWithPLPaths(
      curveColours: Seq[(PLPath, Color)]
  ): Map[SkewPantsHexagon, Picture[Unit]] = {
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

  def facesWithThickPLPaths(
      curveColours: Seq[(PLPath, Color, Int)]
  ): Map[SkewPantsHexagon, Picture[Unit]] = {
    val arcColours =
      curveColours.flatMap {
        case (path, colour, width) =>
          path.plArcs.map(arc => (arc, colour, width))
      }
    val groupedArcs = arcColours.groupBy(_._1.base.face)
    faceToImage ++ groupedArcs.map {
      case (hex, arcCols) =>
        val gen = faceToImageGen(hex)
        hex -> gen.thickPlArcs(arcCols).on(gen.polygon)
    }
  }

  def imageGrid(images: Map[SkewPantsHexagon, Picture[Unit]] = faceToImage) = {
    skp.indices
      .map { pants =>
        (images(SkewPantsHexagon(pants, true, skp.cs))
          .on(square[Algebra, Drawing](radius * 2.2)))
          .above(
            (images(SkewPantsHexagon(pants, false, skp.cs)))
              .on(square[Algebra, Drawing](radius * 2.2))
          )
      }
      .reduce(_ beside _)
  }
}

case class SkewHexImageGen(
    hex: SkewPantsHexagon,
    colourMap: Map[Edge, Color],
    thicknessMap: Map[Edge, Int],
    labelMap: Map[Edge, Int],
    radius: Double = 100
) extends PolygonImageGen(hex.boundary.size, radius) {
  val edgeImages: Vector[Picture[Unit]] =
    hex.boundary.zipWithIndex.map {
      case (edge, j) =>
        val (ep, mid) = edgePathsMid(j)
        val path = ep
          .strokeWidth(thicknessMap(edge))
          .strokeColor(colourMap(edge))
        val label = text[Algebra, Drawing](labelMap(edge).toString()).at(mid)
        label.on(path)
    }

  val polygon = edgeImages.reduce(_ on _)
}

object SkewPantsSurfaceImage {
  def arcsImage(
      surf: SkewPantsSurface,
      curveColours: Seq[(PLPath, Color)],
      size: Int = 150
  ): doodle.algebra.Picture[Algebra, Drawing, Unit] = {
    val skim: SkewPantsSurfaceImage = SkewPantsSurfaceImage(surf, size)
    val arcMap = skim.facesWithPLPaths(curveColours)
    skim.imageGrid(arcMap)
  }

  def arcPairImage(
      surf: SkewPantsSurface,
      arc1: PLPath,
      arc2: PLPath,
      size: Int = 150
  ) = {
    val skim: SkewPantsSurfaceImage = SkewPantsSurfaceImage(surf, size)
    val arcMap = skim.facesWithThickPLPaths(
      List((arc1, Color.black, 1), (arc2, Color.grey, 3))
    )
    skim.imageGrid(arcMap)
  }
}
