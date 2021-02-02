import superficial._

val genus2 = PantsSurface.allClosed(2).toVector
genus2.size

genus2.forall(_.isClosedSurface) 

val genus3 = PantsSurface.allClosed(4).toVector
genus3.size

genus3.forall(_.isClosedSurface) 

val genus4 = PantsSurface.allClosed(6).toVector

genus4.forall(_.isClosedSurface)

genus4.size

PantsSurface.all(5).forall(_.isSurfaceWithBoundary)