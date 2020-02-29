---
author: "Arka Ghosh"
github-id: "anotherArka"
---

## `TwoComplex.scala`

1. `allCollapsed` - Extended definition to give forward and backward maps along with the surface with one vertex.

2. `collapseEdge` -  Extended definition to give forward and backward maps along with the surface with one vertex.

3. `succOpt` - Defined this method which gives the succesor of an edge if possible.

4. `predOpt` - Defined this method which gives the predecessor of an edge if possible.

5. `rotateLeftOpt` - Defined this method which gives the result of rotating an edge by left once when such rotation is possible.

6. `rotateRightOpt` - Defined this method which gives the result of rotating an edge by right once when such rotation is possible.

7. `allEdgesToTheLeftOf` - Defined this method which gives all edges to the left of the given edge.

8. `allEdgesToTheRightOf` - Defined this method which gives all edges to the right of the given edge.

9. `edgesEndingAt` - Defined this method which gives all edges ending at the given vertex.

10. `isEdgeAtBoundary` - Defined this method which checks if the given edge is at the boundary. That is exactly one of `e` and `e.flip` is inside a face of the twocomplex.

11. `isClosedSurface` - Defined this method which checks if the TwoComplex is a closed surface.

12. `isSurfaceWithBoundary` - Defined this method which checks if the TwoComplex is a surface with boundary

13. `addVertices` - Defined this method which, given a set of vertices `vs` gives the TwoComplex got by adding `vs` to the existing TwoComplex. If `vs` is already inside gives the same TwoComplex.  

14. `addEdges` - Defined this method which, given a set of edges `eds` gives the TwoComplex got by adding `eds` to the existing TwoComplex. If `eds` is already inside gives the same TwoComplex.

15. `addFaces` - Defined this method which, given a set of faces `fcs` gives the TwoComplex got by adding `fcs` to the existing TwoComplex. If `fcs` is already inside gives the same TwoComplex.

16. `addTwoComplexes` - Defined this method which, gives the result of adding the given set of Twocomplexes to the existing one.

17. `subComplex` - Defined this method which, given a set of vertices gives the subcomplex on the vertices.

18. `slightLeft` - Defined this method which, given an edge `e` rotates left twice and flips it. This is same as rotating left once and then taking the successor. 

19. `slightRight` - Given e takes two right rotations and flips it.

