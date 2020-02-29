---
author: "Arka Ghosh"
github-id: "anotherArka"
---

### `TwoComplex.scala`

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

20. `angleBetween` - Given two edges `e1` and `e2` gives the angle between them. Where angle is the number of turns to reach `e2` from `e1`. Left turns are considered positive and right turns are considered negative.

### `Quadrangulation.scala`
Defined the `quadrangulate` method which quadrangulates a closed surface and also gives forward and backward maps between edgepaths in the original TwoComplex and its quadragulation.

### `EdgePath.scala`

1. `cyclicalTake` - Defined this method which, given indexes `i` and `j` gives the subpath between `i`-th and `j`-th vertex. Because it works on loops `j` can be less than `i`.

2. `shiftBasePoint` - Defined this method which, In case the EdgePath is a loop, shifts the basePoint to the terminal of the first edge.

3. `loopToGeodesic` - Defined this method which, reduces a loop to geodesic.

4. `intersectionsWith` - Defined this method which, given another loop gives intersection along with signs.

5. `selfIntersection` - Defined this method which gives self intersections with signs.

6. `Intersection` - Defined the `trait` `Intersection`. This helps us to compute intersections between two loops.

### `Examples.scala` and `SphereComplex.scala`

Defined examples of non-surfaces, non-closed-surfaces, sufaces with boundaries.


### `VariousChecks.scala`

1. `Termination` - Defined this method to check terminations of the `quadrangulate` method and correctness of the forward and backward maps.

2. `HomotopyClassesOfPaths` -  Defined this trait which is  equivalence class of paths between two vertices. Contains methods to expand the homotopy classes by adding one more set, merge two homotopy classes, etc.

3. `CollectionOfHomotopyClasses` - Defined this trait which is a collection of homotopy classes between multiple pairs of edges.

4. `starter` - Defines basic homotopy classes by declaring paths in the boundary of a face to be homotopic when they start and end at the same vertex.

5. `mainCourse` - For homotopic pairs of paths `(a,b)` and `(c,d)` declares `a*c` and `b*d` to be homotopic and expands the collection of homotopy classes accordingly.




