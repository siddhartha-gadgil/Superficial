---
author: "Chinmaya Kausik"
github-id: "Chinmaya-Kausik"
---

### `TwoComplex.scala`

1. `connectedComponent` gives the connected component of a vertex.

2. `isConnected` determines whether a Two Complex is connected.

3. `turnLeft` and `turnRight` turn left and right respectively.

4. `L`, `R` force lefts and rights respectively and give errors otherwise.

5. `SL`, `SR` force slight lefts and rights and give errors otherwise.

6. `SwL`, `SwR` force left and right swivels about the initial edge.

7. `vectorOrbit`  gives the vector of edges related by a function opt to an edge, modified version of orbit.

8. `vectorLeftTurns`, `vectorRightTurns` give the vector of left and right turns respectively from an edge.

9. `vectorEdgesToTheLeftOf`, `vectorEdgesToTheRightOf` give the vector of edges to the left and right respectively of an edge, sharing the same terminal vertex.

10. `turnIndex` gives the index of the turn between two edges, if defined.

11. `turnEdge` gives the edge obtained by turning from the given edge by the given index.

### `Edge.scala`

1. `++` concatenates EdgePaths.

2. `+` adds an edge to an EdgePath.

3. `isHomotopicTo` checks if two paths are homotopic fixing endpoints.

4. `isFreelyHomotopicTo` checks if two loops are freely homotopic.

5. `apply` creates an EdgePath from a vector of edges.

6. `isReduced` checks if an edgePath is reduced.

7. `edgeVectors` gives the edges corresponding to an EdgePath.

8. `turnPath` gives the initial edge and the vector of subsequent turn indices of an EdgePath.

9. `turnPathToEdgePath` gives the EdgePath corresponding to an initial edge and a vector of turn indices.

10. `findFirstLeftBracketTurnPath`, `findFirstRightBracketTurnPath` find the first left and right turn brackets in a turn path respectively.

11. `isGeodesic` checks if an EdgePath is a geodesic.

12. `turnPathStandardForm` converts a turn path to it standard form (1,2,-1,-2 for L,SL,R,SR respectively).

13. `turnPathReduce`, `edgePathReduce` reduce turn paths and EdgePaths respectively.

14. `turnPathToGeodesic`, `edgePathToGeodesic` convert turn paths and EdgePaths respectively to geodesics.

15. `canoniciseTurnLoop`, `canoniciseLoop` canonicise loops in turn path form and EdgePath form respectively.

16. `isGeodesicLoop` checks if a loop is a geodesic loop.

17. `isCanonicalGeodesicLoop` checks if a loop is a canonical geodesic loop.
