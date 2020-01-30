---
title: "Surfaces and Turns"
date: 2020-01-28
---

In this note we describe how to determine if a two-complex is an oriented surface (possibly with boundary), with polygons oriented according to the orientation of the surface. Some cases may be missed below, so please check the description in addition to implementing it. Assume that we are given a two-complex `complex: TwoComplex`.

##### First steps

1. Check that the complex is valid, using `complex.checkComplex` (also check if that method covers all checks).
2. Check that every vertex is the initial vertex of an edge.
3. Check that every edge is in _at most_ one face, and appears at most once in the boundary (for this and the next checks, may help to consider `complex.faces.toVector.flatMap(_.boundary)`).
4. For closed surfaces, check that each edge `e` is in some face. If you allow boundary, check that at least one of `e` and `e.flip` is in a face.

##### Some functions for checking

We use some functions for checking that we have a surface. These will be extended for non-negative quadrangulation.

1. `succOpt: Edge -> Option[Edge]` - given an edge, find a face whose boundary contains `e` (if it exists, it is unique); take the _next_ edge along the boundary.
2. `predOpt: Edge -> Option[Edge]` - given an edge, find a face whose boundary contains `e` (if it exists, it is unique); take the _previous_ edge along the boundary.
3. `rotateLeftOpt: Edge -> Option[Edge]` - `succOpt` mapped by flip; gives the edge with same terminal vertex obtained by left rotation.
4. `rotateRightOpt: Edge -> Option[Edge]` - gives the edge with same terminal vertex obtained by right rotation.

##### Checking for surfaces.

1. For a _closed surface_, if we start with an edge `e` with `v == e.terminal`, using left rotations, (by iterating) we should get all edges with terminal vertex `v`.
2. For a _surface with boundary_, if we start with an edge `e` with `v == e.terminal`, using left and right rotations, (by iterating) we should get all edges with terminal vertex `e.terminal`.

##### Other steps.

These will be useful for non-positive quadrangulations. The terminology makes sense if each vertex has valence at least $5$.

1. We can define a _slight left_ (optional) step which given `e` takes its successor, then flips and then takes successor again.
2. We can also define _slight right_ using flips and predecessors.
