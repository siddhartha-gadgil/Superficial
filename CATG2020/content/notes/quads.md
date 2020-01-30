---
title: "Quadrangulations: Constructions and Geodesics"
date: 2020-01-29
---

We previously worked with general two-complexes, including those representing surfaces. We now specialize to the first of the three special representations that are most useful to us - _non-positive quadrangulations_. Here we discuss what these are, how to construct these and the geodesics for these.

##### Non-positive quadrangulations

For a closed surface `$\Sigma$`, a non-positive quadrangulation is a two-complex homeomorphic to `$\Sigma$` such that

1. Every face has four sides.
2. The degree of (i.e., number of edges terminating in) each vertex is at least `$5$`.

##### Constructing quadrangulations

Start with a `$2$`-complex `$\Delta$` representing `$\Sigma$`. We describe a general procedure for constructing quadrangulations `$Q$`. If the `$2$`-complex `$\Delta$` has one vertex and one face, and the surface has genus at least `$2$`, then the quadrangulation `$Q$` we construct is automatically negatively curved.

The quadrangulation is the two complex `$Q$` given in terms of `$\Delta$` as follows.

1. __Vertices:__ the vertices of `$Q$` are the vertices of `$\Delta$` together with a vertex `$b_f$` for each face `$f$` of `$\Delta$` (thought of as the _barycenter_ of `$f$`).
2. __Edges:__ For a face `$f$` with `$n$` sides, with boundary `$e_0(f), e_1(f), \dots, e_{n-1}(f)$`, we have `$n$` edges `$\theta_i(f)$`, `$0 \leq i < n$`, with `$\theta_i$` starting at `$b_f$` and ending at `$e_i(f)$`. Note that the edges of `$\Delta$` are not edges of `$Q$`.
3. __Faces:__ There is one face `$\Phi(\eta)$` in `$Q$` for each edge-pair `$(\eta, \bar\eta)$`, where _bar_ denotes flip. Namely, we have unique faces `$f$` and `$g$` and indices `$i$` and `$j$` so that `$\eta=e_i(f)$` and `$\bar\eta = e_j(g)$`. The corresponding face `$\Phi(\eta)$` is the quadrilateral with boundary `$\theta_i(f)$`, `$\bar\theta_{j + 1}(g)$`, `$\theta_j (g)$`, `$\bar\theta_i(f)$` (check this before implementing).

##### Geodesics

Fix a non-positive quadrangulation, so each vertex has degree at least `$5$`, and let `$\eta$` be an edge. The left edge `$\theta$` following `$\eta$` is the successor in the unique face containing `$\eta$` as a directed edge. This is the next edge in the sharpest left turn. We call going from `$\eta$` to `$\theta$` a _left turn_.

The _slight left turn_  is the next sharpest left turn, obtained by taking a left turn, flipping and taking a left turn again (remember that this is a 5 or more pointed junction, so the terminology is justified). We can similarly define _right turn_ and _slight right turn_ (using flips and predecessors) as indicated in the previous note.

If an edge path has a segment where we make a left turn, followed by `$0$` or more slight left turns and then another left turn, it is not a geodesic, and similarly with rights in place of lefts. Indeed in both these cases there is a clear shortening transformation that keeps the ends fixed and is a homotopy. We can apply such transformations till there are no obvious ones left.

The key point about non-positive curvature is that if there are no shortenings of the above form, the curve we have is a _geodesic_. Further geodesics are almost unique, and there is indeed a canonical leftmost geodesics. Many questions can be addressed using these.
