---
title: Intersections of Combinatorial Geodesics (following Despre-Lazarus)
date: 2020-03-19
---

This is a sketch of the description of intersection numbers in the reference paper, including why it works. I have not checked some of the technical lemmas. I also focus on the basic case - different, primitive curves in a closed, oriented surface. I will mainly just describe stuff.

##### Geometric intersections and geodesics

The guiding example is the description of intersection numbers in terms of hyperbolic geometry. The classic reference is the book _Automorphisms of Surfaces after Nielsen and Thurston_ by Casson and Bleiler.

Fix a hyperbolic surface $\Sigma$ with distinct primitive geodesic closed curves `$\alpha$` and `$\beta$` on the surface. The universal covering map is `$p: \mathbb{H} \to \Sigma$`from the hyperbolic plane. We can describe the geometric intersection number in terms of the inverse images of `$\alpha$` and `$\beta$`. 

Namely, the inverse images are a collection of geodesics in `$\mathbb{H}$`, which can be viewed as arcs in `$\mathbb{H}\cup \partial\mathbb{H} = D^2$`. Further, given two geodesics in the universal cover which are lifts of the given curves, these either coincide or their endpoints are disjoint. Hence if the geodesics are distinct they either intersect in a single point or none, and which case is determined by whether the endpoints are linked.

##### Transveral intersections

If we take general transversal curves, they may have excess intersection numbers. This corresponds to lifts intersecting in pairs of points bounding bigons even if they are not linked. All algorithms are based on avoiding this situation.

##### The Combinatorial intersection

The key point is that canonical curves `$c_R$` and  `$d$` do not have bigons. However they may not be transversal, intersecting instead in double paths in general. Further, we may have _non-crossing_ double paths, which correspond to pairs of geodesics that are not linked in the universal cover. Further, the double curve may be with reversed direction. 

Thus one has to:

* count only crossing double paths - this is achieved by determining crossing.
* count double paths just once, even for reversed paths.
* avoid double counting for a pair of geodesics with these counted once as a forward double path and once as a reverse double path.

To achieve this, after determining crossings, three sets are defined. Note that by double path in the reverse direction we mean a double path of `$c_L$`. The cases are determined by the _segments_ of intersection with the region `$\Delta$` between `$c_L$` and `$c_R$`.

* $D_+$ - this is the set of crossing double paths with length at least `$1$` in the forward direction.
* $D_0$ - these correspond to `$d$` crossing `$c_R$` in a single point, but the same segment intersecting $c_L$ in the _forward direction_ in at least $1$ edge.
* $D_-$ - these correspond to crossing double paths in the reverse direction, but conditions ruling out being counted in one of the above cases.

Any pair of crossing canonical curves in the universal cover correspond to exactly one point in exactly one of these sets. This is proved by first classifying possible intersection segments between a region such as `$\Delta$` and a canonical curve.
