
# Torus as a two-comples

This is an introduction to the implementation of two-complexes, including issues with scala, ending with constructing a torus. To start with we load the compiled code.


```scala
import $cp.bin.`superficial-c3516ca6f4.jar`
import superficial._
```




    [32mimport [39m[36m$cp.$                               
    [39m
    [32mimport [39m[36msuperficial._[39m



## A vertex

The code for vertices is rather simple.

```scala
class Vertex
```

Let us first create a vertex. As it has no abstract methods, we can just ask for a new one. 


```scala
val v0 = new Vertex
```




    [36mv0[39m: [32mVertex[39m = superficial.Vertex@75085bd8



Alternatively, we can declare a new object.


```scala
object v1 extends Vertex
```




    defined [32mobject[39m [36mv1[39m




```scala
3: Int
```




    [36mres4[39m: [32mInt[39m = [32m3[39m




```scala
v1 : v1.type
```




    [36mres3[39m: [32mv1[39m.type = ammonite.$sess.cmd2$Helper$v1$@5328d611



## Edges

Edges come in pairs, an edge and the same with the opposite orientation (i.e., flipped). Further, each edge has an initial and terminal vertex. We can also describe the boundary as a formal sum. This is _trait_, i.e., an _abstract class_. This means some of its fields are undefined, and have to be defined in a subclass/object with this trait.

```scala
trait Edge {
  /**
   * the same edge with the opposite orientation.
   */
  def flip: Edge

  def terminal: Vertex

  def initial: Vertex

  def del : FormalSum[Vertex] = FormalSum.reduced(Vector(terminal -> 1, initial -> -1))
}
```

### A loop

We construct a _loop_ at `v0`, i.e., an edge beginning and ending at `v0`. But note that we cannot construct just one, as we need to flip it. Instead we will construct a pair of edges, each a flip of the other. 


```scala
object E1 extends Edge{
    lazy val flip = E2
    
    val terminal = v0
    
    val initial = v0        
}

object E2 extends Edge{
    lazy val flip = E1
    
    val terminal = v0
    
    val initial = v0        
}
```




    defined [32mobject[39m [36mE1[39m
    defined [32mobject[39m [36mE2[39m



Note a key feature above - we have defined the flip of each of the edges as `lazy val`. This means this is not computed until it is needed. Otherwise defining each of `E1` and `E2` leads to an infinite loop. 

We can see some computations on demand.


```scala
E1.flip
```




    [36mres6[39m: [32mE2[39m.type = ammonite.$sess.cmd5$Helper$E2$@444af6b3




```scala
E1.flip == E2
```




    [36mres7[39m: [32mBoolean[39m = true




```scala
E1.flip.flip == E1
```




    [36mres8[39m: [32mBoolean[39m = true




```scala
E1 == E2
```




    [36mres9[39m: [32mBoolean[39m = false



## Torus time

We now turn to constructing a torus. Recall that this has a single vertex, two edges that become loops and a single face. We will follow the same pattern as above but give things nicer names. We will also make objects `case object` definitions. This means the names are cleaner (as is equality).


```scala
case object V extends Vertex
```




    defined [32mobject[39m [36mV[39m




```scala
case object VV extends Vertex
```




    defined [32mobject[39m [36mVV[39m




```scala
V == VV
```




    [36mres12[39m: [32mBoolean[39m = false




```scala
case class VC() extends Vertex
case class VVC() extends Vertex
```




    defined [32mclass[39m [36mVC[39m
    defined [32mclass[39m [36mVVC[39m




```scala
VC() == VVC()
```




    [36mres14[39m: [32mBoolean[39m = false




```scala
VC() == VC()
```




    [36mres15[39m: [32mBoolean[39m = true




```scala
case object A extends Edge{
    lazy val flip = Abar
    
    val terminal = V
    
    val initial = V        
}

case object Abar extends Edge{
    lazy val flip = A
    
    val terminal = V
    
    val initial = V        
}
```




    defined [32mobject[39m [36mA[39m
    defined [32mobject[39m [36mAbar[39m




```scala
case object B extends Edge{
    lazy val flip = Bbar
    
    val terminal = V
    
    val initial = V        
}

case object Bbar extends Edge{
    lazy val flip = B
    
    val terminal = V
    
    val initial = V        
}

```




    defined [32mobject[39m [36mB[39m
    defined [32mobject[39m [36mBbar[39m




```scala
A.flip
```




    [36mres18[39m: [32mAbar[39m.type = Abar




```scala
Abar.flip
```




    [36mres19[39m: [32mA[39m.type = A



Now we construct the face of the torus. Here is an extract from the code for defining a polygon. We have omitted  _concrete_ methods, i.e., methods in the trait that are defined, so need not be implemented in subclasses/objects.

```scala
trait Polygon extends TwoComplex {
  val sides: Int

  val boundary: Vector[Edge]

  val vertices: Set[Vertex]
}
```

Note that this is a two-complex, so we are constructing a torus.


```scala
case object Torus extends Polygon{
    val sides = 4
    
    val boundary = Vector(A, B, Abar, Bbar)
    
    val vertices = Set(V)
}
```




    defined [32mobject[39m [36mTorus[39m




```scala
Torus.faces
```




    [36mres21[39m: [32mSet[39m[[32mPolygon[39m] = [33mSet[39m(Torus)




```scala
Torus.edges
```




    [36mres22[39m: [32mSet[39m[[32mEdge[39m] = [33mSet[39m(A, Abar, B, Bbar)




```scala
Torus.boundary
```




    [36mres24[39m: [32mVector[39m[[32mProduct[39m with [32mEdge[39m with [32mSerializable[39m{def initial: ammonite.$sess.cmd10.instance.V.type;def terminal: ammonite.$sess.cmd10.instance.V.type;def flip: Product with superficial.Edge with java.io.Serializable}] = [33mVector[39m(
      A,
      B,
      Abar,
      Bbar
    )



Note that there are subclasses that let us construct more directly, but for now (i.e. as of Jan 13, 2020) they are not sufficient. We took a more direct approach to illustrate the structures.
