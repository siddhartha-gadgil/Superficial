# Superficial

Identifying simple closed curves over an orientable surface (topology).

## Implemented

An [algorithm](Reference.pdf) given by D.R.J. Chillingworth, that determines whether a given element of [Funamental Group](https://en.wikipedia.org/wiki/Fundamental_group) of an [orientable](https://en.wikipedia.org/wiki/Orientability) surface which is not a sphere; can be represented by a [simple](http://www.mathwords.com/s/simple_closed_curve.htm) closed curve.


## Getting Started

### Setting Things Up
One needs to install [sbt](http://www.scala-sbt.org/release/docs/Setup.html) first and then should follow following steps:
* Clone this repository and move to home directory of this project as below,
```
$ cd Superficial
```
* Run sbt-update once as below, to install the required scala version
```
$ sbt update
```
* And compile the source code (being present at the project home directory) before running as below,
```
$ sbt compile
```

### Running the code
At present, only way of running this sbt project is through sbt-console. However, it can be used as an unmanaged dependency after packaging it to a .jar file using ```sbt package``` command.

Following are the steps to run on the sbt-console;
* Move to the project directory.
* Lauch console as below from the project directory, as below
```
$ sbt console
```
* Then import the source code to the console as below and get going,
```
> import superficial._
```

## Documentation
Source code includes:
* a trait ```Generator``` with classes ```a(i)```, ```b(i)``` and ```s(i)``` (each indexed) extending it.
* a type alias ```Word``` which is ```Vector[Generator]```.
* a class ```OrientableSurface(g,r)``` which takes a integer argument ```g``` as the genus and another integer argument ```r``` which is the number of components in the boundary. Following are its notable methods:
     * ```red1(w: Word): Word``` -- performs cyclic reduction
     * ```reduce(w: Word): Word``` -- performs complete reduction
     * ```satisfyEquation(w: Word,0,0): Boolean``` -- checks division property (for correctness other arguments should be zero.)
     * ```isSimple(w: Word): Boolean``` -- checks if w is simple closed curve

## Demo and Results
Refer to [demo](superficial.ipynb) for some code demo and also some results involving computing approximate fraction of words of fixed length, which are homotopic to simple closed curve.

## Acknowledgments
* [Prof. Siddhartha Gadgil](https://github.com/siddhartha-gadgil)
* [Scala Standard Library](http://www.scala-lang.org/api/2.12.0/index.html)
