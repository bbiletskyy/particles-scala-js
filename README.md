# Particles Scala.js application

This is a particles application written in
[Scala.js](http://www.scala-js.org/). In particular, it links
in libraries that are indispensible in being productive working with Scala.js.

## Get started

To get started, run `sbt ~fastOptJS` in this example project. This should
download dependencies and prepare the relevant javascript files. If you open
`localhost:12345/target/scala-2.11/classes/index-dev.html` in your browser, it will show you an animated [Sierpinski
triangle](http://en.wikipedia.org/wiki/Sierpinski_triangle) ([live demo](http://lihaoyi.github.io/workbench-example-app/triangle.html)). You can then
edit the application and see the updates be sent live to the browser
without needing to refresh the page.

## The optimized version

Run `sbt fullOptJS` and open up `index-opt.html` for an optimized (~200kb) version
of the final application, useful for final publication.
