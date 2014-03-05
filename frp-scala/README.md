scalawag
========

This is an implementation of a simple roguelike game in the spirit of
Steve Losh's
[Caves of Clojure](http://stevelosh.com/blog/2012/07/caves-of-clojure-01/).
The idea is to write a similarly-themed tutorial series that explores
various parts of the Scala library ecosystem while maintaining
functional purity as much as is practical.

## Running

Install `sbt` to manage project dependencies and the Scala compiler
itself for you (i.e. don't `apt-get install scala`). I found that the
[Manual Installation](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html)
works best so you're not tied to old versions of `sbt` in e.g.
Debian's repositories. This applies to other "languages as a library"
as well, such as Clojure.

Launch the game using `sbt run`.
