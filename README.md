This repo is just meant to show some basic macros, with the full setup of how to use them.

These examples go along with this blog post <http://imranrashid.com/posts/learning-scala-macros/>

It uses scala 2.10.2 + the macro-paradise plugin, along with sbt 0.13.0

Run with `sbt/sbt "project macrotests" test`


The macros are defined in the `macros` project, and then they are tested in the `macrotests` project.  They are
in separate project because macros must be compiled in a separate compilation from their usage.  The examples
show :

* basic creation & usage of macros
* using macro annotations to modify class definitions
* using quasi-quotes to simplify macro definitions.
* using reflection to find methods of a trait
