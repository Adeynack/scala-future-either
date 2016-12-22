Future of Either Extensions
===

The idea is to extends the existing base API of Scala to make it interesting to write
code chaining `Future[Either[A, B]]` without having to resolve to using complex
libraries like _Cats_ or _ScalaZ_.

The proposed extensions are made to map constantly on a success of `Future` of a _success_ (`Right`) of `Either`.

They are:

| Feature \ Extension method  | rightMap | rightFlatMap | rightMapWith | rightFlatMapWith | rightMapWithPF | rightFlatMapWithPF |
|-----------------------------|----------|--------------|--------------|------------------|----------------|--------------------|
| Can be changed to a `Left`  |          |              | X            | X                | X              | X                  |
| Value of `Right` can change | X        | X            | X            | X                | X              | X                  |
| Type of `Right` can change  | X        | X            | X            | X                |                |                    |
| Value returned as Future    |          | X            |              | X                |                | X                  |

