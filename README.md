# LaGioconda

---

## How to try

This software requires Java as SBT. To install `sbt` follow instruction at http://www.scala-sbt.org/0.13/docs/Setup.html.

If you use OSX/MacOs, you can use HomeBrew:

```brew install sbt```

Then at root folder type

```sbt run```

Open a browser at http://localhost:9000. When the page is downloaded, algorithm starts.


## Getting Started

Run each line in a new terminal.

```
sbt "backend/run 2551"
sbt "backend/run 2552"
sbt "project frontend" run
