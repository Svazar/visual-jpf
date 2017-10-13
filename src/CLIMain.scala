import visual.jpf.parsing.{Parser, Trace}

import scala.io.Source

object CLIMain {

  def parseJPFTrace(path: String) : Trace = {
    val buffer = collection.mutable.ArrayBuffer[String]()
    var buffering = false

    for (line <- Source.fromFile(path).getLines) {
      if (line.startsWith("====================================================== trace")) {
        buffering = true
      } else if (buffering) {
        if (line.startsWith("====================================================== output")) {
          buffering = false
          return Parser.parseTrace(buffer)
        } else {
          buffer += line
        }
      }
    }

    Trace.empty
  }
  
  def example(path: String) : Seq[ThreadData] = {

    val result = collection.mutable.ArrayBuffer[ThreadData]()
    val trace = parseJPFTrace(path)
    for (l <- trace.sortedLines()) {
      result += new ThreadData(l.tid, l.content)
    }

    result
  }

  def main(args: Array[String]) : Unit = {
    if (args.length  != 1) {
      println("Usage: tool <filepath>")
      return
    }

    val trace = parseJPFTrace(args(0))
    println(trace)
  }

}
