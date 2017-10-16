package visual.jpf.parsing

import java.io.File

import scala.io.Source

object ParseHelper {

  val tracePrefix = "====================================================== trace"
  val outputPrefix = "====================================================== output"

  def parseJPFTrace(traceFile: File): Trace = {
    val buffer = collection.mutable.ArrayBuffer[String]()
    var buffering = false

    for (line <- Source.fromFile(traceFile).getLines) {
      if (line.startsWith(tracePrefix)) {
        buffering = true
      } else if (buffering) {
        if (line.startsWith(outputPrefix)) {
          buffering = false
          return Parser.parseTrace(buffer)
        } else {
          buffer += line
        }
      }
    }

    Trace.empty
  }
}
