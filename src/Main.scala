import visual.jpf.parsing.{Parser, Trace}

import scala.io.Source
import scala.reflect.io.File
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{ObjectProperty, ReadOnlyStringWrapper, StringProperty}
import scalafx.scene.Scene
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}

case class ThreadData(n: Int, s: StringProperty) {
  def this(_n: Int, _s: String) = this(_n, StringProperty(_s))
}

class ControlColumn extends TreeTableColumn[ThreadData, String]("View") {
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper("-")}
}

class ThreadColumn(val n: Int) extends TreeTableColumn[ThreadData, String]("Thread " + n) {
  editable = false
  minWidth = 100
  cellValueFactory = { p => ReadOnlyStringWrapper(if (n == p.value.value.value.n) p.value.value.value.s() else "")}
}

object ParseHelper {

  val tracePrefix = "====================================================== trace"
  val outputPrefix = "====================================================== output"

  def parseJPFTrace(path: String): Trace = {
    val buffer = collection.mutable.ArrayBuffer[String]()
    var buffering = false

    for (line <- Source.fromFile(path).getLines) {
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

  def trace2IR(trace: Trace): Seq[ThreadData] = {
    val result = collection.mutable.ArrayBuffer[ThreadData]()
    for (l <- trace.sortedLines()) {
      result += new ThreadData(l.tid, l.content)
    }
    result
  }
}

object Main extends JFXApp {

  if (parameters.raw.length != 1) {
    println("Usage: tool <filepath>")
    System.exit(-1)
  }

  val trace = ParseHelper.parseJPFTrace(parameters.raw.head)
  val data = ParseHelper.trace2IR(trace)

  def group(data: Seq[ThreadData]): Seq[Seq[ThreadData]] = {
    if (data.isEmpty) Seq()
    else {
      val (h, t) = data.span(_.n == data.head.n)
      h +: group(t)
    }
  }

  val rootnode = new TreeItem(new ThreadData(0, "all")) {
    expanded = true
//    children = data.map(new TreeItem(_))
    children = group(data).map(g => {
      val n = new TreeItem(g.head) {
        if (!g.tail.isEmpty) {
          children = g.tail.map(new TreeItem(_))
        }
      }
      n
    })
  }

  stage = new PrimaryStage {
    title = "Visual JPF"
    scene = new Scene(1024, 768) {
      root = new TreeTableView[ThreadData](rootnode) {
        columns ++= Seq(
          new ControlColumn,
          new ThreadColumn(0),
          new ThreadColumn(1),
          new ThreadColumn(2)
        )
      }
    }
  }
}
