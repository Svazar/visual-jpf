import visual.jpf.filters.Filters
import visual.jpf.parsing.{Trace, TraceLine}
import visual.jpf.parsing.{Parser, Trace}

import scala.io.Source
import scala.reflect.io.File
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{ObjectProperty, ReadOnlyIntegerWrapper, ReadOnlyStringWrapper, StringProperty}
import scalafx.scene.Scene
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}

class ControlColumn extends TreeTableColumn[TraceLine, String]("View") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper("-")}
}

class IdColumn extends TreeTableColumn[TraceLine, String]("ID") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper(p.value.value.value.id.toString)}
}

class ThreadColumn(val tid: Int) extends TreeTableColumn[TraceLine, String]("Thread " + tid) {
  sortable = false
  editable = false
  minWidth = 100
  cellValueFactory = { p => ReadOnlyStringWrapper(if (tid == p.value.value.value.tid) p.value.value.value.content else "")}
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
}

object Main extends JFXApp {
  if (parameters.raw.length != 1) {
    println("Usage: tool <filepath>")
    System.exit(-1)
  }

  val filters = Seq(Filters.ignoreBrackets)

  val trace = ParseHelper.parseJPFTrace(parameters.raw.head)
    .withFilters(filters)
    .sortedLines

  def numberOfThreads(trace: Traversable[TraceLine]): Int =
    trace.map(_.tid).max + 1

  def group(trace: Traversable[TraceLine]): Seq[Seq[TraceLine]] = {
    if (trace.isEmpty) Seq()
    else {
      val (h, t) = trace.span(_.tid == trace.head.tid)
      h.toSeq +: group(t)
    }
  }

  val rootnode: TreeItem[TraceLine] = new TreeItem(new TraceLine(0, -1, "", "")) {
    expanded = true
    children = group(trace).map(g => {
      val n = new TreeItem(g.head) {
        if (g.tail.nonEmpty) {
          children = g.tail.map(new TreeItem(_))
          expanded = true
        }
      }
      n
    })
  }

  stage = new PrimaryStage {
    title = "Visual JPF"
    scene = new Scene(1024, 768) {
      root = new TreeTableView[TraceLine](rootnode) {
        columns += new ControlColumn
        columns += new IdColumn
        (0 to numberOfThreads(trace) - 1).foreach{ columns += new ThreadColumn(_) }
      }
    }
  }
}
