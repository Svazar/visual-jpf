import visual.jpf.filters.Filters
import visual.jpf.parsing.{Parser, Trace, TraceLine}

import scala.collection.mutable
import scala.io.Source
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{ReadOnlyStringWrapper, StringProperty}
import scalafx.event.ActionEvent
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldTreeTableCell
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

class ControlColumn extends TreeTableColumn[TraceLine, String]("View") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper("-") }
}

class IdColumn extends TreeTableColumn[TraceLine, String]("ID") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper(p.value.value.value.id.toString) }
}

class ThreadColumn(val tid: Int) extends TreeTableColumn[TraceLine, String]("Thread " + tid) {
  sortable = false
  editable = false
  minWidth = 100
  cellValueFactory = { p => ReadOnlyStringWrapper(if (tid == p.value.value.value.tid) p.value.value.value.content else "") }

  // Click an header must collapse/restore thread columns.
  // To implement this, we remove the default header text, create a new VBox with a Label,
  // and set it as custom grpahic
  var storedWidth: Option[Double] = None
  private def toggle(): Unit = {
    storedWidth match {
      case Some(w) =>
        storedWidth = None
        maxWidth = Double.MaxValue
        minWidth = w
        minWidth = 100
        label.text = "Thread " + tid
      case None =>
        storedWidth = Some(width.value)
        minWidth = 30
        maxWidth = 30
        label.text = "" + tid
    }
  }
  val label = new Label("Thread " + tid)
  val header = new VBox {
    alignment = Pos.Center
    children += label
    onMouseClicked = (me: MouseEvent) => {
      if (me.button == MouseButton.Primary) toggle
    }
  }
  text = ""
  graphic = header
}

class NotesColumn(val notes: mutable.Map[Int, StringProperty]) extends TreeTableColumn[TraceLine, String]("Notes") {
  sortable = false
  editable = true
  minWidth = 200
  cellValueFactory = { p => notes.getOrElse(p.value.value.value.id, new StringProperty("") {onChange((_, _, _) => notes += p.value.value.value.id -> this )}) }
  cellFactory = { c => new TextFieldTreeTableCell[TraceLine, String](scalafx.scene.control.TextFormatter.IdentityStringConverter) { editable = true } }
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
  val filteredTrace = trace.withFilters(filters: _*)
  val traceLines = filteredTrace.sortedLines

  def numberOfThreads(trace: Traversable[TraceLine]): Int =
    trace.map(_.tid).max + 1

  def group(trace: Traversable[TraceLine]): Seq[Seq[TraceLine]] = {
    if (trace.isEmpty) Seq()
    else {
      val (h, t) = trace.span(_.tid == trace.head.tid)
      h.toSeq +: group(t)
    }
  }

  val rootnode: TreeItem[TraceLine] = new TreeItem(TraceLine(0, -1, "", "")) {
    expanded = true
    children = group(traceLines).map(g => {
      val n = new TreeItem(g.head) {
        if (g.tail.nonEmpty) {
          children = g.tail.map(new TreeItem(_))
          expanded = true
        }
      }
      n
    })
  }

  val notes: mutable.Map[Int, StringProperty] = mutable.Map()

  val txt = new TextArea()

  stage = new PrimaryStage {
    title = "Visual JPF"
    scene = new Scene(1024, 768) {
      root = new BorderPane {
        val table =
          new TreeTableView[TraceLine](rootnode) {
            editable = true
            columns += new ControlColumn
            columns += new IdColumn
            (0 until numberOfThreads(traceLines)).foreach { columns += new ThreadColumn(_) }
            columns += new NotesColumn(notes)
          }

        new TableSelectionModel(table.selectionModel()) {
          selectedItem.onChange((_,_, newValue) => {
            val tid = newValue.value.value.tid
            println(tid)
            val specific = filteredTrace.linesOfThread(tid)

          })
        }

/*
        table.selectionModel().getSelectedCells.addListener(new ListChangeListener[control.TreeTablePosition[TraceLine, _]] {
          override def onChanged(chgs: ListChangeListener.Change[_ <: control.TreeTablePosition[TraceLine, _]]): Unit = {
            while (chgs.next()) {
              // TODO: refactor this hell
              if (chgs.wasAdded) {
                val lineId = chgs.getAddedSubList.get(0)
                val line = lineId.getTreeItem.getValue

                val specific = filteredTrace.linesOfThread(line.tid)

                var plus = true
                var anchorId = 0
                var text = ""

                for (l <- specific.sortedLines) {
                  text = text + l.content + "\n"

                  if (l == line) {
                    plus = false
                  }

                  if (plus) {
                    anchorId += (l.content + "\n").length
                  }
                }

                txt.setText(text)
                txt.selectRange(anchorId, anchorId + line.content.length)
              }
            }
          }
        })
*/
        center = table

        bottom =
          new HBox {
            padding = Insets(15)

            children.add(txt)

            (0 until numberOfThreads(traceLines)).foreach { t =>
              val btn = new Button("Thread-" + t) {
                onAction = (event: ActionEvent) => {
                  txt.setText(trace
                    .linesOfThread(t)
                    .sortedLines
                    .map(_.content) mkString "\n")
                }
              }

              children.add(btn)
            }

          }
      }
    }
  }
  override def stopApp(): Unit = {
    notes foreach println
  }
}
