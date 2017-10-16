import visual.jpf.filters.{AvailableFilters, Filter}
import visual.jpf.parsing.{ParseHelper, TraceLine}

import scala.collection.mutable
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{ReadOnlyStringWrapper, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldTreeTableCell
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.text.Font

class ControlColumn extends TreeTableColumn[TraceLine, String]("View") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { _ => ReadOnlyStringWrapper("") }
}

class IdColumn extends TreeTableColumn[TraceLine, String]("ID") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper(p.value.value.value.id.toString) }
}

class ThreadColumn(val tid: Int, val preferredWidth: Double) extends TreeTableColumn[TraceLine, String]("Thread " + tid) {
  sortable = false
  editable = false
  minWidth = 100
  prefWidth = preferredWidth
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
  cellValueFactory = { p =>
    notes.getOrElse(p.value.value.value.id, new StringProperty("") {
      onChange((_, _, _) => notes += p.value.value.value.id -> this)
    })
  }

  cellValueFactory = { p =>
    notes.getOrElse(p.value.value.value.id, new StringProperty("") {
      onChange((_, _, _) => notes += p.value.value.value.id -> this)
    })
  }

  cellFactory = { _ =>
    new TextFieldTreeTableCell[TraceLine, String](scalafx.scene.control.TextFormatter.IdentityStringConverter) {
      editable = true
    }
  }
}

object GUIParameters {
  val SCENE_WIDTH = 1024
  val SCENE_HEIGHT = 768
  val BOTTOM_SIZE = 15
}

object Main extends JFXApp {

  var availableFilters = AvailableFilters.default

  if (parameters.raw.length == 2) {
    // <filepath> <filters_path>
    availableFilters = availableFilters.merge(AvailableFilters(parameters.raw(1)))
  } else if (parameters.raw.length != 1) {
    // filters_path -- empty
    println("Usage: tool <filepath> <filters_path>")
    println("       tool <filepath>")
    System.exit(-1)
  }

  val trace = ParseHelper.parseJPFTrace(parameters.raw.head)
  val numberOfThreads = trace.sortedLines.map(_.tid).toSet.size
  var filteredTrace = trace.withFilters(availableFilters.list: _*)

  val rootnode: TreeItem[TraceLine] = new TreeItem(TraceLine(0, -1, "", "")) {
    expanded = true
  }

  updateView(availableFilters.list: _*)

  val notes: mutable.Map[Int, StringProperty] = mutable.Map()

  val txt = new TextArea() {
    minWidth = GUIParameters.SCENE_WIDTH * 0.7
    font = Font.font("Consolas", 12)
  }

  val obs = ObservableBuffer(availableFilters.list.map(_.name))
  val appliedFiltersView = new ListView[String]() {
    selectionModel.value.setSelectionMode(SelectionMode.Multiple)
    items = obs
  }

  appliedFiltersView.selectionModel.value.selectAll()

  new MultipleSelectionModel(appliedFiltersView.selectionModel()) {
    selectedItems.onChange((selected, _) => {
      updateView(availableFilters.list.filter { f =>
        selected.contains(f.name)
      }: _*)
    })
  }

  stage = new PrimaryStage {
    title = "Visual JPF"
    scene = new Scene(GUIParameters.SCENE_WIDTH, GUIParameters.SCENE_HEIGHT) {
      root = new BorderPane {
        val table =
          new TreeTableView[TraceLine](rootnode) {
            editable = true
            columns += new ControlColumn
            columns += new IdColumn
            (0 until numberOfThreads).foreach {
              columns += new ThreadColumn(_, GUIParameters.SCENE_WIDTH / (numberOfThreads + 1))
            }
            columns += new NotesColumn(notes)
          }

        new TableSelectionModel(table.selectionModel()) {
          selectedItem.onChange((_, _, newValue) => {
            if (newValue != null && newValue.getValue != null) {
              val currentSelectedLine = newValue.value.value
              val threadSpecificTrace = filteredTrace.linesOfThread(currentSelectedLine.tid)

              // TODO refactor this hell
              var plus = true
              var anchorId = 0
              var text = ""

              val maxClassLength = (threadSpecificTrace.sortedLines.map(_.className.length) ++ Seq(0)).max

              def wrapSelected(l: TraceLine) = wrap(l, "-->", "<--")

              def wrapNonSelected(l: TraceLine) = wrap(l, "   ", "   ")

              def wrap(l: TraceLine, prefix: String, postfix: String) = {
                prefix + l.className.padTo(maxClassLength + 1, ' ').mkString("") + ": " + l.content + postfix + "\n"
              }

              for (l <- threadSpecificTrace.sortedLines) {
                if (l == currentSelectedLine) {
                  plus = false
                }

                val addition = if (l == currentSelectedLine) wrapSelected(l) else wrapNonSelected(l)
                text = text + addition

                if (plus) {
                  anchorId += addition.length
                }
              }

              txt.setText(text)
              txt.selectRange(anchorId, anchorId + wrapSelected(currentSelectedLine).length)
            }
          })
        }

        center = table

        bottom =
          new HBox {
            padding = Insets(GUIParameters.BOTTOM_SIZE)

            children.add(txt)
            children.add(appliedFiltersView)
          }
      }
    }
  }

  def group(trace: Traversable[TraceLine]): Seq[Seq[TraceLine]] = {
    if (trace.nonEmpty) {
      val (h, t) = trace.span(_.tid == trace.head.tid)
      h.toSeq +: group(t)
    } else {
      Seq()
    }
  }

  def updateView(filters: Filter*): Unit = {
    filteredTrace = trace.withFilters(filters: _*)
    rootnode.expanded = true
    rootnode.children = group(filteredTrace.sortedLines).map(g => {
      val n = new TreeItem(g.head) {
        if (g.tail.nonEmpty) {
          children = g.tail.map(new TreeItem(_))
          expanded = true
        }
      }
      n
    })
  }

  override def stopApp(): Unit = {
    notes foreach println
  }
}
