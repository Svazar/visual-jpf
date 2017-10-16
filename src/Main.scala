import java.io.File

import visual.jpf.filters.{Filter, Filters}
import visual.jpf.parsing.{Parser, Trace, TraceLine}

import scala.collection.mutable
import scala.io.Source
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
import scalafx.stage.FileChooser

class ControlColumn extends TreeTableColumn[TraceLine, String]("View") {
  sortable = false
  editable = false
  minWidth = 50
  cellValueFactory = { p => ReadOnlyStringWrapper("") }
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
      onChange((a, b, c) => notes += p.value.value.value.id -> this)
    })
  }

  cellValueFactory = { p =>
    notes.getOrElse(p.value.value.value.id, new StringProperty("") {
      onChange((_, _, _) => notes += p.value.value.value.id -> this)
    })
  }

  cellFactory = { c =>
    new TextFieldTreeTableCell[TraceLine, String](scalafx.scene.control.TextFormatter.IdentityStringConverter) {
      editable = true
    }
  }
}

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

object GUIParameters {
  val SCENE_WIDTH = 1024
  val SCENE_HEIGHT = 768
  val BOTTOM_SIZE = 15
}

case class AvailableFilters(list: Seq[Filter]) {
  def merge(other: AvailableFilters): AvailableFilters = AvailableFilters(this.list ++ other.list)
}

object AvailableFilters {

  def apply(path: String): AvailableFilters = new AvailableFilters(parseFilters(path))

  val empty = new AvailableFilters(Seq.empty)
  val default = new AvailableFilters(Seq(Filters.ignoreContentStartsWith("Ignore closed bracket", "}")))

  private def parseFilters(path: String) = {
    val currentBlock = mutable.ArrayBuffer[String]()
    val filters = mutable.ArrayBuffer[Filter]()

    def processBlock() = if (currentBlock.nonEmpty) {
      val name = currentBlock(0).split("=")(1).trim
      val typeName = currentBlock(1).split("=")(1).trim
      val parametersRaw = currentBlock(2).split("=")(1).trim
      currentBlock.remove(0, currentBlock.size)
      assert(currentBlock.isEmpty)

      typeName match {
        case "FilterClassStartsWith" =>
          val newFilters = parametersRaw.replace("[", "").replace("]", "").split(",").map(
            p => Filters.ignoreClassStartsWith(name, p.replace("\"", ""))
          )
          filters ++= newFilters

        case "FilterContentStartsWith" =>
          val newFilters = parametersRaw.replace("[", "").replace("]", "").split(",").map(
            p => Filters.ignoreContentStartsWith(name, p.replace("\"", ""))
          )

          filters ++= newFilters
      }
    }

    for (rawLine <- Source.fromFile(path).getLines if rawLine.nonEmpty) {
      val line = rawLine.trim
      if (line.startsWith("Name")) {
        processBlock()
      }
      currentBlock += line
    }

    processBlock()

    filters
  }
}

object Main extends JFXApp {
  stage = new PrimaryStage {
    title = "Visual JPF"
  }

  val (traceFile: File, availableFilters: AvailableFilters) =
    if (parameters.raw.length > 2) {
      println("Usage: tool <filepath> <filters_path>")
      println("       tool <filepath>")
      println("       tool")
      System.exit(-1)
    } else if (parameters.raw.isEmpty) {
      val fc = new FileChooser() {
        title = "Select JPF trace file"
        initialDirectory = new File(System.getProperty("user.dir"))
      }
      val f = fc.showOpenDialog(stage)
      if (f == null) { System.exit(1) }
      (f, AvailableFilters.default)
    } else {
      (new File(parameters.raw.head),
        if (parameters.raw.length == 2) {
          AvailableFilters.default.merge(AvailableFilters(parameters.raw(1)))
        } else AvailableFilters.default
      )
    }

  val trace = ParseHelper.parseJPFTrace(traceFile)
  val numberOfThreads = trace.sortedLines.map(_.tid).toSet.size
  var filteredTrace = trace.withFilters(availableFilters.list: _*)

  val rootnode: TreeItem[TraceLine] = new TreeItem(TraceLine(0, -1, "", "")) {
    expanded = true
  }

  updateView(availableFilters.list: _*)

  def group(trace: Traversable[TraceLine]): Seq[Seq[TraceLine]] = {
    if (trace.isEmpty) Seq()
    else {
      val (h, t) = trace.span(_.tid == trace.head.tid)
      h.toSeq +: group(t)
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


  stage.scene = new Scene(GUIParameters.SCENE_WIDTH, GUIParameters.SCENE_HEIGHT) {
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

  override def stopApp(): Unit = {
    notes foreach println
  }
}
