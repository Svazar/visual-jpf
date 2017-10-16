import java.io.File

import visual.jpf.filters.{AvailableFilters, Filter}
import visual.jpf.parsing.{ParseHelper, Trace, TraceLine}
import visual.jpf.serialization.{SerializedTrace, Serializer}

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
import scalafx.stage.FileChooser

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
  cellFactory = { _ =>
    new TextFieldTreeTableCell[TraceLine, String](scalafx.scene.control.TextFormatter.IdentityStringConverter) {
      font = GUIParameters.ID_FONT
    }
  }
}

class ThreadColumn(val tid: Int, val preferredWidth: Double) extends TreeTableColumn[TraceLine, String]("Thread " + tid) {
  sortable = false
  editable = false
  minWidth = 100
  prefWidth = preferredWidth
  cellValueFactory = { p => ReadOnlyStringWrapper(if (tid == p.value.value.value.tid) p.value.value.value.content else "") }

  cellFactory = { _ =>
    new TextFieldTreeTableCell[TraceLine, String](scalafx.scene.control.TextFormatter.IdentityStringConverter) {
      font = GUIParameters.THREAD_CONTENT_FONT
    }
  }

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

  val ID_FONT: Font = Font.font("Consolas", 14)
  val THREAD_CONTENT_FONT: Font = Font.font("Consolas", 14)
  val THREAD_LOCAL_AREA_FONT: Font = Font.font("Consolas", 14)
}

object Main extends JFXApp {
  stage = new PrimaryStage {
    title = "Visual JPF"
  }

  val initialDirectory = new File(System.getProperty("user.dir"))

  val availableFilters = AvailableFilters.default
    .merge(AvailableFilters(initialDirectory + "/filters.ini"))

  val traceFile: File = parameters.raw.toList match {
    case path :: Nil => new File(path)

    case Nil =>
      val fc = new FileChooser() {
        title = "Select JPF trace file"
        initialDirectory = new File(System.getProperty("user.dir"))
      }
      val f = fc.showOpenDialog(stage)
      if (f == null) {
        System.exit(1)
      }
      f

    case _ =>
      println(
        """
          |Usage:
          |  tool <filepath>
          |  tool
        """.stripMargin
      )
      System.exit(-1)
      null
  }

  val sTrace = parseSerialTrace(traceFile)
    .orElse(parsePlainTrace(traceFile))
    .get


  val trace = Trace(sTrace.trace)
  val numberOfThreads = trace.sortedLines.map(_.tid).toSet.size
  var filteredTrace = trace.withFilters(availableFilters.list: _*)

  val rootnode: TreeItem[TraceLine] = new TreeItem(TraceLine(0, -1, "", "")) {
    expanded = true
  }

  updateView(availableFilters.list: _*)

  val notes: mutable.Map[Int, StringProperty] = mutable.Map()
  sTrace.notes.foreach(e => notes.put(e._1, new StringProperty(e._2)))

  val txt = new TextArea() {
    minWidth = GUIParameters.SCENE_WIDTH * 0.7
    font = GUIParameters.THREAD_LOCAL_AREA_FONT
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

  def parseSerialTrace(path: File): Option[SerializedTrace] = {
    try {
      Some(Serializer.load(path))
    } catch {
      case e: Exception => None
    }
  }

  def parsePlainTrace(path: File): Option[SerializedTrace] = Some(
    SerializedTrace(
      ParseHelper.parseJPFTrace(path).sortedLines,
      Map.empty))

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
    val fc = new FileChooser() {
      title = "Select save file"
      initialDirectory = new File(System.getProperty("user.dir"))
    }
    val f = fc.showOpenDialog(stage)
    if (f != null) {
      Serializer.save(f, SerializedTrace(
        trace.sortedLines,
        notes.map(e => (e._1, e._2.value)).toMap))
    }
  }
}
