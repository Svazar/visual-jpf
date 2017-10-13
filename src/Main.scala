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

object Main extends JFXApp {

  val data = Seq(
    new ThreadData(0, "t0-0"),
    new ThreadData(0, "t0-1"),
    new ThreadData(1, "t1-0")
  )

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
