import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{ObjectProperty, ReadOnlyStringWrapper, StringProperty}
import scalafx.scene.Scene
import scalafx.scene.control.{TreeTableColumn, TreeTableView}

case class Data(s: StringProperty) {
  def this(_s: String) = this(StringProperty(_s))
}

class ThreadColumn extends TreeTableColumn[Data, String]("Thread") {
  editable = false
  minWidth = 100
  cellValueFactory = { p => ReadOnlyStringWrapper(p.value.value.value.s())}
}

object Main extends JFXApp {

  val data = Seq(
    new Data("a"),
    new Data("b"),
    new Data("c")
  )

  stage = new PrimaryStage {
    title = "Visual JPF"
    scene = new Scene(1024, 768) {
      root = new TreeTableView[Data] {
        columns ++= Seq(
          new ThreadColumn
        )
      }
    }
  }


}
