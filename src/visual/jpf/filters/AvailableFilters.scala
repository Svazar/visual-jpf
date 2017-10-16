package visual.jpf.filters

import scala.collection.mutable
import scala.io.Source

object AvailableFilters {

  def apply(path: String): AvailableFilters = new AvailableFilters(parseFilters(path))

  val empty = new AvailableFilters(Seq.empty)
  val default = new AvailableFilters(Seq(Filters.ignoreContentStartsWith("Ignore closed bracket", "}")))

  private def parseFilters(path: String): Seq[Filter] = {
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

    try {
      for (rawLine <- Source.fromFile(path).getLines if rawLine.nonEmpty) {
        val line = rawLine.trim
        if (line.startsWith("Name")) {
          processBlock()
        }
        currentBlock += line
      }

      processBlock()
    } catch {
      case _: Exception => return Seq.empty
    }

    filters
  }
}

case class AvailableFilters(list: Seq[Filter]) {
  def merge(other: AvailableFilters): AvailableFilters = AvailableFilters(this.list ++ other.list)
}