package visual.jpf.parsing

case class TraceLine(id: Int, tid: Int, className: String, content: String)

class Trace(private val lines: Map[Int, TraceLine]) {

  def line(id: Int): Option[TraceLine] = lines.get(id)

  def linesOfThread(tid: Int): Traversable[TraceLine] = lines.filter {
    case (k: Int, v: TraceLine) => v.tid == tid
  }.values


  override def toString: String = lines.toList.sortWith( (a, b) => a._1 < b._1) mkString "\n"
}

object Trace {
  def apply(lines: Map[Int, TraceLine]): Trace = new Trace(lines)

  val empty = Trace(Map.empty)
}



