package visual.jpf.parsing

import visual.jpf.filters.Filter

case class TraceLine(id: Int, tid: Int, className: String, content: String)

class Trace(private val lines: Map[Int, TraceLine]) {

  def sortedLines : Traversable[TraceLine] = lines.toList.sortWith( (a, b) => a._1 < b._1).map( _._2)

  def line(id: Int): Option[TraceLine] = lines.get(id)

  def linesOfThread(tid: Int): Trace = this.withFilters(new Filter {
    override def name: String = ???
    override def ignoreLine(t: TraceLine): Boolean = t.tid != tid
  })


  def withFilters(seq: Filter*) = Trace(
    lines.filterNot {
      case (_, line) => seq.exists(_.ignoreLine(line))
    }
  )

  override def toString: String = sortedLines mkString "\n"
}

object Trace {
  def apply(lines: Map[Int, TraceLine]): Trace = new Trace(lines)
  def apply(lines: Traversable[TraceLine]): Trace = new Trace(lines.map(l => (l.id, l)).toMap)

  val empty = Trace(Map[Int, TraceLine]())
}



