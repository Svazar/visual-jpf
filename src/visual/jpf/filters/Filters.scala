package visual.jpf.filters

import visual.jpf.parsing.TraceLine

trait Filter {
  def name : String
  def ignoreLine(t: TraceLine) : Boolean

  override def toString: String = "Filter(" + name + ")"
}

object Filters {

  def ignoreContentStartsWith(nameStr : String, prefix: String) = new Filter {
    override def name: String = nameStr

    override def ignoreLine(t: TraceLine): Boolean =
      t.content.startsWith(prefix)
  }

  def ignoreClassStartsWith(nameStr : String, prefix: String) = new Filter {
    override def name: String = nameStr

    override def ignoreLine(t: TraceLine): Boolean =
      t.className.startsWith(prefix)
  }
}
