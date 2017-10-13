package visual.jpf.filters

import visual.jpf.parsing.TraceLine


trait Filter {
  def ignoreLine(t: TraceLine) : Boolean
}

object Filters {

  val ignoreBrackets = new Filter {
    override def ignoreLine(t: TraceLine): Boolean =
      t.content.trim == "}" || t.content.trim == "{"
  }

  def ignoreClass(name : String) = new Filter {
    override def ignoreLine(t: TraceLine): Boolean =
      t.className.startsWith(name)
  }
}
