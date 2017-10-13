package visual.jpf.parsing

object Parser {

  val transitionLinePrefix = "------------------------------------------------------ transition"

  private def isNotUsefulLine(l: String) : Boolean = {
    l.contains("insn w/o sources") ||
      l.startsWith("gov.nasa.jpf.vm.choice.ThreadChoiceFromSet") ||
        l.isEmpty
  }

  def parseTrace(lines: Traversable[String]) : Trace = {

    val map = collection.mutable.Map[Int, TraceLine]()
    var currentTid = 0
    var currentInstrNumber = 0
    var prevLine = ""

    for (line <- lines if !isNotUsefulLine(line) && prevLine != line) {
      if (line.startsWith(transitionLinePrefix)) {
        val rawInt = line.stripPrefix(transitionLinePrefix).split(":")(1)
        currentTid = Integer.parseInt(rawInt.trim)
      } else {
        assert (!map.contains(currentInstrNumber))

        line.split("  :").toList match {
          case className :: content :: Nil =>
            map.put(currentInstrNumber, TraceLine(currentInstrNumber, currentTid, className.trim, content.trim))
          case _ => assert (false)
        }

        currentInstrNumber += 1
        prevLine = line
      }
    }

    Trace(map.toMap)
  }
}
