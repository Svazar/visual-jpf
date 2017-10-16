package visual.jpf.serialization

import java.io._

import visual.jpf.parsing.TraceLine

import scala.collection.mutable

@SerialVersionUID(1L)
class SerializedTraceLine(var id: Int, var tid: Int, var className: String, var content: String, var comment: String)
  extends Serializable {
}

case class SerializedTrace(trace: Traversable[TraceLine], notes: mutable.Map[Int, String])

object Serializer {

  def save(path: String, trace: SerializedTrace): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    try {
      for (line <- trace.trace) {
        oos.writeObject(new SerializedTraceLine(
          line.id,
          line.tid,
          line.className,
          line.content,
          trace.notes.getOrElse(line.id, "")))
      }
    } finally {
      oos.close()
    }
  }

  def load(path: String): SerializedTrace = {
    val traceLines = collection.mutable.ArrayBuffer[TraceLine]()
    val notes = collection.mutable.Map[Int, String]()
    val ois = new ObjectInputStream(new FileInputStream(path))

    try {
      while (true) {
        val line = ois.readObject.asInstanceOf[SerializedTraceLine]
        traceLines += TraceLine(line.id, line.tid, line.className, line.content)
        notes.put(line.id, line.comment)
      }
    } catch {
      case _: EOFException =>
    } finally {
      ois.close()
    }

    SerializedTrace(traceLines, notes)
  }
}
