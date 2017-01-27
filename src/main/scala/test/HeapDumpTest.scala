package test

import java.io.File

import heap.{HeapDump, Record, StringRecord}

import scala.collection.mutable.Map
/**
  * Created by mehmetgunturkun on 22/01/17.
  */
object HeapDumpTest {
  def main(args: Array[String]): Unit = {
    val file = new File("/tmp/test2.dump")
    val maybeHeapDump: Option[HeapDump] = HeapDump(file)

    val tagToOccurence = Map.empty[Byte, Int]

    maybeHeapDump match {
      case Some(heapDump) =>
        while (heapDump.hasNext()) {
          val record = heapDump.nextRecord()
          record match {
            case StringRecord(id, content, length) =>
            case record if record.tag == 0x0c =>
              println(record)
              return 0
            case _ =>
              val occurence = tagToOccurence.getOrElse(record.tag, 0)
              tagToOccurence.put(record.tag, occurence + 1)
          }
        }
      case None =>
        println("There is no heap dump to parse")
    }

    tagToOccurence.foreach(println)

  }
}

