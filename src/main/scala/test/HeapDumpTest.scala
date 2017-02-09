package test

import java.io.File

import heap.{HeapDump, HeapDumpRecordTag, StringRecord}

/**
  * Created by mehmetgunturkun on 22/01/17.
  */
object HeapDumpTest {
  def main(args: Array[String]): Unit = {
    val file = new File("/tmp/test2.dump")
    val maybeHeapDump: Option[HeapDump] = HeapDump(file)

    maybeHeapDump match {
      case Some(heapDump) =>
        val dumpRecordMap = collection.mutable.Map.empty[HeapDumpRecordTag, Int]

        while (heapDump.hasNext) {
          val record = heapDump.nextRecord()
          record match {
            case r =>
              val value = dumpRecordMap.getOrElse(r.tag, 0)
              dumpRecordMap.put(r.tag, value + 1)
              println(r)
          }
        }

        println(dumpRecordMap)

      case None =>
        println("There is no heap dump to parse")
    }
  }
}
