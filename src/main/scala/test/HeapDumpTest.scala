package test

import java.io.File

import heap.{HeapDump, StringRecord}

/**
  * Created by mehmetgunturkun on 22/01/17.
  */
object HeapDumpTest {
  def main(args: Array[String]): Unit = {
    val file = new File("/tmp/test2.dump")
    val maybeHeapDump: Option[HeapDump] = HeapDump(file)

    maybeHeapDump match {
      case Some(heapDump) =>
        val record = heapDump.nextRecord()
        record match {
          case strRec@StringRecord(tag, length) => println(strRec)
        }
      case None =>
        println("There is no heap dump to parse")
    }
  }
}
