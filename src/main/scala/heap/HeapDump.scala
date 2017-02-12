package heap

import heap.records._

/**
  * Created by mehmetgunturkun on 12/02/17.
  */
class HeapDump(val stream: HeapDumpStream) {

  loadCommonMaps()

  def hasNext = stream.hasNext

  private def loadCommonMaps(): Unit = {
    def iterateUntilHeapDump(f: HeapDumpRecord => Unit): Unit = {
      val record: HeapDumpRecord = nextInternalRecord()
      println(record)
      record.tag match {
        case HeapDumpStartTag => {}
        case other =>
          f(record)
          iterateUntilHeapDump(f)
      }
    }

    iterateUntilHeapDump { other => }
  }

  private def nextInternalRecord(): HeapDumpRecord = {
    val tagByte = stream.read()
    val tag: HeapDumpRecordTag = HeapDumpInternalRecordTag(tagByte)

    val ts: Int = stream.readInt()
    val length: Int = stream.readInt()

    HeapDumpRecord(tag, length, stream)
  }

  def nextRecord(): HeapDumpRecord = {
    val tagByte = stream.read()
    val tag: HeapDumpRecordTag = HeapDumpRecordTag(tagByte)

    val ts: Int = stream.readInt()
    val length: Int = stream.readInt()

    HeapDumpRecord(tag, length, stream)
  }

}
