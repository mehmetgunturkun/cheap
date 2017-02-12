package heap

import heap.records._
import scala.collection.mutable.{Map => MMap}

/**
  * Created by mehmetgunturkun on 12/02/17.
  */
class HeapDump(val stream: HeapDumpStream) {

  private val strings: MMap[Long, String] = MMap.empty
  private val classes: MMap[Long, Class] = MMap.empty

  loadCommonMaps()

  def hasNext = stream.hasNext

  private def loadCommonMaps(): Unit = {
    def iterateUntilHeapDump(f: HeapDumpRecord => Unit): Unit = {
      val record: HeapDumpRecord = nextInternalRecord()
      record.tag match {
        case HeapDumpStartTag => {}
        case other =>
          f(record)
          iterateUntilHeapDump(f)
      }
    }

    iterateUntilHeapDump {
      case StringRecord(id, content) =>
        strings(id) = content
      case LoadClassRecord(classSerialNumber: Int, classObjectId: Long, stackTraceSerialNumber: Int, classNameStringId: Long) =>
        val classNameString = strings(classNameStringId)
        val clazz = Class(classObjectId, classNameString)
        classes(classObjectId) = clazz
      case other =>
        //unrecognized tag - do nothing
    }
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
