package heap.records

import heap.HeapDumpStream

/**
  * Created by mehmetgunturkun on 12/02/17.
  */
class HeapDumpRecord(val tag: HeapDumpRecordTag)

case class DefaultHeapDumpRecord(override val tag: HeapDumpRecordTag) extends HeapDumpRecord(tag)
case class StringRecord(id: Long, str: String) extends HeapDumpRecord(StringTag)
case class LoadClassRecord(classSerialNumber: Int, classObjectId: Long, stackTraceSerialNumber: Int, classNameStringId: Long) extends HeapDumpRecord(LoadClassTag)
case class UnloadClassRecord(classSerialNumber: Int) extends HeapDumpRecord(UnloadClassTag)
case class StackFrameRecord(stackFrameId: Long, methodNameStringId: Long, methodSignatureStringId: Long, sourceFileNameStringId: Long, classSerialNumber: Int, lineNumber: Int) extends HeapDumpRecord(StackFrameTag)
case class StackTraceRecord(stackTraceSerialNumber: Int, threadSerialNumber: Int, nrOfFrames: Int, stackFrameIds: Array[Long]) extends HeapDumpRecord(StackTraceTag)

object HeapDumpRecord {
  def apply(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    tag match {
      case any => parseDefaultRecord(any, length, stream)
    }
  }

  def parseDefaultRecord(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    stream.consume(length)
    new DefaultHeapDumpRecord(tag)
  }
}