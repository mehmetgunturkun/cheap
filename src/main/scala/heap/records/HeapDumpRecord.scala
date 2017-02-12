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

case object HeapDumpStartRecord extends HeapDumpRecord(HeapDumpStartTag)

case class ClassDumpRecord() extends HeapDumpRecord(ClassDump)

object HeapDumpRecord {
  def apply(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    tag match {
      case StringTag => parseStringRecord(length, stream)
      case LoadClassTag => parseLoad(length, stream)
      case HeapDumpStartTag => HeapDumpStartRecord
      case any => parseDefaultRecord(any, length, stream)
    }
  }

  def parseDefaultRecord(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    stream.consume(length)
    new DefaultHeapDumpRecord(tag)
  }

  private def parseStringRecord(length: Int, stream: HeapDumpStream): StringRecord = {
    val stringId: Long = stream.readId()

    val contentLength = length - stream.idSize
    val arr: Array[Byte] = stream.read(contentLength)

    StringRecord(
      id = stringId,
      str = new String(arr)
    )
  }

  private def parseLoad(length: Int, data: HeapDumpStream): LoadClassRecord = {
    val classSerialNumber: Int = data.readInt()
    val classObjectId: Long = data.readId()
    val stackTraceSerialNumber: Int = data.readInt()
    val classNameStringId: Long = data.readId()

    LoadClassRecord(classSerialNumber, classObjectId, stackTraceSerialNumber, classNameStringId)
  }

  def parseHeapDumpStartRecord(): HeapDumpStartRecord.type  = {
    // No additional parsing is needed.
    HeapDumpStartRecord
  }
}