package heap

import java.io.DataInputStream

/**
  * Created by mehmetgunturkun on 27/01/17.
  */
class HeapDumpRecord(val tag: HeapDumpRecordTag, val length: Int) {
  override def toString: String = s"HeapDumpRecord($tag, $length)"
}

object HeapDumpRecord {
  def apply(tag: HeapDumpRecordTag, idSize: Int, length: Int, stream: DataInputStream): HeapDumpRecord = {
    tag match {
      case StringTag => parseStringRecord(idSize, length, stream)
      case LoadClassTag => parseLoad(idSize, length, stream)
      case UnloadClassTag => parseUnloadClass(idSize, length, stream)
      case StackFrameTag => parseStackFrame(idSize, length, stream)
      case StackTraceTag => parseStackTrace(idSize, length, stream)
      case AllocSitesTag => parseDefaultRecord(tag, idSize, length, stream)
      case HeapSummaryTag => parseDefaultRecord(tag, idSize, length, stream)
      case StartThreadTag => parseDefaultRecord(tag, idSize, length, stream)
      case EndThreadTag => parseDefaultRecord(tag, idSize, length, stream)
      case HeapDumpTag => parseDefaultRecord(tag, idSize, length, stream)
      case HeapDumpSegmentTag => parseDefaultRecord(tag, idSize, length, stream)
      case HeapDumpEndTag => parseDefaultRecord(tag, idSize, length, stream)
      case default => parseDefaultRecord(tag, idSize, length, stream)
    }
  }

  private def consume(data: DataInputStream, length: Int): Unit = {
    val nrOfSlots: Int = length / 1000
    val remainingBytes = length - (nrOfSlots * 1000)

    val arr: Array[Byte] = Array.ofDim[Byte](1000)
    for (i <- 0 until nrOfSlots) {
      data.read(arr)
    }

    data.read(arr, 0, remainingBytes)
  }

  private def parseDefaultRecord(tag: HeapDumpRecordTag, idSize: Int, length: Int, stream: DataInputStream): HeapDumpRecord = {
    consume(stream, length)
    new HeapDumpRecord(tag, length)
  }

  private def parseStringRecord(idSize: Int, length: Int, stream: DataInputStream): StringRecord = {
    //TODO Assuming it as long for now. It should readInt or readLong according to idSize
    val stringId = stream.readLong()

    val contentLength = length - idSize
    val arr: Array[Byte] = Array.ofDim[Byte](contentLength)
    stream.read(arr)

    StringRecord(
      stringId = stringId,
      content = new String(arr),
      length = length)
  }

  private def parseLoad(idSize: Int, length: Int, data: DataInputStream): LoadClassRecord = {
    val classSerialNumber: Int = data.readInt()
    val classObjectId: Long = data.readLong()
    val stackTraceSerialNumber: Int = data.readInt()
    val classNameStringId: Long = data.readLong()

    LoadClassRecord(classSerialNumber, classObjectId, stackTraceSerialNumber, classNameStringId, length)
  }

  private def parseUnloadClass(idSize: Int, length: Int, data: DataInputStream): UnloadClassRecord = {
    val classSerialNumber: Int = data.readInt()
    UnloadClassRecord(classSerialNumber, length)
  }

  private def parseStackFrame(idSize: Int, length: Int, data: DataInputStream): StackFrameRecord = {
    val stackFrameId: Long = data.readLong()
    val methodNameStringId: Long = data.readLong()
    val methodSignatureStringId: Long = data.readLong()
    val sourceFileNameStringId: Long = data.readLong()
    val classSerialNumber: Int = data.readInt()
    val lineNumber: Int = data.readInt()

    StackFrameRecord(
      stackFrameId = stackFrameId,
      methodNameStringId = methodNameStringId,
      methodSignatureStringId = methodSignatureStringId,
      sourceFileNameStringId = sourceFileNameStringId,
      classSerialNumber = classSerialNumber,
      lineNumber = lineNumber,
      length = length
    )
  }

  private def parseStackTrace(idSize: Long, length: Int, data: DataInputStream): StackTraceRecord = {
    val stackTraceSerialNumber = data.readInt()
    val threadSerialNumber: Int = data.readInt()
    val nrOfFrames: Int = data.readInt()

    val stackFrames = Array.ofDim[Long](nrOfFrames)
    for (i <- 0 until nrOfFrames) {
      val frameId = data.readLong()
      stackFrames(i) = frameId
    }

    StackTraceRecord(
      stackTraceSerialNumber = stackTraceSerialNumber,
      threadSerialNumber = threadSerialNumber,
      nrOfFrames = nrOfFrames,
      stackTraceIds = stackFrames,
      length = length
    )
  }
}

case class StringRecord(stringId: Long,
                        content: String,
                        override val length: Int) extends HeapDumpRecord(StringTag, length)

case class LoadClassRecord(classSerialNumber: Int,
                           classObjectId: Long,
                           stackTraceSerialNumber: Int,
                           classNameStringId: Long,
                           override val length: Int) extends HeapDumpRecord(LoadClassTag, length)

case class UnloadClassRecord(classSerialNumber: Int,
                             override val length: Int) extends HeapDumpRecord(UnloadClassTag, length)

case class StackFrameRecord(stackFrameId: Long,
                            methodNameStringId: Long,
                            methodSignatureStringId: Long,
                            sourceFileNameStringId: Long,
                            classSerialNumber: Int,
                            lineNumber: Int,
                            override val length: Int) extends HeapDumpRecord(StackFrameTag, length)

case class StackTraceRecord(stackTraceSerialNumber: Int,
                            threadSerialNumber: Int,
                            nrOfFrames: Int,
                            stackTraceIds: Array[Long],
                            override val length: Int) extends HeapDumpRecord(StartThreadTag, length)

//case class ClassDump(classObjectId: Long,
//                     stackTraceSerialNumber: Int,
//                     superClassObjectId: Long,
//                     classLoaderObjectId: Long,
//                     signersObjectId: Long,
//                     protectionDomainObjectId: Long,
//                     reserved1: Long,
//                     reserved2: Long,
//                     instanceSize: Int)