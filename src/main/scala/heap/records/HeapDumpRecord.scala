package heap.records

import heap.core.{BasicType, HeapDumpStream, LongType}
import heap.core._

/**
  * Created by mehmetgunturkun on 12/02/17.
  */
class HeapDumpRecord(val tag: AbstractHeapDumpRecordTag)

case class DefaultHeapDumpRecord(override val tag: AbstractHeapDumpRecordTag) extends HeapDumpRecord(tag)

case class StringRecord(id: Long,
                        str: String) extends HeapDumpRecord(StringTag)

case class LoadClassRecord(classSerialNumber: Int,
                           classObjectId: Long,
                           stackTraceSerialNumber: Int,
                           classNameStringId: Long) extends HeapDumpRecord(LoadClassTag)

case class UnloadClassRecord(classSerialNumber: Int,
                             classObjectId: Long,
                             stackTraceSerialNumber: Int,
                             classNameStringId: Long) extends HeapDumpRecord(UnloadClassTag)

case class StackFrameRecord(stackFrameId: Long,
                            methodNameStringId: Long,
                            methodSignatureStringId: Long,
                            sourceFileNameStringId: Long,
                            classSerialNumber: Int,
                            lineNumber: Int) extends HeapDumpRecord(StackFrameTag)

case class StackTraceRecord(stackTraceSerialNumber: Int,
                            threadSerialNumber: Int,
                            nrOfFrames: Int,
                            stackFrameIds: Array[Long]) extends HeapDumpRecord(StackTraceTag)

case class StartThreadRecord(threadSerialNumber: Int,
                             threadObjectId: Long,
                             stackTraceSerialNumber: Int,
                             threadNameStringId: Long,
                             threadGroupNameStingId: Long,
                             threadParentGroupNameStringId: Long) extends HeapDumpRecord(StartThreadTag)

case class EndThreadRecord(threadSerialNumber: Int) extends HeapDumpRecord(EndThreadTag)

case object HeapDumpStartRecord extends HeapDumpRecord(HeapDumpStartTag)

case class RootUnknownRecord(objectId: Long) extends HeapDumpRecord(RootUnknown)

case class RootJniGlobalRecord(objectId: Long,
                               jniGlobalRefId: Long) extends  HeapDumpRecord(RootJniGlobal)

case class RootJavaFrameRecord(threadObjectId: Long,
                               threadSerialNumber: Int,
                               stackTraceSerialNumber: Int) extends HeapDumpRecord(RootJavaFrame)

case class RootNativeStackRecord(objectId: Long,
                                 threadSerialNumber: Int) extends HeapDumpRecord(RootNativeStack)

case class RootStickyClassRecord(objectId: Long) extends HeapDumpRecord(RootStickyClass)

case class RootThreadBlockRecord(objectId: Long,
                                 threadSerialNumber: Int) extends HeapDumpRecord(RootThreadBlock)

case class RootMonitorUsedRecord(objectId: Long) extends HeapDumpRecord(RootMonitorUsed)

case class RootThreadObjectRecord(threadObjectId: Long,
                                  threadSerialNumber: Int,
                                  stackTraceSerialNumber: Int) extends HeapDumpRecord(RootThreadObject)

case class ClassDumpRecord(classObjectId: Long,
                           stackTraceSerialNumber: Int,
                           superClassObjectId: Long,
                           classLoaderObjectId: Long,
                           signersObjectId: Long,
                           protectionDomainObjectId: Long,
                           reserved1: Long,
                           reserved2: Long,
                           instanceSize: Int) extends HeapDumpRecord(ClassDump)

case class InstanceDumpRecord(objectId: Long,
                              threadSerialNumber: Int,
                              classObjectId: Long) extends HeapDumpRecord(InstanceDump)

case class PrimitiveArrayRecord(arrayObjectId: Long,
                                stackTraceSerialNumber: Int,
                                nrOfElements: Int,
                                elementType: BasicType) extends HeapDumpRecord(PrimitiveArrayDump)

case class ObjectArrayRecord(arrayObjectId: Long, stackTraceSerialNumber: Int, nrOfElements: Int, arrayClassObjectId: Long) extends HeapDumpRecord(ObjectArrayDump)


object HeapDumpRecord {
  def apply(tag: HeapDumpInternalRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    println("Inernal Tag: " + tag)
    tag match {
      case StringTag => parseStringRecord(length, stream)
      case LoadClassTag => parseLoad(length, stream)
      case UnloadClassTag => parseUnload(stream)
      case StackFrameTag => parseStackFrame(stream)
      case StackTraceTag => parseStackTrace(stream)
      case StartThreadTag => parseStartThread(stream)
      case EndThreadTag => parseEndThread(stream)
      case HeapDumpStartTag => HeapDumpStartRecord
      case any => throw new Exception(s"Unrecognized tag: $any")
    }
  }

  def apply(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    println("Tag: " + tag)
    tag match {
      case RootUnknown => parseRootUnknown(stream)
      case RootJniGlobal => parseRootJniGlobal(stream)
      case RootJavaFrame => parseRootJavaFrame(stream)
      case RootNativeStack => parseRootNativeStack(stream)
      case RootStickyClass => parseRootStickyClass(stream)
      case RootThreadBlock => parseRootThreadBlock(stream)
      case RootMonitorUsed => parseRootMonitorUsed(stream)
      case RootThreadObject => parseRootThreadObject(stream)
      case ClassDump => parseClassDump(length, stream)
      case InstanceDump => parseInstanceDump(stream)
      case PrimitiveArrayDump => parsePrimitiveArrayDump(stream)
      case ObjectArrayDump => parseObjectArrayDump(stream)
      case any => throw new Exception(s"Unrecognized tag: $any")
    }
  }

  def parseDefaultRecord(tag: AbstractHeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
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

  private def parseUnload(data: HeapDumpStream): UnloadClassRecord = {
    val classSerialNumber: Int = data.readInt()
    val classObjectId: Long = data.readId()
    val stackTraceSerialNumber: Int = data.readInt()
    val classNameStringId: Long = data.readId()

    UnloadClassRecord(
      classSerialNumber = classSerialNumber,
      classObjectId = classObjectId,
      stackTraceSerialNumber = stackTraceSerialNumber,
      classNameStringId = classNameStringId
    )
  }

  private def parseStackFrame(data: HeapDumpStream): StackFrameRecord = {
    val stackFrameId: Long = data.readId()
    val methodNameStringId: Long = data.readId()
    val methodSignatureStringId: Long = data.readId()
    val sourceFileNameStringId: Long = data.readId()
    val classSerialNumber: Int = data.readInt()

    val lineNumber: Int = data.readInt()

    StackFrameRecord(
      stackFrameId = stackFrameId,
      methodNameStringId = methodNameStringId,
      methodSignatureStringId = methodSignatureStringId,
      sourceFileNameStringId = sourceFileNameStringId,
      classSerialNumber = classSerialNumber,
      lineNumber = lineNumber
    )
  }

  private def parseStackTrace(data: HeapDumpStream): StackTraceRecord = {
    val stackTraceSerialNumber: Int = data.readInt()
    val threadSerialNumber: Int = data.readInt()
    val nrOfFrames = data.readInt()

    val frameIds = Array.ofDim[Long](nrOfFrames)
    for (i <- 0 until nrOfFrames) {
      val frameId = data.readId()
      frameIds(i) = frameId
    }

    StackTraceRecord(
      stackTraceSerialNumber = stackTraceSerialNumber,
      threadSerialNumber = threadSerialNumber,
      nrOfFrames = nrOfFrames,
      stackFrameIds = frameIds
    )
  }

  private def parseStartThread(data: HeapDumpStream): StartThreadRecord = {
    val threadSerialNumber: Int = data.readInt()
    val threadObjectId = data.readLong()
    val stackTraceSerialNumber = data.readInt()

    val threadNameStringId = data.readLong()
    val threadGroupNameStringId = data.readLong()
    val threadParentGroupNameStringId = data.readLong()

    StartThreadRecord(
      threadSerialNumber = threadSerialNumber,
      threadObjectId = threadObjectId,
      stackTraceSerialNumber = stackTraceSerialNumber,
      threadNameStringId = threadNameStringId,
      threadGroupNameStingId = threadGroupNameStringId,
      threadParentGroupNameStringId = threadParentGroupNameStringId
    )
  }

  private def parseEndThread(data: HeapDumpStream): EndThreadRecord = {
    val threadSerialNumber: Int = data.readInt()
    EndThreadRecord(threadSerialNumber = threadSerialNumber)
  }

  // Heap Records
  private def parseRootUnknown(data: HeapDumpStream): RootUnknownRecord = {
    val objectId: Long = data.readId()
    RootUnknownRecord(objectId = objectId)
  }

  private def parseRootJniGlobal(data: HeapDumpStream): RootJniGlobalRecord = {
    val objectId: Long = data.readId()
    val jniGlobalRefId: Long = data.readId()

    RootJniGlobalRecord(
      objectId = objectId,
      jniGlobalRefId = jniGlobalRefId
    )
  }

  private def parseRootJavaFrame(data: HeapDumpStream): RootJavaFrameRecord = {
    val threadObjectId: Long = data.readId()

    val threadSerialNumber: Int = data.readInt()
    val stackTraceSerialNumber: Int = data.readInt()

    RootJavaFrameRecord(
      threadObjectId = threadObjectId,
      threadSerialNumber = threadSerialNumber,
      stackTraceSerialNumber = stackTraceSerialNumber
    )
  }

  private def parseRootNativeStack(data: HeapDumpStream): RootNativeStackRecord = {
    val objectId: Long = data.readId()
    val threadSerialNumber: Int = data.readInt()

    RootNativeStackRecord(
      objectId = objectId,
      threadSerialNumber = threadSerialNumber
    )
  }

  private def parseRootStickyClass(data: HeapDumpStream): RootStickyClassRecord = {
    val objectId: Long = data.readId()
    RootStickyClassRecord(objectId = objectId)
  }

  private def parseRootThreadBlock(data: HeapDumpStream): RootThreadBlockRecord = {
    val objectId: Long = data.readId()
    val threadSerialNumber: Int = data.readInt()

    RootThreadBlockRecord(
      objectId = objectId,
      threadSerialNumber = threadSerialNumber
    )
  }

  private def parseRootMonitorUsed(data: HeapDumpStream): RootMonitorUsedRecord = {
    val objectId: Long = data.readId()
    RootMonitorUsedRecord(objectId = objectId)
  }

  private def parseRootThreadObject(data: HeapDumpStream): RootThreadObjectRecord = {
    val threadObjectId: Long = data.readId()
    val threadSerialNumber: Int = data.readInt()
    val stackTraceSerialNumber: Int = data.readInt()

    RootThreadObjectRecord(
      threadObjectId = threadObjectId,
      threadSerialNumber = threadSerialNumber,
      stackTraceSerialNumber = stackTraceSerialNumber
    )
  }

  private def parseClassDump(length: Int, data: HeapDumpStream): ClassDumpRecord = {
    val classObjectId: Long = data.readId()

    val stackTraceSerialNumber: Int = data.readInt()

    val superClassObjectId: Long = data.readId()
    val classLoaderObjectId: Long = data.readId()
    val signersObjectId: Long = data.readId()

    val protectionDomainObjectId: Long = data.readId()

    val reserved1: Long = data.readId()
    val reserved2: Long = data.readId()

    val instancesSizeInBytes: Int = data.readInt()

    val nrOfConstants: Short = data.readShort()
    for (i <- 0 until nrOfConstants) {
      val constantPoolIndex = data.readShort()

      val dataType: Byte = data.read()
      val basicType = BasicType(dataType)

      basicType match {
        case ObjectType =>
          val objectId = data.readId()
        case BooleanType =>
          val value: Boolean = data.readBoolean()
        case CharType =>
          val value = data.readChar()
        case FloatType =>
          val value = data.readFloat()
        case DoubleType =>
          val value = data.readDouble()
        case ByteType =>
          val value = data.read()
        case ShortType =>
          val value = data.readShort()
        case IntType =>
          val value = data.readInt()
        case LongType =>
          val value = data.readLong()
      }
    }

    val nrOfStaticInstances: Short = data.readShort()
    for (i <- 0 until nrOfStaticInstances) {
      val fieldNameStringId = data.readId()

      val dataType: Byte = data.read()
      val basicType = BasicType(dataType)

      basicType match {
        case ObjectType =>
          val objectId = data.readId()
        case BooleanType =>
          val value: Boolean = data.readBoolean()
        case CharType =>
          val value = data.readChar()
        case FloatType =>
          val value = data.readFloat()
        case DoubleType =>
          val value = data.readDouble()
        case ByteType =>
          val value = data.read()
        case ShortType =>
          val value = data.readShort()
        case IntType =>
          val value = data.readInt()
        case LongType =>
          val value = data.readLong()
      }
    }

    val nrOfInstances: Short = data.readShort()
    for (i <- 0 until nrOfInstances) {
      val fieldNameStringId = data.readId()

      val dataType = data.read()
      val basicType = BasicType(dataType)
    }

    ClassDumpRecord(
      classObjectId = classObjectId,
      stackTraceSerialNumber = stackTraceSerialNumber,
      superClassObjectId = superClassObjectId,
      classLoaderObjectId = classLoaderObjectId,
      signersObjectId = signersObjectId,
      protectionDomainObjectId = protectionDomainObjectId,
      reserved1 = reserved1,
      reserved2 = reserved2,
      instanceSize = instancesSizeInBytes
    )
  }

  private def parseInstanceDump(data: HeapDumpStream): InstanceDumpRecord = {
    val objectId = data.readId()
    val stackTraceSerialNumber: Int = data.readInt()
    val classObjectId: Long = data.readId()

    // TODO How to bind this data to class
    val nrOfBytes: Int = data.readInt()
    val arr = data.read(nrOfBytes)
    InstanceDumpRecord(objectId, stackTraceSerialNumber, classObjectId)
  }

  private def parsePrimitiveArrayDump(data: HeapDumpStream): PrimitiveArrayRecord = {
    val arrayObjectId: Long = data.readId()
    val stackTraceSerialNumber: Int = data.readInt()

    val nrOfElements: Int = data.readInt()
    val typeByte = data.read()
    val basicType = BasicType(typeByte)

    for (i <- 0 until nrOfElements) {
      basicType match {
        case ObjectType =>
          val objectId = data.readId()
        case BooleanType =>
          val value: Boolean = data.readBoolean()
        case CharType =>
          val value = data.readChar()
        case FloatType =>
          val value = data.readFloat()
        case DoubleType =>
          val value = data.readDouble()
        case ByteType =>
          val value = data.read()
        case ShortType =>
          val value = data.readShort()
        case IntType =>
          val value = data.readInt()
        case LongType =>
          val value = data.readLong()
      }
    }

    PrimitiveArrayRecord(arrayObjectId, stackTraceSerialNumber, nrOfElements, basicType)
  }

  private def parseObjectArrayDump(data: HeapDumpStream): ObjectArrayRecord = {
    val arrayObjectId: Long = data.readId()

    val stackTraceSerialNumber: Int = data.readInt()
    val nrOfElements: Int = data.readInt()

    val arrayClassObjectId: Long = data.readId()
    println(ClassStore.get(arrayClassObjectId))

    for (i <- 0 until nrOfElements) {
      val elementId = data.readId()
    }

    ObjectArrayRecord(
      arrayObjectId = arrayObjectId,
      stackTraceSerialNumber = stackTraceSerialNumber,
      nrOfElements = nrOfElements,
      arrayClassObjectId = arrayClassObjectId
    )
  }
}