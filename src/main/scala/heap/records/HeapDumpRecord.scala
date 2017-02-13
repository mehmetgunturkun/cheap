package heap.records

import heap.core.{BasicType, HeapDumpStream, LongType}
import heap.core._

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

case class ClassDumpRecord(classObjectId: Long,
                           stackTraceSerialNumber: Int,
                           superClassObjectId: Long,
                           classLoaderObjectId: Long,
                           signersObjectId: Long,
                           protectionDomainObjectId: Long,
                           reserved1: Long,
                           reserved2: Long,
                           instanceSize: Int) extends HeapDumpRecord(ClassDump)

case class PrimitiveArrayRecord(arrayObjectId: Long,
                                stackTraceSerialNumber: Int,
                                nrOfElements: Int, elementType: BasicType) extends HeapDumpRecord(PrimitiveArrayDump)

case class ObjectArrayRecord() extends HeapDumpRecord(ObjectArrayDump)

case class InstanceDumpRecord() extends HeapDumpRecord(InstanceDump)

object HeapDumpRecord {
  def apply(tag: HeapDumpRecordTag, length: Int, stream: HeapDumpStream): HeapDumpRecord = {
    println("Tag: " + tag)
    tag match {
      case StringTag => parseStringRecord(length, stream)
      case LoadClassTag => parseLoad(length, stream)
      case UnloadClassTag => parseDefaultRecord(tag, length, stream)
      case StackFrameTag => parseDefaultRecord(tag, length, stream)
      case StackTraceTag => parseDefaultRecord(tag, length, stream)
      case AllocSitesTag => parseDefaultRecord(tag, length, stream)
      case StartThreadTag => parseDefaultRecord(tag, length, stream)
      case EndThreadTag => parseDefaultRecord(tag, length, stream)
      case HeapDumpStartTag => HeapDumpStartRecord
      case ClassDump => parseClassDump(length, stream)
      case InstanceDump => parseInstanceDump(stream)
      case PrimitiveArrayDump => parsePrimitiveArrayDump(stream)
      case ObjectArrayDump => parseObjectArrayDump(stream)
      case any => throw new Exception(s"Unrecognized tag: $any")
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

  def parseClassDump(length: Int, data: HeapDumpStream): ClassDumpRecord = {
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

  def parseInstanceDump(data: HeapDumpStream): InstanceDumpRecord = {
    val objectId = data.readId()
    val stackTraceSerialNumber: Int = data.readInt()
    val classObjectId: Long = data.readId()

    val clazz = ClassStore.get(classObjectId)
    println(clazz)

    val nrOfBytes: Int = data.readInt()

    val arr = data.read(nrOfBytes)
    InstanceDumpRecord()
  }

  def parsePrimitiveArrayDump(data: HeapDumpStream): PrimitiveArrayRecord = {
    val arrayObjectId = data.readId()
    val stackTraceSerialNumber = data.readInt()
    val nrOfElements = data.readInt()
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

  def parseObjectArrayDump(data: HeapDumpStream): ObjectArrayRecord = {
    val arrayObjectId = data.readId()

    val stackTraceSerialNumber = data.readInt()
    val nrOfElements = data.readInt()

    val arrayClassObjectId = data.readId()
    println(ClassStore.get(arrayClassObjectId))

    for (i <- 0 until nrOfElements) {
      val elementId = data.readId()
    }

    ObjectArrayRecord()
  }
}