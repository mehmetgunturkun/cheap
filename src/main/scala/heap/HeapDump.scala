package heap

import java.io._
import java.nio.ByteBuffer

import org.joda.time.DateTime

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * Created by mehmetgunturkun on 22/01/17.
  */
class HeapDump(private val format: String,
               private val idSize: Int,
               private val startDate: DateTime,
               private val records: DataInputStream) {

  def hasNext():Boolean = records.available() > 0

  def nextRecord(): HeapDumpRecord = {
      val tag: Byte = records.readByte()
      val ts: Int = records.readInt()
      val length: Int = records.readInt()


    tag match {
      case 0x01 =>
        val stringId: Long = records.readLong()
        val contentLength = length - 8

        val arr = Array.ofDim[Byte](contentLength)


        records.read(arr)
        val content = new String(arr)

        StringRecord(stringId, content, length)
      case 0x02 =>

      case 0x0c =>
        parseHeapDump(records)
//        val arr = Array.ofDim[Byte](length)
//        records.readFully(arr)
        Record(tag, length)
      case _ =>
        val arr = Array.ofDim[Byte](length)
        records.readFully(arr)
        Record(tag, length)
    }


  }

  def parseHeapDump(in: DataInputStream): Unit = {
    var segmentCount = 0
    while (true) {
      val tag: Byte = in.readByte()
      segmentCount += 1

      printf(s"#${segmentCount} Tag: %x\n", tag)
      if (tag == 0x2c) {
        return {}
      } else {
        tag match {
          case 0xFF =>
            val objectId = in.readLong()
          case 0x01 =>
            val objectId = in.readLong()
            val refId = in.readLong()
          case 0x02 =>
            val objectId = in.readLong()
            val serialNumber = in.readInt()
            val frameNumber = in.readInt()
          case 0x03 =>
            val objectId = in.readLong()
            val serialNumber = in.readInt()
          case 0x04 =>
            val objectId = in.readLong()
            val serialNumber = in.readInt()
          case 0x05 =>
            val objectId = in.readLong()
          case 0x06 =>
            val objectId = in.readLong()
            val serialNumber = in.readInt()
          case 0x07 =>
            val objectId = in.readLong()
          case 0x08 =>
            val objectId = in.readLong()
            val threadSerialNumber = in.readInt()
            val stackTraceSerialNumber = in.readInt()
          case 0x20 =>
            val objectId = in.readLong()
            val stackTraceSerialNumber = in.readInt()

            val superClassId = in.readLong()
            val classLoaderId = in.readLong()
            val signersId = in.readLong()
            val domainObjectId = in.readLong()
            val res1 = in.readLong()
            val res2 = in.readLong()
            val instanceSize = in.readInt()

            val sizeOfPool = in.readShort()
            for (i <- 0 until sizeOfPool) {
              val poolIndex = in.readShort()

              val bType = in.readByte()
              val itemType = Type(bType)
              itemType match {
                case ObjectType =>
                  val objId = in.readLong()
                case BooleanType =>
                  val value = in.readBoolean()
                case CharType =>
                  val value = in.readChar()
                case FloatType =>
                  val value = in.readFloat()
                case DoubleType =>
                  val value = in.readDouble()
                case ByteType =>
                  val value = in.readByte()
                case ShortType =>
                  val value = in.readShort()
                case IntType =>
                  val value = in.readInt()
                case LongType =>
                  val value = in.readLong()
              }
            }

            val s2 = in.readShort()
            for (i <- 0 until s2) {
              val stringId = in.readLong()

              val bType = in.readByte()
              val itemType = Type(bType)
              itemType match {
                case ObjectType =>
                  val objId = in.readLong()
                case BooleanType =>
                  val value = in.readBoolean()
                case CharType =>
                  val value = in.readChar()
                case FloatType =>
                  val value = in.readFloat()
                case DoubleType =>
                  val value = in.readDouble()
                case ByteType =>
                  val value = in.readByte()
                case ShortType =>
                  val value = in.readShort()
                case IntType =>
                  val value = in.readInt()
                case LongType =>
                  val value = in.readLong()
              }
            }

            val s3 = in.readShort()
            for (i <- 0 until s3) {
              val stringId = in.readLong()

              val bType = in.readByte()
              val itemType = Type(bType)
            }
          case 0x21 =>
            val objectId = in.readLong()
            val stackTraceSerialNumber = in.readInt()

            val classObjectId = in.readLong()
            val nrOfBytes = in.readInt()

            val arr = Array.ofDim[Byte](nrOfBytes)
          case 0x22 =>
            val objectId = in.readLong()
            val serialNumber = in.readInt()

            val nrOfElements = in.readInt()
            val arrayId = in.readLong()
            for (i <- 0 until nrOfElements) {
              val arrayItem = in.readLong()
            }
          case 0x23 =>
            val objectId = in.readLong()
            val stackTraceSerialNumber = in.readInt()
            val nrOfElements = in.readInt()

            val bType = in.readByte()
            val itemType = Type(bType)

            for (i <- 0 until nrOfElements) {
              itemType match {
                case ObjectType =>
                  val objId = in.readLong()
                case BooleanType =>
                  val value = in.readBoolean()
                case CharType =>
                  val value = in.readChar()
                case FloatType =>
                  val value = in.readFloat()
                case DoubleType =>
                  val value = in.readDouble()
                case ByteType =>
                  val value = in.readByte()
                case ShortType =>
                  val value = in.readShort()
                case IntType =>
                  val value = in.readInt()
                case LongType =>
                  val value = in.readLong()
              }
            }
        }
      }
    }
  }
}

sealed trait HeapDumpRecord {
  def tag: Byte
  def length: Int
}

case class Record(tag: Byte, length: Int) extends HeapDumpRecord
case class StringRecord(id: Long, content: String, length: Int) extends HeapDumpRecord {
  override def tag: Byte = 0x01
}

case class HeapDumpSegmentRecord(id: Int, content: String, length: Int) extends HeapDumpRecord {
  override def tag: Byte = 0x12
}

object HeapDump {
  def apply(file: File): Option[HeapDump] = {
    try {
      val fileInputStream: FileInputStream = new FileInputStream(file)
      val dataInputStream: DataInputStream = new DataInputStream(fileInputStream)

      val format: String = readFormat(dataInputStream)

      val sizeOfId: Int = dataInputStream.readInt()

      val startTime: Long = dataInputStream.readLong()
      val startDate: DateTime = new DateTime(startTime)

      val heapDump = new HeapDump(format = format, idSize = sizeOfId, startDate = startDate, records = dataInputStream)
      Some(heapDump)
    } catch {
      case NonFatal(e) =>
        println("Failed to parse heap dump")
        None
    }
  }

  private def readFormat(dataInputStream: DataInputStream): String = {

    var currentByte = dataInputStream.readByte()
    val arrayBuffer = ArrayBuffer.empty[Byte]
    while(currentByte != 0) {
      arrayBuffer += currentByte
      currentByte = dataInputStream.readByte()
    }

    val format = new String(arrayBuffer.toArray, "UTF8")
    format
  }
}

sealed trait Type
object ObjectType extends Type
object BooleanType extends Type
object CharType extends Type
object FloatType extends Type
object DoubleType extends Type
object ByteType extends Type
object ShortType extends Type
object IntType extends Type
object LongType extends Type


object Type {
  def apply(bType: Byte): Type = {
    bType match {
      case 2 => ObjectType
      case 4 => BooleanType
      case 5 => CharType
      case 6 => FloatType
      case 7 => DoubleType
      case 8 => ByteType
      case 9 => ShortType
      case 10 => IntType
      case 11 => LongType
    }
  }
}
