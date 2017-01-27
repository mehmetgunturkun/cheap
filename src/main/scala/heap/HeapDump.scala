package heap

import java.io.{DataInputStream, File, FileInputStream}

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

  def hasNext: Boolean = records.available() > 0

  def nextRecord(): HeapDumpRecord = {
    val tag: Byte = records.readByte()
    val heapDumpTag: HeapDumpRecordTag = HeapDumpRecordTag(tag)

    val ts: Int = records.readInt()
    val length: Int = records.readInt()

    val record = HeapDumpRecord(heapDumpTag, idSize, length, records)
    record
  }
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
