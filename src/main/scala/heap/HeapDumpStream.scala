package heap

import java.io.{DataInputStream, File, FileInputStream}

import org.joda.time.DateTime

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * Created by mehmetgunturkun on 12/02/17.
  */

class HeapDumpStream(val idSize: Int,
                     private val data: DataInputStream) {
  def hasNext: Boolean = data.available() > 0
  def read(): Byte = data.readByte()
  def readBoolean(): Boolean = data.readByte() != 0
  def readShort(): Short = data.readShort()
  def readInt(): Int = data.readInt()
  def readLong(): Long = data.readLong()
  def readId(): Long = if (idSize == 4) data.readInt().toLong else data.readLong()

  def read(length: Int): Array[Byte] = {
    val arr: Array[Byte] = Array.ofDim[Byte](length)
    data.read(arr)
    arr
  }

  def consume(length: Int): Unit = {
    val nrOfSlots: Int = length / 1000
    val remainingBytes = length - (nrOfSlots * 1000)

    val arr: Array[Byte] = Array.ofDim[Byte](1000)
    for (i <- 0 until nrOfSlots) {
      data.read(arr)
    }

    data.read(arr, 0, remainingBytes)
  }
}

object HeapDumpStream {
  def fromFile(file: File): Option[HeapDumpStream] = {
    try {
      val fileInputStream: FileInputStream = new FileInputStream(file)
      val dataInputStream: DataInputStream = new DataInputStream(fileInputStream)

      val format: String = readFormat(dataInputStream)

      val sizeOfId: Int = dataInputStream.readInt()

      val startTime: Long = dataInputStream.readLong()
      val startDate: DateTime = new DateTime(startTime)

      val heapDumpStream = new HeapDumpStream(idSize = sizeOfId, data = dataInputStream)
      Some(heapDumpStream)
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