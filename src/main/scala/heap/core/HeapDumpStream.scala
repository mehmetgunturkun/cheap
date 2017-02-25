package heap.core

import java.io.{DataInputStream, File, FileInputStream}
import java.nio.ByteBuffer

import org.joda.time.DateTime

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * Created by mehmetgunturkun on 12/02/17.
  */


trait HeapDumpStream {
  def idSize: Int
  def hasNext: Boolean
  def read(): Byte
  def readBoolean(): Boolean
  def readChar(): Char
  def readShort(): Short
  def readInt(): Int
  def readFloat(): Float
  def readDouble(): Double
  def readLong(): Long
  def readId(): Long

  def read(length: Int): Array[Byte]
  def consume(length: Int): Unit
}

class HeapDumpStreamFromDataStream(val idSize: Int,
                                   private val data: DataInputStream) extends HeapDumpStream {
  def hasNext: Boolean = data.available() > 0
  def read(): Byte = data.readByte()
  def readBoolean(): Boolean = data.readByte() != 0
  def readChar(): Char = data.readChar()
  def readShort(): Short = data.readShort()
  def readInt(): Int = data.readInt()
  def readFloat(): Float = data.readFloat()
  def readDouble(): Double = data.readDouble()
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

class HeapDumpStreamFromDByteBuffer(val idSize: Int,
                                    private val data: ByteBuffer) extends HeapDumpStream {
  def hasNext: Boolean = data.hasRemaining
  def read(): Byte = data.get()
  def readBoolean(): Boolean = data.get() != 0
  def readChar(): Char = data.getChar()
  def readShort(): Short = data.getShort()
  def readInt(): Int = data.getInt()
  def readFloat(): Float = data.getFloat()
  def readDouble(): Double = data.getDouble()
  def readLong(): Long = data.getLong()
  def readId(): Long = if (idSize == 4) data.getInt().toLong else data.getLong()

  def read(length: Int): Array[Byte] = {
    val arr: Array[Byte] = Array.ofDim[Byte](length)
    data.get(arr)
    arr
  }

  def consume(length: Int): Unit = {
    val nrOfSlots: Int = length / 1000
    val remainingBytes = length - (nrOfSlots * 1000)

    val arr: Array[Byte] = Array.ofDim[Byte](1000)
    for (i <- 0 until nrOfSlots) {
      data.get(arr)
    }

    data.get(arr, 0, remainingBytes)
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

      val heapDumpStream = new HeapDumpStreamFromDataStream(idSize = sizeOfId, data = dataInputStream)
      Some(heapDumpStream)
    } catch {
      case NonFatal(e) =>
        println("Failed to parse heap dump")
        None
    }
  }

  def fromByteBuffer(idSize: Int, byteArray: Array[Byte]): HeapDumpStream = {
    new HeapDumpStreamFromDByteBuffer(idSize, ByteBuffer.wrap(byteArray))
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