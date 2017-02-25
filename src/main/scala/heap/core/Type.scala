package heap.core

/**
  * Created by mehmetgunturkun on 12/02/17.
  */
sealed trait Type

case object ObjectType extends Type
case object BooleanType extends Type
case object CharType extends Type
case object FloatType extends Type
case object DoubleType extends Type
case object ByteType extends Type
case object ShortType extends Type
case object IntType extends Type
case object LongType extends Type

object Type {
  def apply(byte: Byte): Type = {
    byte match {
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

sealed trait Value

case class ObjectValue(objectId: Long) extends Value
case class BooleanValue(value: Boolean) extends Value
case class ByteValue(value: Byte) extends Value
case class CharValue(value: Char) extends Value
case class ShortValue(value: Short) extends Value
case class IntValue(value: Int) extends Value
case class FloatValue(value: Float) extends Value
case class DoubleValue(value: Double) extends Value
case class LongValue(value: Long) extends Value

object Value {
  def apply(data: HeapDumpStream, fieldType: Type): Value = {
    fieldType match {
      case ObjectType =>
        val objectId = data.readId()
        ObjectValue(objectId)
      case ByteType =>
        val value = data.read()
        ByteValue(value)
      case BooleanType =>
        val value: Boolean = data.readBoolean()
        BooleanValue(value)
      case CharType =>
        val value = data.readChar()
        CharValue(value)
      case ShortType =>
        val value = data.readShort()
        ShortValue(value)
      case IntType =>
        val value = data.readInt()
        IntValue(value)
      case FloatType =>
        val value = data.readFloat()
        FloatValue(value)
      case DoubleType =>
        val value = data.readDouble()
        DoubleValue(value)
      case LongType =>
        val value = data.readLong()
        LongValue(value)
    }
  }
}