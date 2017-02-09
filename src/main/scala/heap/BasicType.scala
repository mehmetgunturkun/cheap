package heap

/**
  * Created by mehmetgunturkun on 02/02/17.
  */
trait BasicType

case object ObjectType extends BasicType
case object BooleanType extends BasicType
case object CharType extends BasicType
case object FloatType extends BasicType
case object DoubleType extends BasicType
case object ByteType extends BasicType
case object ShortType extends BasicType
case object IntType extends BasicType
case object LongType extends BasicType

object BasicType {
  def apply(byte: Byte): BasicType = {
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