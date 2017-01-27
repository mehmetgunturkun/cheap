package heap

/**
  * Created by mehmetgunturkun on 27/01/17.
  */
abstract class HeapDumpRecordTag(val tag: Byte)

case object StringTag extends HeapDumpRecordTag(0x01)
case object LoadClassTag extends HeapDumpRecordTag(0x02)
case object UnloadClassTag extends HeapDumpRecordTag(0x03)
case object StackFrameTag extends HeapDumpRecordTag(0x04)
case object StackTraceTag extends HeapDumpRecordTag(0x05)
case object AllocSitesTag extends HeapDumpRecordTag(0x06)
case object HeapSummaryTag extends HeapDumpRecordTag(0x07)
case object StartThreadTag extends HeapDumpRecordTag(0x0A)
case object EndThreadTag extends HeapDumpRecordTag(0x0B)
case object HeapDumpTag extends HeapDumpRecordTag(0x0C)
case object HeapDumpSegmentTag extends HeapDumpRecordTag(0x1C)
case object HeapDumpEndTag extends HeapDumpRecordTag(0x2C)
case object CpuSamplesTag extends HeapDumpRecordTag(0x0D)
case object ControlSettingsTag extends HeapDumpRecordTag(0x0E)

object HeapDumpRecordTag {

  private val tagList = List(
    StringTag,
    LoadClassTag,
    UnloadClassTag,
    StackFrameTag,
    StackTraceTag,
    AllocSitesTag,
    StartThreadTag,
    EndThreadTag,
    HeapDumpTag,
    HeapDumpSegmentTag,
    CpuSamplesTag,
    ControlSettingsTag
  )

  private val tagMap: Map[Byte, HeapDumpRecordTag] = tagList.map(tag => tag.tag -> tag).toMap

  def apply(tag: Byte): HeapDumpRecordTag = tagMap(tag)
}
