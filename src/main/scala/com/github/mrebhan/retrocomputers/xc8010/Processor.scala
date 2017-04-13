package com.github.mrebhan.retrocomputers.xc8010

import com.github.mrebhan.retrocomputers.common.api.crosscompat.NBTCompoundLike
import com.github.mrebhan.retrocomputers.common.api.{MemoryProvider, ProcessorLike}

/**
  * Created by marco on 12.04.17.
  */
class Processor extends ProcessorLike {
  override var memoryProvider: MemoryProvider = _

  override def next(): Unit = ()

  override def reset(hard: Boolean): Unit = ()

  override def serialize(out: NBTCompoundLike): Unit = ()

  override def deserialize(in: NBTCompoundLike): Unit = ()

  override def timeout(): Unit = ()

  override def clearTimeout(): Unit = ()

  override def isTimedOut: Boolean = false

  override def insnBufferSize: Int = 10000

  override def insnGain: Int = 1000
}
