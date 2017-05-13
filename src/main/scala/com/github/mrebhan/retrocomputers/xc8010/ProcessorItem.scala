package com.github.mrebhan.retrocomputers.xc8010

import com.github.mrebhan.retrocomputers.common.api.{CreativeTab, ProcessorFactory, ProcessorLike}
import net.minecraft.item.Item

/**
  * Created by marco on 12.04.17.
  */
class ProcessorItem() extends Item with ProcessorFactory {
  setHasSubtypes(true)
  setMaxDamage(3)
  setCreativeTab(CreativeTab)

  override def apply(): ProcessorLike = new Processor

  override def processorType: Class[_ <: ProcessorLike] = classOf[Processor]
}
