package com.github.mrebhan.retrocomputers.xc8010

import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.item.Item
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.Mod.EventHandler
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.Side

/**
  * Created by marco on 12.04.17.
  */
@Mod(modid = Mod.ModId, name = Mod.Name, version = Mod.Version, dependencies = "required-after:retrocomputers", modLanguage = "scala")
object Mod {
  final val ModId = "xc8010"
  final val Name = "RetroComputers: XC8010 CPU"
  final val Version = "1.1.0"

  var cpu: Item = _

  @EventHandler
  def preInit(e: FMLPreInitializationEvent): Unit = {
    cpu = new ProcessorItem()
      .setUnlocalizedName("xc8010:cpu")
      .setRegistryName("xc8010:cpu")

    GameRegistry.register(cpu)

    if (e.getSide == Side.CLIENT) {
      ModelLoader.setCustomModelResourceLocation(cpu, 0, new ModelResourceLocation("xc8010:cpu", "inventory"))
    }
  }

}
