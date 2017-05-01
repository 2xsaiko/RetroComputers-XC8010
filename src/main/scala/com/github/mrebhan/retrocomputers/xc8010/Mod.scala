package com.github.mrebhan.retrocomputers.xc8010

import com.github.mrebhan.retrocomputers.common.api.item.ItemReadonlyDisk
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

    val disks = new ItemReadonlyDisk(
      () => getClass.getClassLoader.getResourceAsStream("assets/xc8010/minforth.bin"),
      () => getClass.getClassLoader.getResourceAsStream("assets/xc8010/forth.bin"),
      () => getClass.getClassLoader.getResourceAsStream("assets/xc8010/extforth.bin"),
      () => getClass.getClassLoader.getResourceAsStream("assets/xc8010/decompiler.img")
    ).setRegistryName("xc8010:disk").setUnlocalizedName("xc8010:disk")

    GameRegistry.register(cpu)
    GameRegistry.register(disks)

    if (e.getSide == Side.CLIENT) {
      ModelLoader.setCustomModelResourceLocation(cpu, 0, new ModelResourceLocation("xc8010:cpu", "inventory"))
      ModelLoader.setCustomModelResourceLocation(disks, 0, new ModelResourceLocation("retrocomputers:system_disk", "inventory"))
      ModelLoader.setCustomModelResourceLocation(disks, 1, new ModelResourceLocation("retrocomputers:system_disk", "inventory"))
      ModelLoader.setCustomModelResourceLocation(disks, 2, new ModelResourceLocation("retrocomputers:system_disk", "inventory"))
      ModelLoader.setCustomModelResourceLocation(disks, 3, new ModelResourceLocation("retrocomputers:system_disk", "inventory"))
    }
  }

}
