package com.github.mrebhan.retrocomputers.xc8010;

import com.github.mrebhan.retrocomputers.api.DiskRegistry;
import com.github.mrebhan.retrocomputers.api.ItemCPU;

import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventHandler;
import net.minecraftforge.fml.common.Mod.Instance;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.registry.GameRegistry;
import net.minecraftforge.fml.relauncher.Side;

@Mod(modid = "xc8010", name = "RetroComputers: XC8010 CPU", version = "1.0", dependencies = "required-after:retrocomputers")
public class ModCPUXC8010 {

    @Instance
    public static ModCPUXC8010 instance = new ModCPUXC8010();

    public ItemCPU cpuItem;

    @EventHandler
    public void preInit(FMLPreInitializationEvent e) {
        cpuItem = new ItemCPUXC8010("xc8010:cpu", "cpu_xc8010");
        GameRegistry.register(cpuItem);

        DiskRegistry.INSTANCE.registerSystemDisk("xc8010", "item.xc8010:forth_disk",
                new ResourceLocation("xc8010:forth.bin"));
        if (e.getSide() == Side.CLIENT) {
            ModelLoader.setCustomModelResourceLocation(cpuItem, 0,
                    new ModelResourceLocation("xc8010:cpu", "inventory"));
        }
    }

}
