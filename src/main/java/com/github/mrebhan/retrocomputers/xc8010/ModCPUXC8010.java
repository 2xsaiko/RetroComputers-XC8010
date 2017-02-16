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
    public ItemCPU cpuItem_amp8;
    public ItemCPU cpuItem_amp2;

    @EventHandler
    public void preInit(FMLPreInitializationEvent e) {
        cpuItem = new ItemCPUXC8010("xc8010:cpu", "cpu_xc8010", 1);
        cpuItem_amp2 = new ItemCPUXC8010("xc8010:cpu_amp2", "cpu_xc8010_amp2", 2);
        cpuItem_amp8 = new ItemCPUXC8010("xc8010:cpu_amp8", "cpu_xc8010_amp8", 8);
        GameRegistry.register(cpuItem);
        GameRegistry.register(cpuItem_amp2);
        GameRegistry.register(cpuItem_amp8);

        DiskRegistry.INSTANCE.registerSystemDisk("xc8010", "item.xc8010:forth_disk",
                new ResourceLocation("xc8010:forth.bin"));
        DiskRegistry.INSTANCE.registerSystemDisk("xc8010", "item.xc8010:forth_disk_ext",
                new ResourceLocation("xc8010:extforth.bin"));
        DiskRegistry.INSTANCE.registerSystemDisk("xc8010", "item.xc8010:forth_disk_min",
                new ResourceLocation("xc8010:minforth.bin"));
        DiskRegistry.INSTANCE.registerSystemDisk("xc8010", "item.xc8010:source_decompiler",
                new ResourceLocation("xc8010:decompiler.img"));
        if (e.getSide() == Side.CLIENT) {
            ModelLoader.setCustomModelResourceLocation(cpuItem, 0,
                    new ModelResourceLocation("xc8010:cpu", "inventory"));
            ModelLoader.setCustomModelResourceLocation(cpuItem_amp2, 0,
                    new ModelResourceLocation("xc8010:cpu", "inventory"));
            ModelLoader.setCustomModelResourceLocation(cpuItem_amp8, 0,
                    new ModelResourceLocation("xc8010:cpu", "inventory"));
        }
    }

}
