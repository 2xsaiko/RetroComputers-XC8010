package com.github.mrebhan.retrocomputers.xc8010;

import com.github.mrebhan.retrocomputers.api.ICPU;
import com.github.mrebhan.retrocomputers.api.IComputerCase;
import com.github.mrebhan.retrocomputers.api.IMemory64;
import com.github.mrebhan.retrocomputers.api.ItemCPU;

import net.minecraft.item.ItemStack;

public class ItemCPUXC8010 extends ItemCPU {

	public ItemCPUXC8010(String unlocalizedName, String registryName) {
		super(unlocalizedName, registryName);
	}

	@Override
	public ICPU createCPU(IMemory64 arg0, IComputerCase arg1, ItemStack arg2) {
		return new CPUXC8010(arg0, arg1);
	}

	@Override
	public Class<? extends ICPU> getCPUClass(ItemStack arg0) {
		return CPUXC8010.class;
	}

}
