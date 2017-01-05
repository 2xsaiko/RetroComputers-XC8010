package com.github.mrebhan.retrocomputers.xc8010;

import com.github.mrebhan.retrocomputers.api.ICPU;
import com.github.mrebhan.retrocomputers.api.IComputerCase;
import com.github.mrebhan.retrocomputers.api.ItemCPU;
import io.netty.buffer.ByteBuf;
import net.minecraft.item.ItemStack;

public class ItemCPUXC8010 extends ItemCPU {

    private final int amp;

    public ItemCPUXC8010(String unlocalizedName, String registryName, int amp) {
        super(unlocalizedName, registryName);
        this.amp = amp;
    }

    @Override
    public ICPU createCPU(IComputerCase arg1, ItemStack arg2) {
        return new ICPUWrapper(new CPUXC8010(arg1), amp);
    }

    @Override
    public Class<? extends ICPU> getCPUClass(ItemStack arg0) {
        return CPUXC8010.class;
    }

    private static class ICPUWrapper implements ICPU {
        private final ICPU base;
        private final int amp;

        public ICPUWrapper(ICPU base, int amp) {
            this.base = base;
            this.amp = amp;
        }

        @Override
        public int insnGain() {
            return base.insnGain() * amp;
        }

        @Override
        public int insnBufferSize() {
            return base.insnBufferSize() * amp;
        }

        @Override
        public boolean isTimedOut() {
            return base.isTimedOut();
        }

        @Override
        public void next() {
            base.next();
        }

        @Override
        public void deserialize(ByteBuf byteBuf) {
            base.deserialize(byteBuf);
        }

        @Override
        public void timeout() {
            base.timeout();
        }

        @Override
        public void reset(boolean b) {
            base.reset(b);
        }

        @Override
        public void serialize(ByteBuf byteBuf) {
            base.serialize(byteBuf);
        }

        @Override
        public void clearTimeout() {
            base.clearTimeout();
        }

        @Override
        public Class<? extends ICPU> getCPUClass() {
            return base.getClass();
        }
    }

}
