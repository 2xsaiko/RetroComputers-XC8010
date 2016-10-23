package com.github.mrebhan.retrocomputers.xc8010;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.function.Consumer;

import com.github.mrebhan.retrocomputers.api.ICPU;
import com.github.mrebhan.retrocomputers.api.IComputerCase;
import com.github.mrebhan.retrocomputers.api.IMemory;
import com.github.mrebhan.retrocomputers.api.IMemory64;

import io.netty.buffer.ByteBuf;
import net.minecraft.util.ResourceLocation;

public class CPUXC8010 implements ICPU {

	private static final short E, N, V, M, X, D, I, Z, C;
	private static final ResourceLocation BOOTLOADER = new ResourceLocation("xc8010:bootldr.bin");

	private IMemory64 mem;
	private IComputerCase pcCase;

	private int regA;
	private int regB;
	private int regX;
	private int regY;

	private int sp;
	private int rp;
	private int pc;

	private int resetAddr;
	private int brkAddr;

	private byte rbAddr;
	private int rbOffset;
	private boolean rbEnabled;

	private short flags;

	private boolean timeout;
	private IMemory cache;

	public CPUXC8010(IMemory64 mem, IComputerCase pcCase) {
		this.mem = mem;
		this.pcCase = pcCase;
		hardReset();
	}

	private void hardReset() {
		regA = 0;
		regX = 0;
		regY = 0;

		sp = 0x01FF;
		rp = 0x02FF;
		pc = 0x0400;

		resetAddr = 0x0400;
		brkAddr = 0x2000;

		dn(-1);
		up(E | M | X);

		mem.pokeC(0, pcCase.getDiskAddr()); // Disk drive address [Default: 2]
		mem.pokeC(1, pcCase.getTermAddr()); // terminal address [Default: 1]

		String path = "assets/" + BOOTLOADER.getResourceDomain() + "/" + BOOTLOADER.getResourcePath();
		InputStream str = ModCPUXC8010.class.getClassLoader().getResourceAsStream(path);

		if (str != null) {
			byte[] bl = new byte[0x100];
			try {
				str.read(bl);
				mem.poke(0x400, bl);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public void reset(boolean arg0) {
		if (arg0) {
			hardReset();
			return;
		}
		sp = 0x01FF;
		rp = 0x02FF;
		pc = resetAddr;
	}

	@Override
	public void next() {
		int insn = pc1();
//		System.out.printf("%04x: %02x%n", pc - 1, insn);
		switch (insn) {
		case 0x00: // brk
			push2(pc);
			push1(flags);
			pc = brkAddr;
			break;
		case 0x01: // ora (ind, x)
			_ora(pcIX());
			break;
		case 0x03: // ora r,S
			_ora(peekM(pcS()));
			break;
		case 0x04: // tsb zp
			_tsb(peekM(pc1()));
			break;
		case 0x08: // php
			push1(flags & 0xFF);
			break;
		case 0x18: // clc
			dn(C);
			break;
		case 0x1A: // inc a
			regA++;
			regA &= maskM();
			break;
		case 0x20: // jsr abs
			_jsr(pc2());
			break;
		case 0x28: // plp
			setFlags(pop1());
			break;
		case 0x48: // pha
			pushM(regA);
			break;
		case 0x4c: // jmp abs
			pc = pc2();
			break;
		case 0x5a: // phy
			pushX(regY);
			break;
		case 0x60: // rts
			_rts();
			break;
		case 0x64: // stz zp
			_stz(pc1());
			break;
		case 0x68: // pla
			regA = popM();
			updNZ(regA);
			break;
		case 0x74: // stz zp, x
			_stz(pc1X());
			break;
		case 0x7a: // ply
			regY = popX();
			updNZX(regY);
			break;
		case 0x80: // bra rel
			_bra(pc1(), true);
			break;
		case 0x84: // sty zp
			_sty(pc1());
			break;
		case 0x85: // sta zp
			_sta(pc1());
			break;
		case 0x88: // dey
			regY--;
			regY = regY & maskX();
			updNZX(regY);
			break;
		case 0x8c: // sty abs
			_sty(pc2());
			break;
		case 0x8d: // sta abs
			_sta(pc2());
			break;
		case 0x91: // sta (ind), y
			_sta(pcIY());
			break;
		case 0x92: // sta (ind)
			_sta(pcI());
			break;
		case 0x94: // sty zp, x
			_sty(pc1X());
			break;
		case 0x95: // sta zp, x
			_sta(pc1X());
			break;
		case 0x99: // sta abs, y
			_sta(pc2Y());
			break;
		case 0x9a: // txs
			sp = regX & 0xFFFF;
			break;
		case 0x9c: // stz abs
			_stz(pc2());
			break;
		case 0xa0: // ldy #
			_ldy(pcX());
			break;
		case 0xa2: // ldy #
			_ldx(pcX());
			break;
		case 0xa5: // lda zp
			_lda(peekM(pc1()));
			break;
		case 0xa9: // lda #
			_lda(pcM());
			break;
		case 0xad: // lda abs
			_lda(peekM(pc2()));
			break;
		case 0xb1: // lda (ind), y
			_lda(peekM(pcIY()));
			break;
		case 0xb9: // lda abs, y
			_lda(peekM(pc2Y()));
			break;
		case 0xc0: // cpy #
			_cmpx(regY, pcX());
			break;
		case 0xc2: // rep #
			_rep(pc1());
			break;
		case 0xc8: // iny
			regY++;
			regY = regY & maskX();
			updNZX(regY);
			break;
		case 0xc9: // cmp #
			_cmp(regA, pcM());
			break;
		case 0xca: // dex
			regX--;
			regX = regX & maskX();
			updNZX(regX);
		case 0xcb: // wai
			timeout();
			break;
		case 0xd0: // bne rel
			_bra(pc1(), !isup(Z));
			break;
		case 0xdb: // stp
			pcCase.haltAndCatchFire();
			break;
		case 0xe2: // sep #
			_sep(pc1());
			break;
		case 0xe6: // inc zp
			_inc(pc1());
			break;
		case 0xee: // inc abs
			_inc(pc2());
			break;
		case 0xef: // mmu
			_mmu(pc1());
			break;
		case 0xf0: // beq rel
			_bra(pc1(), isup(Z));
			break;
		case 0xfb: // xce
			_xce();
			break;
		default:
			System.out.printf("Invalid opcode: %02x at %04x%n", insn, pc - 1);
			pcCase.haltAndCatchFire();
		}
	}

	private void _tsb(int data) {
		setFlags(Z, (data & regA) != 0);
		this.regA |= data & 0xFFFF;
	}
	
	private void _ora(int data) {
		this.regA |= data & 0xFFFF;
		updNZ(regA);
	}
	
	private void _inc(int addr) {
		int data = peekM(addr);
		data++;
		data &= maskM();
		pokeM(addr, data);
		updNZ(data);
	}

	private void _cmp(int a, int b) {
		int c = a - b;
		updNZC(c);
	}

	private void _cmpx(int a, int b) {
		int c = a - b;
		updNZCX(c);
	}

	private void _lda(int data) {
		regA = data & maskM();
		updNZ(regA);
	}

	private void _ldy(int data) {
		regY = data & maskX();
		updNZX(regY);
	}

	private void _ldx(int data) {
		regX = data & maskX();
		updNZX(regX);
	}

	private void updNZ(int data) {
		setFlags(Z, data == 0);
		setFlags(N, (data & negM()) != 0);
	}

	private void updNZX(int data) {
		setFlags(Z, data == 0);
		setFlags(N, (data & negX()) != 0);
	}

	private void updNZC(int data) {
		setFlags(C, data >= 0);
		setFlags(Z, data == 0);
		setFlags(N, (data & negM()) != 0);
	}

	private void updNZCX(int data) {
		setFlags(C, data >= 0);
		setFlags(Z, data == 0);
		setFlags(N, (data & negX()) != 0);
	}

	private int negM() {
		return isup(M) ? 0x8000 : 0x80;
	}

	private int negX() {
		return isup(X) ? 0x8000 : 0x80;
	}

	private int maskM() {
		return isup(M) ? 0xFFFF : 0xFF;
	}

	private int maskX() {
		return isup(X) ? 0xFFFF : 0xFF;
	}

	private void _sta(int addr) {
		poke1(addr, regA & 0xFF);
		if (isup(M)) {
			poke1(addr + 1, regA >> 8);
		}
	}

	private void _sty(int addr) {
		poke1(addr, regY & 0xFF);
		if (isup(X)) {
			poke1(addr + 1, regY >> 8);
		}
	}

	private void _stz(int addr) {
		poke1(addr, 0);
		if (isup(M)) {
			poke1(addr + 1, 0);
		}
	}

	private void _jsr(int data) {
		push2(pc);
		pc = data & 0xFFFF;
	}

	private void _rts() {
		pc = pop2();
	}

	private void _xce() {
		if (isup(C) != isup(E)) {
			if (isup(C)) {
				up(E);
				dn(C);

				up(X);
				regX &= 0xFF;
				regY &= 0xFF;

				if (!isup(M)) {
					regB = regA >> 8;
				}
				regA &= 0xFF;
				up(M);
			} else {
				up(C);
				dn(E);
			}
		}
	}

	private void _sep(int nflags) {
		setFlags(flags | nflags);
	}

	private void _rep(int nflags) {
		setFlags(flags & ~nflags);
	}

	private void _bra(int offset, boolean b) {
		if (b) {
			byte bo = (byte) offset;
			pc = (pc + bo) & 0xFFFF;
		}
	}

	private void _mmu(int data) {
		switch (data) {
		case 0x00:
			if (rbAddr != (byte) regA && cache != null) {
				timeout();
			}
			rbAddr = (byte) regA;
			break;
		case 0x80:
			regA = rbAddr & 0xFF;
			break;
		case 0x01:
			rbOffset = regA;
			break;
		case 0x81:
			regA = rbOffset;
			break;
		case 0x02:
			rbEnabled = true;
			break;
		case 0x82:
			rbEnabled = false;
			break;
		case 0x03:
			// TODO
			break;
		case 0x83:
			// TODO
			break;
		case 0x04:
			// TODO
			break;
		case 0x84:
			// TODO
			break;
		case 0x05:
			brkAddr = regA;
			break;
		case 0x85:
			regA = brkAddr;
			break;
		case 0x06:
			resetAddr = regA;
			break;
		case 0x86:
			regA = resetAddr;
			break;
		}
	}

	private void setFlags(int nflags) {
		dn(~(E | M));
		flags |= (N | V | D | I | Z | C) & nflags;
		if (!isup(E)) {
			flags |= X & nflags;
			if (!isup(X)) {
				regX &= 0xFF;
				regY &= 0xFF;
			}
			boolean om = isup(M);
			boolean nm = (nflags & M) == M;
			if (om != nm) {
				if (om) {
					regA |= regB << 8;
				} else {
					regB = regA >> 8;
					regA &= 0xFF;
				}
				setFlags(M, nm);
			}
		}
	}

	private int pc1() {
		return peek1((pc = (pc + 1 & 0xFFFF)) - 1);
	}

	private int pc2() {
		int i1 = pc1();
		int i2 = pc1();
		return i1 | i2 << 8;
	}

	private int peek2(int addr) {
		int p1 = peek1(addr);
		int p2 = peek1(addr + 1);
		return p1 | p2 << 8;
	}

	private int pcM() {
		if (isup(M))
			return pc2();
		return pc1();
	}

	private int pcX() {
		if (isup(X))
			return pc2();
		return pc1();
	}

	private int pcI() {
		int addr = pc2();
		return peek2(addr);
	}
	
	private int pcIY() {
		int addr = pc2();
		int val = peek2(addr);
		return (val + regY) & 0xFFFF;
	}

	private int pc1Y() {
		return (pc1() + regY) & 0xFF;
	}

	private int pc2Y() {
		return (pc2() + regY) & 0xFFFF;
	}

	private int pc1X() {
		return (pc1() + regX) & 0xFF;
	}

	private int pc2X() {
		return (pc2() + regX) & 0xFFFF;
	}

	private int pcIX() {
		int addr = (pc2() + regX) & 0xFFFF;
		int val = peek2(addr);
		return val;
	}
	
	private int pcS() {
		return (pc1() + sp) & 0xFFFF;
	}
	
	private int pcR() {
		return (pc1() + rp) & 0xFFFF;
	}
	
	private int pcSY() {
		return (peek2(pcS()) + regY) & 0xFFFF;
	}
	
	private int pcRY() {
		return (peek2(pcR()) + regY) & 0xFFFF;
	}

	private void push1(int i) {
		poke1(sp--, i & 0xFF);
	}

	private void push2(int i) {
		push1(i & 0xFF);
		push1(i >> 8);
	}

	private void pushM(int i) {
		if (isup(M)) {
			push2(i);
		} else {
			push1(i);
		}
	}

	private void pushX(int i) {
		if (isup(X)) {
			push2(i);
		} else {
			push1(i);
		}
	}

	private int pop1() {
		return peek1(++sp);
	}

	private int pop2() {
		int i1 = pop1();
		int i2 = pop1();
		return i1 << 8 | i2;
	}

	private int popM() {
		if (isup(M)) {
			return pop2();
		} else {
			return pop1();
		}
	}

	private int popX() {
		if (isup(X)) {
			return pop2();
		} else {
			return pop1();
		}
	}

	private void pushr1(int i) {
		poke1(rp--, i & 0xFF);
	}

	private void pushr2(int i) {
		pushr1(i & 0xFF);
		pushr1(i >> 8);
	}

	private int popr1() {
		return peek1(++rp);
	}

	private int popr2() {
		int i1 = popr1();
		int i2 = popr1();
		return i1 << 8 | i2;
	}

	private int peekM(int addr) {
		int i = peek1(addr);
		if (isup(M)) {
			i |= peek1(addr + 1) << 8;
		}
		return i;
	}

	private int peekX(int addr) {
		int i = peek1(addr);
		if (isup(X)) {
			i |= peek1(addr + 1) << 8;
		}
		return i;
	}

	private int peek1(int addr) {
		if (rbEnabled && addr >= rbOffset && addr < rbOffset + 0x100) {
			if (cache())
				return cache.peekC(addr - rbOffset);
			return 0;
		}
		return mem.peekC(addr);
	}

	private void pokeM(int addr, int data) {
		if (isup(M)) {
			poke2(addr, data);
		} else {
			poke1(addr, data);
		}
	}

	private void pokeX(int addr, int data) {
		if (isup(X)) {
			poke2(addr, data);
		} else {
			poke1(addr, data);
		}
	}

	private void poke2(int addr, int data) {
		poke1(addr, data & 0xFF);
		poke1(addr + 1, data >> 8);
	}

	private void poke1(int addr, int data) {
		if (rbEnabled && addr >= rbOffset && addr < rbOffset + 0x100) {
			if (cache())
				cache.pokeC(addr - rbOffset, data);
			return;
		}
		mem.pokeC(addr, data);
	}

	private boolean cache() {
		if (cache == null) {
			IMemory mem = pcCase.findBus(rbAddr);
			if ((cache = mem) == null) {
				timeout();
				return false;
			}
		}
		return true;
	}

	@Override
	public void clearCache() {
		cache = null;
	}

	@Override
	public void clearTimeout() {
		timeout = false;
	}

	@Override
	public boolean isTimedOut() {
		return timeout;
	}

	@Override
	public void deserialize(ByteBuf buf) {
		regA = buf.readShort() & 0xFFFF;
		regB = buf.readShort() & 0xFFFF;
		regX = buf.readShort() & 0xFFFF;
		regY = buf.readShort() & 0xFFFF;
		sp = buf.readShort() & 0xFFFF;
		rp = buf.readShort() & 0xFFFF;
		pc = buf.readShort() & 0xFFFF;
		resetAddr = buf.readShort() & 0xFFFF;
		brkAddr = buf.readShort() & 0xFFFF;
		rbAddr = buf.readByte();
		rbOffset = buf.readShort() & 0xFFFF;
		rbEnabled = buf.readBoolean();
		flags = buf.readShort();
		timeout = buf.readBoolean();
	}

	@Override
	public void serialize(ByteBuf buf) {
		buf.writeShort(regA);
		buf.writeShort(regB);
		buf.writeShort(regX);
		buf.writeShort(regY);
		buf.writeShort(sp);
		buf.writeShort(rp);
		buf.writeShort(pc);
		buf.writeShort(resetAddr);
		buf.writeShort(brkAddr);
		buf.writeByte(rbAddr);
		buf.writeShort(rbOffset);
		buf.writeBoolean(rbEnabled);
		buf.writeShort(flags);
		buf.writeBoolean(timeout);
	}

	@Override
	public void timeout() {
		timeout = true;
	}

	private void up(int uflags) {
		this.flags |= uflags;
	}

	private void dn(int uflags) {
		this.flags &= ~uflags;
	}

	private boolean isup(int uflags) {
		return (this.flags & uflags) == uflags;
	}

	private void setFlags(short flags, boolean condition) {
		if (condition) {
			up(flags);
		} else {
			dn(flags);
		}
	}

	static {
		E = 256;
		N = 128;
		V = 64;
		M = 32;
		X = 16;
		D = 8;
		I = 4;
		Z = 2;
		C = 1;
	}

}
