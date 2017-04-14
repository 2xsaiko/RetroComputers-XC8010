package com.github.mrebhan.retrocomputers.xc8010

import com.github.mrebhan.retrocomputers.common.api.Conversions._
import com.github.mrebhan.retrocomputers.common.api.crosscompat.NBTCompoundLike
import com.github.mrebhan.retrocomputers.common.api.crosscompat.NBTCompoundLike.getValue
import com.github.mrebhan.retrocomputers.common.api.{MemoryProvider, ProcessorLike}

/**
  * Created by marco on 12.04.17.
  */
class Processor extends ProcessorLike {
  override var memoryProvider: MemoryProvider = _

  private def mem = memoryProvider

  private var rB: Byte = 0
  private var rI: Short = 0
  private var sp: Short = 0
  private var rp: Short = 0
  private var pc: Short = 0
  private var resetAddr: Short = 0
  private var brkAddr: Short = 0
  private var flags: Short = 0
  private var _timeout: Boolean = false

  private var busOffset: Short = 0
  private var busEnabled: Boolean = false

  private def rA: Short = hidden.rA & maskM
  private def rA_=(s: Short): Unit = hidden.rA = s
  private def rD: Short = hidden.rD & maskM
  private def rD_=(s: Short): Unit = hidden.rD = s
  private def rX: Short = hidden.rX & maskX
  private def rX_=(s: Short): Unit = hidden.rX = s
  private def rY: Short = hidden.rA & maskX
  private def rY_=(s: Short): Unit = hidden.rY = s

  override def reset(hard: Boolean): Unit = {
    if (hard) {
      rA = 0
      rB = 0
      rX = 0
      rY = 0
      rI = 0
      rD = 0

      resetAddr = 0x0400
      brkAddr = 0x2000
      ↓(-1)
      ↑(E, M, X)

      mem(0) = mem.diskAddr
      mem(1) = mem.termAddr

      val path = "assets/xc8010/bootldr.bin"
      val is = getClass.getClassLoader.getResourceAsStream(path)

      if (is != null) {
        val loader = new Array[Byte](0x100)
        is.read(loader)
        for (i <- loader.indices; b = loader(i)) mem(i) = b
      }
      is.close()
    }
    sp = 0x01FF
    rp = 0x02FF
    pc = resetAddr
  }

  override def serialize(out: NBTCompoundLike): Unit = {
    out.short("a") := rA
    out.byte("b") := rB
    out.short("x") := rX
    out.short("y") := rY
    out.short("i") := rI
    out.short("d") := rD
    out.short("sp") := sp
    out.short("rp") := rp
    out.short("pc") := pc
    out.short("ra") := resetAddr
    out.short("ba") := brkAddr
    out.short("f") := flags
    out.boolean("t") := _timeout
    out.short("bo") := busOffset
    out.boolean("be") := busEnabled
  }

  override def deserialize(in: NBTCompoundLike): Unit = {
    rA = in.short("a")
    rB = in.byte("b")
    rX = in.short("x")
    rY = in.short("y")
    rI = in.short("i")
    rD = in.short("d")
    sp = in.short("sp")
    rp = in.short("rp")
    pc = in.short("pc")
    resetAddr = in.short("ra")
    brkAddr = in.short("ba")
    flags = in.short("f")
    _timeout = in.boolean("t")
    busOffset = in.short("bo")
    busEnabled = in.boolean("be")
  }

  override def timeout(): Unit = _timeout = true

  override def clearTimeout(): Unit = _timeout = false

  override def isTimedOut: Boolean = _timeout

  override def insnBufferSize: Int = 0x10000

  override def insnGain: Int = 0x400

  override def next(): Unit = pc1() match {
    case 0x00 => // brk
      push2(pc)
      push1(flags)
      pc = brkAddr
    case 0x01 => // ora (ind, x)
      i.ora(pc2IX())
    case 0x02 => // nxt
      pc = peek2(rI)
      rI += 2
    case 0x03 => // ora r, S
      i.ora(peekM(pc1S()))
    case 0x04 => // tsb zp
      i.tsb(peekM(pc1()))
    case 0x05 => // ora zp
      i.ora(peekM(pc1()))
    case 0x08 => // php
      push1(flags)
    case 0x09 => // ora #
      i.ora(pcM())
    case 0x0B => // rhi
      pushr2(rI)
    case 0x0F => // mul zp
      i.mul(peekM(pc1()))
    case 0x18 => // clc
      ↓(C)
    case 0x1A => // inc a
      rA += 1
      sNZ(rA)
    case 0x20 => // jsr abs
      push2(pc)
      pc = pc2()
    case 0x22 => // ent
      pushr2(rI)
      rI = pc + 2
      pc = pc2()
    case 0x23 => // and r, S
      i.and(peekM(pc1S()))
    case 0x28 => // plp
      setFlags(pop1())
    case 0x2A => // rol a
      rA = i.rol(rA)
    case 0x2B => // rli
      rI = popr2()
      sNXZ(rI)
    case 0x30 => // bmi rel
      i.bra(pc1(), N)
    case 0x3D => // and abs, x
      i.and(peekM(pc2X()))
    case 0x38 => // sec
      ↑(C)
    case 0x3A => // dec a
      rA -= 1
      sNZ(rA)
    case 0x3F => // mul abs, x
      i.mul(peekM(pc2X()))
    case 0x42 => // nxa
      rA = peekM(rI)
      rI += (if (M) 1 else 2)
    case 0x43 => // eor r, S
      i.eor(pc1S())
    case 0x45 => // eor zp
      i.eor(peekM(pc1()))
    case 0x48 => // pha
      pushM(rA)
    case 0x49 => // eor #
      i.eor(pcM())
    case 0x4B => // rha
      pushrM(rA)
    case 0x4C => // jmp abs
      pc = pc2()
    case 0x50 => // bvc rel
      i.bra(pc1(), !V)
    case 0x5A => // phy
      pushX(rY)
    case 0x5B => // rhy
      pushrX(rY)
    case 0x5C => // txi
      rI = rX
      sNXZ(rX)
    case 0x60 => // rts
      pc = pop2()
    case 0x63 => // adc r, S
      i.adc(peekM(pc1S()))
    case 0x64 => // stz zp
      i.stz(pc1())
    case 0x65 => // adc zp
      i.adc(peekM(pc1()))
    case 0x68 => // pla
      rA = popM()
      sNZ(rA)
    case 0x69 => // adc #
      i.adc(pcM())
    case 0x6A => // ror a
      rA = i.ror(rA)
    case 0x6B => // rla
      rA = poprM()
      sNZ(rA)
    case 0x6D => // adc abs
      i.adc(peekM(pc2()))
    case 0x70 => // bvs rel
      i.bra(pc1(), V)
    case 0x74 => // stz zp, x
      i.stz(pc1X())
    case 0x7A => // ply
      rY = popX()
      sNXZ(rY)
    case 0x7B => // rly
      rY = poprX()
      sNXZ(rY)
    case 0x7C => // jmp (ind, x)
      pc = pc2IX()
    case 0x7D => // adc abs, x
      i.adc(peekM(pc2X()))
    case 0x7F => // div abs, x
      i.div(peekM(pc2X()))
    case 0x80 => // bra rel
      i.bra(pc1(), true)
    case 0x84 => // sty zp
      i.sty(pc1())
    case 0x85 => // sta zp
      i.sta(pc1())
    case 0x86 => // stx zp
      i.stx(pc1())
    case 0x88 => // dey
      rY -= 1
      sNXZ(rY)
    case 0x89 => // bit #
      Z := !(rA & pcM())
    case 0x8A => // txa
      rA = rX
      sNZ(rA)
    case 0x8B => // txr
      rp = rX
      sNXZ(rX)
    case 0x8C => // sty abs
      i.sty(pc2())
    case 0x8D => // sta abs
      i.sta(pc2())
    case 0x8E => // stx abs
      i.stx(pc2())
    case 0x90 => // bcc rel
      i.bra(pc1(), !C)
    case 0x91 => // sta (ind), y
      i.sta(pc2IY())
    case 0x92 => // sta (ind)
      i.sta(pc2I())
    case 0x93 => // sta (r, S), y
      i.sta(pc2ISY())
    case 0x94 => // sty zp, x
      i.sty(pc1X())
    case 0x95 => // sta zp, x
      i.sta(pc1X())
    case 0x98 => // tya
      rA = rY
    case 0x99 => // sta abs, y
      i.sta(pc2Y())
    case 0x9A => // txs
      sp = rX
    case 0x9C => // stz abs
      i.stz(pc2())
    case 0x9D => // sta abs, x
      i.sta(pc2X())
    case 0x9E => // stz abs, x
      i.stz(pc2X())
    case 0x9F => // sea
      rD = 0
      if ((M && rA.toByte < 0) || (!M && rA < 0)) rD = maskM
    case 0xA0 => // ldy #
      i.ldy(pcX())
    case 0xA2 => // ldx #
      i.ldx(pcX())
    case 0xA3 => // lda r, S
      i.lda(peekM(pc1S()))
    case 0xA5 => // lda zp
      i.lda(peekM(pc1()))
    case 0xA7 => // lda r, R
      i.lda(peekM(pc1R()))
    case 0xA8 => // tay
      rY = rA
      sNXZ(rY)
    case 0xA9 => // lda #
      i.lda(pcM())
    case 0xAA => // tax
      rX = rA
      sNXZ(rX)
    case 0xAD => // lda abs
      i.lda(peekM(pc2()))
    case 0xAE => // ldx abs
      i.ldx(peekM(pc2()))
    case 0xB0 => // bcs rel
      i.bra(pc1(), C)
    case 0xB1 => // lda (ind), y
      i.lda(peekM(pc2IY()))
    case 0xB3 => // lda (r, S), y
      i.lda(peekM(pc2ISY()))
    case 0xB9 => // lda abs, y
      i.lda(peekM(pc2Y()))
    case 0xBA => // tsx
      rX = sp
      sNXZ(rX)
    case 0xBD => // lda abs, x
      i.lda(peekM(pc2X()))
    case 0xC0 => // cpy #
      i.cmpx(rY, pcX())
    case 0xC2 => // rep #
      i.rep(pc1())
    case 0xC3 => // cmp r, S
      i.cmp(rA, peekM(pc1S()))
    case 0xC6 => // dec zp
      i.dec(pc1())
    case 0xC8 => // iny
      rY += 1
      sNXZ(rY)
    case 0xC9 => // cmp #
      i.cmp(rA, pcM())
    case 0xCA => // dex
      rX -= 1
      sNXZ(rX)
    case 0xCB => // wai
      timeout()
    case 0xCF => // pld
      rD = popM()
    case 0xD0 => // bne rel
      i.bra(pc1(), !Z)
    case 0xDA => // phx
      pushX(rX)
    case 0xDB => // stp
      mem.halt()
    case 0xDC => // tix
      rX = rI
      sNXZ(rX)
    case 0xDD => // cmp abs, x
      i.cmp(rA, peekM(pc2X()))
    case 0xDE => // dec abs, x
      i.dec(pc2X())
    case 0xDF => // phd
      pushM(rD)
    case 0xE2 => // sep #
      i.sep(pc1())
    case 0xE3 => // sbc s, R
      i.sbc(peekM(pc1S()))
    case 0xE6 => // inc zp
      i.inc(pc1())
    case 0xEB => // xba
      if (M) {
        val b = rB
        rB = rA
        rA = b
      } else {
        val a = rA << 8
        val b = rA >> 8 & 0xFF
        rA = a | b
      }
    case 0xEE => // inc abs
      i.inc(pc2())
    case 0xEF => // mmu
      i.mmu(pc1())
    case 0xF0 => // beq rel
      i.bra(pc1, Z)
    case 0xFA => // plx
      rX = popX()
      sNXZ(rX)
    case 0xFB => // xce
      if (isup(C) != isup(E)) {
        if (C) {
          ↓(C)
          ↑(E, X)
          if (!M) rB = rA >> 8
          ↑(M)
        } else {
          ↑(C)
          ↓(E)
        }
      }
    case 0xFE => // inc abs, x
      i.inc(pc2X())
    case insn =>
      printf("Invalid opcode: %02x at %04x%n", insn, pc - 1)
      mem.halt()
  }

  // Read from PC

  private def pc1(): Short = peek1 {
    pc += 1
    peek1(pc - 1)
  }

  private def pc2(): Short = pc1() | pc1() << 8

  private def pc2X(): Short = pc2() + rX

  private def pc2Y(): Short = pc2() + rY

  private def pc1S(): Short = pc1() + sp

  private def pc1R(): Short = pc1() + rp

  private def pcM(): Short = if (M) pc1() else pc2()

  private def pcX(): Short = if (X) pc1() else pc2()

  private def pc1X(): Short = (pc1() + rX) & maskX

  private def pc1Y(): Short = (pc1() + rY) & maskX

  private def pcMX(): Short = (pcM() + rX) & maskX

  private def pcXX(): Short = (pcX() + rX) & maskX

  private def pc2I(): Short = peek2(pc2())

  private def pc2IX(): Short = peek2(pc2() + rX)

  private def pc2IY(): Short = peek2(pc2()) + rY

  private def pc2SY(): Short = peek2(pc1S()) + rY

  private def pc2RY(): Short = peek2(pc1R()) + rY

  private def pc2ISY(): Short = peek2(pc1S()) + rY

  // Read from memory address

  private def peek1(addr: Short): Short = {
    // TODO peripheral access
    mem(addr) & 0xFF
  }

  private def peek2(addr: Short): Short = peek1(addr) | peek1(addr + 1) << 8

  private def peekM(addr: Short): Short = peek1(addr) | (if (M) 0 else peek1(addr + 1) << 8)

  private def peekX(addr: Short): Short = peek1(addr) | (if (X) 0 else peek1(addr + 1) << 8)

  // Write to memory address

  private def poke1(addr: Short, b: Byte): Unit = {
    // TODO peripheral access
    mem(addr) = b
  }

  private def poke2(addr: Short, s: Short): Unit = {
    poke1(addr, s)
    poke1(addr + 1, s >> 8)
  }

  private def pokeM(addr: Short, s: Short): Unit = if (M) poke1(addr, s) else poke2(addr, s)

  private def pokeX(addr: Short, s: Short): Unit = if (X) poke1(addr, s) else poke2(addr, s)

  // Parameter stack ops

  private def push1(b: Byte): Unit = {
    poke1(sp, b)
    sp -= 1
  }

  private def push2(s: Short): Unit = {
    push1(s >> 8)
    push1(s)
  }

  private def pushM(s: Short): Unit = {
    if (M) push1(s)
    else push2(s)
  }

  private def pushX(s: Short): Unit = {
    if (X) push1(s)
    else push2(s)
  }

  private def pop1(): Short = {
    sp += 1
    peek1(sp)
  }

  private def pop2(): Short = {
    pop1() | pop1() << 8
  }

  private def popM(): Short = {
    if (M) pop1()
    else pop2()
  }

  private def popX(): Short = {
    if (X) pop1()
    else pop2()
  }

  // Return stack ops

  private def pushr1(b: Byte): Unit = {
    poke1(rp, b)
    rp -= 1
  }

  private def pushr2(s: Short): Unit = {
    pushr1(s >> 8)
    pushr1(s)
  }

  private def pushrM(s: Short): Unit = {
    if (M) pushr1(s)
    else pushr2(s)
  }

  private def pushrX(s: Short): Unit = {
    if (X) pushr1(s)
    else pushr2(s)
  }

  private def popr1(): Short = {
    rp += 1
    peek1(rp)
  }

  private def popr2(): Short = {
    popr1() | popr1() << 8
  }

  private def poprM(): Short = {
    if (M) popr1()
    else popr2()
  }

  private def poprX(): Short = {
    if (X) popr1()
    else popr2()
  }

  // Masks

  private def maskM: Short = if (M) 0xFF else 0xFFFF

  private def negM: Short = if (M) 0x80 else 0x8000

  private def maskX: Short = if (X) 0xFF else 0xFFFF

  private def negX: Short = if (X) 0x80 else 0x8000

  // Set flags based on value

  private def sNZ(data: Short): Unit = {
    Z := data == 0
    N := data & negM
  }

  private def sNXZ(data: Short): Unit = {
    Z := data == 0
    N := data & negX
  }

  private def sNZC(data: Short): Unit = {
    C := data >= 0
    sNZ(data)
  }

  private def sNXZC(data: Short): Unit = {
    C := data >= 0
    sNXZ(data)
  }

  // BCD conversion

  private def toBCD(s: Short): Short = {
    val i = s & 0xFFFF
    Integer.parseInt(i.toString, 16)
  }

  private def fromBCD(s: Short): Option[Short] = {
    val str = (s & 0xFFFF).toHexString
    if ("abcdef".forall(c => !str.contains(c))) Some(str.toInt)
    else None
  }

  // Instruction helpers

  private object i {
    def adc(data: Short): Unit = {
      // TODO: implement BCD addition
      var i = rA + data
      if (C) i += 1
      C := i > maskM
      V := (data ^ i) & (rA ^ i) & negM
      rA = i
      sNZ(rA)
    }

    def sbc(data: Short): Unit = {
      // TODO: implement BCD subtraction
      var i = rA - data
      if (!C) i -= 1
      C := (i & maskM + 1) == 0
      V := (maskM - data ^ i) & (rA ^ i) & negM
      rA = i
      sNZ(rA)
    }

    def inc(addr: Short): Unit = {
      val data = peekM(addr) + 1
      pokeM(addr, data)
      sNZ(data)
    }

    def dec(addr: Short): Unit = {
      val data = peekM(addr) - 1
      pokeM(addr, data)
      sNZ(data)
    }

    def mul(data: Short): Unit = {
      if (C) {
        if (M) {
          val c: Int = (rA & 0xFF) * (data & 0xFF)
          rA = c
          rD = c >> 8
          N := false
          Z := c == 0
          V := c & 0xFFFF0000
        } else {
          val c: Long = (rA & 0xFFFF) * (data & 0xFFFF)
          rA = c
          rD = c >> 16
          N := false
          Z := c == 0
          V := c & 0xFFFFFFFF00000000L
        }
      } else {
        if (M) {
          val c: Int = rA * data
          rA = c
          rD = c >> 8
          N := c < 0
          Z := c == 0
          V := c & 0xFFFF0000
        } else {
          val c: Long = rA * data
          rA = c
          rD = c >> 16
          N := c < 0
          Z := c == 0
          V := c & 0xFFFFFFFF00000000L
        }
      }
    }

    def div(data: Short): Unit = {
      if (data == 0) {
        ↑(V)
        rA = 0
        rD = 0
        sNZ(0)
      } else {
        if (C) {
          if (M) {
            val a = (rA | rD << 8) & 0xFFFF
            rA = a / data
            rD = a % data
          } else {
            val a = (rA | rD << 16) & 0xFFFFFFFFL
            rA = a / data
            rD = a % data
          }
        } else {
          if (M) {
            val a: Short = rA | rD << 8
            val b: Byte = data
            rA = a / b
            rD = a % b
          } else {
            val a = rA | rD << 16
            rA = a / data
            rD = a % data
          }
        }
        sNZ(rA)
        V := rD
      }
    }

    def and(data: Short): Unit = {
      rA &= data
      sNZ(rA)
    }

    def ora(data: Short): Unit = {
      rA |= data
      sNZ(rA)
    }

    def eor(data: Short): Unit = {
      rA ^= data
      sNZ(rA)
    }

    def rol(data: Short): Short = {
      val i = data << 1 | (if (C) 1 else 0)
      C := data & negM
      sNZ(i)
      i
    }

    def ror(data: Short): Short = {
      val i = data >> 1 | (if (C) negM else 0)
      C := data & 1
      sNZ(i)
      i
    }

    def tsb(data: Short): Unit = {
      Z := data & rA
      rA |= data
    }

    def bra(off: Byte, b: Boolean): Unit = if (b) pc += off

    def stz(addr: Short): Unit = {
      poke1(addr, 0)
      if (!M) poke1(addr + 1, 0)
    }

    def sta(addr: Short): Unit = pokeM(addr, rA)

    def stx(addr: Short): Unit = pokeX(addr, rX)

    def sty(addr: Short): Unit = pokeX(addr, rY)

    def lda(data: Short): Unit = {
      rA = data
      sNZ(rA)
    }

    def ldx(data: Short): Unit = {
      rX = data
      sNXZ(rX)
    }

    def ldy(data: Short): Unit = {
      rY = data
      sNXZ(rY)
    }

    def cmp(a: Short, b: Short): Unit = {
      sNZC(a - b)
    }

    def cmpx(a: Short, b: Short): Unit = {
      sNXZC(a - b)
    }

    def sep(data: Byte): Unit = setFlags(flags | data)

    def rep(data: Byte): Unit = setFlags(flags & ~data)

    def mmu(data: Byte): Unit = data match {
      case 0x00 => mem.targetBus = rA
      case 0x80 => rA = mem.targetBus & 0xFF

      case 0x01 => busOffset = rA
      case 0x81 => rA = busOffset

      case 0x02 => busEnabled = true
      case 0x82 => busEnabled = false

      case 0x03 => mem.allowWrite = true
      case 0x04 => mem.allowWrite = false

      case 0x05 => brkAddr = rA
      case 0x85 => rA = brkAddr

      case 0x06 => resetAddr = rA
      case 0x86 => rA = resetAddr
    }

  }

  // Misc

  private object hidden {
    var rA: Short = 0
    var rD: Short = 0
    var rX: Short = 0
    var rY: Short = 0
  }

  // Flags stuff

  def setFlags(b: Byte): Unit = {
    ↓(~(E.v | M.v))
    flags |= b & Set(N, V, D, I, Z, C).map(_.v).reduce((i1, i2) => i1 | i2)
    if (!E) {
      flags |= X.v & b
      if (X) {
        rX &= 0xFF
        rY &= 0xFF
      }
      if (M ^ (b & M.v)) {
        if (M) {
          rA |= rB << 8
        } else {
          rB = rA >> 8
          rA &= 0xFF
        }
      }
    }
  }

  sealed class Flag(val v: Short) {
    def up(): Unit = ↑(v)

    def down(): Unit = ↓(v)

    def :=(x: Boolean): Unit = if (x) up() else down()
  }

  implicit def isup(x: Flag): Boolean = (flags & x.v) == x.v

  object E extends Flag(256)

  object N extends Flag(128)

  object V extends Flag(64)

  object M extends Flag(32)

  object X extends Flag(16)

  object D extends Flag(8)

  object I extends Flag(4)

  object Z extends Flag(2)

  object C extends Flag(1)

  def ↑(f: Short): Unit = flags |= f

  def ↑(f: Flag*): Unit = f.foreach(_.up())

  def ↓(f: Short): Unit = flags &= ~f

  def ↓(f: Flag*): Unit = f.foreach(_.down())

}
