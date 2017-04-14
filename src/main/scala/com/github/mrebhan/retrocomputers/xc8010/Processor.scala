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

  private var rA: Short = 0
  private var rB: Byte = 0
  private var rX: Short = 0
  private var rY: Short = 0
  private var rI: Short = 0
  private var rD: Short = 0
  private var sp: Short = 0
  private var rp: Short = 0
  private var pc: Short = 0
  private var resetAddr: Short = 0
  private var brkAddr: Short = 0
  private var flags: Short = 0
  private var _timeout: Boolean = false

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
      rA = i & maskM
      sNZ(rA)
    }

    def sbc(data: Short): Unit = {
      // TODO: implement BCD subtraction
      var i = rA - data
      if (!C) i -= 1
      C := (i & maskM + 1) == 0
      V := (maskM - data ^ i) & (rA ^ i) & negM
      rA = i & maskM
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
          rA = c & 0xFF
          rD = (c >> 8) & 0xFF
          N := false
          Z := c == 0
          V := c & 0xFFFF0000
        } else {
          val c: Long = (rA & 0xFFFF) * (data & 0xFFFF)
          rA = c & 0xFFFF
          rD = (c >> 16) & 0xFFFF
          N := false
          Z := c == 0
          V := c & 0xFFFFFFFF00000000L
        }
      } else {
        if (M) {
          val c: Int = rA * data
          rA = c & 0xFF
          rD = (c >> 8) & 0xFF
          N := c < 0
          Z := c == 0
          V := c & 0xFFFF0000
        } else {
          val c: Long = rA * data
          rA = c & 0xFFFF
          rD = (c >> 16) & 0xFFFF
          N := c < 0
          Z := c == 0
          V := c & 0xFFFFFFFF00000000L
        }
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

    def tsb(data: Short): Unit = {
      Z := data & rA
      rA |= data
    }
  }

  // Flags stuff

  sealed class Flag(val v: Short) {
    def up(): Unit = ↑(v)

    def down(): Unit = ↓(v)

    def :=(x: Boolean): Unit = if (x) up() else down()
  }

  implicit def flag2bool(x: Flag): Boolean = (flags & x.v) == x.v

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
