// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._
import rocket.Instructions._

abstract trait PCUParameters extends UsesParameters
{
  val memAddrBits = params(MIFAddrBits)
  val memDataBits = params(MIFDataBits)
  val memDataBeats = params(MIFDataBeats)

  val spadSize = 8192
  val spadWidth = memDataBits
  val spadWordBytes = spadWidth / 8
  val spadDepth = spadSize / spadWordBytes
  val spadByteMaskBits = spadWordBytes
  val spadAddrBits = log2Up(spadDepth)
  val spadTagBits = 1

  val xprLen = 32
  val addrBits = log2Up(spadSize)
  val coreInstBits = params(CoreInstBits) 
  val coreInstBytes = coreInstBits / 8
}

class MemIOSplitter extends Module with PCUParameters
{
  val io = new Bundle {
    val hub = new MemPipeIO().flip
    val serdes = new MemPipeIO
    val spad = new MemPipeIO
  }

  val addr_msb = io.hub.req_cmd.bits.addr(memAddrBits-1)
  val asid_serdes = addr_msb === UInt(0)

  io.serdes.req_cmd.valid := io.hub.req_cmd.valid && asid_serdes
  io.spad.req_cmd.valid := io.hub.req_cmd.valid && !asid_serdes
  io.hub.req_cmd.ready := asid_serdes && io.serdes.req_cmd.ready || !asid_serdes && io.spad.req_cmd.ready
  io.serdes.req_cmd.bits := io.hub.req_cmd.bits
  io.spad.req_cmd.bits := io.hub.req_cmd.bits

  io.serdes.req_data.valid := io.hub.req_data.valid
  io.spad.req_data.valid := io.hub.req_data.valid
  io.hub.req_data.ready := io.serdes.req_data.ready || io.spad.req_data.ready
  io.serdes.req_data.bits := io.hub.req_data.bits
  io.spad.req_data.bits := io.hub.req_data.bits

  io.hub.resp.valid := io.serdes.resp.valid || io.spad.resp.valid
  io.hub.resp.bits := Mux(io.spad.resp.valid, io.spad.resp.bits, io.serdes.resp.bits)
}

class ScratchPadRequest extends Bundle with PCUParameters
{
  val addr = UInt(width = spadAddrBits)
  val rw = Bool()
  val wmask = Bits(width = spadByteMaskBits)
  val data = Bits(width = spadWidth)
  val tag = UInt(width = spadTagBits)
}

class ScratchPadResponse extends Bundle with PCUParameters
{
  val data = Bits(width = spadWidth)
  val tag = UInt(width = spadTagBits)
}

class ScratchPadIO extends Bundle with PCUParameters
{
  val req = Decoupled(new ScratchPadRequest)
  val resp = Valid(new ScratchPadResponse).flip
}

class ScratchPad extends Module with PCUParameters
{
  val io = new Bundle {
    val cpu = new ScratchPadIO().flip
    val mem = new MemPipeIO().flip
  }

  val s_idle :: s_read :: s_write :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  val ram = Mem(Bits(width = spadWidth), spadDepth, seqRead = true)
  val raddr = Reg(UInt())
  val rtag = Reg(UInt())
  val rdata = Bits()
  val wen = Bool()
  val waddr = UInt()
  val wdata = Bits()
  val wmask = Bits()

  // ram read port
  rdata := ram(raddr)

  // ram write port
  when (wen) {
    ram.write(waddr, wdata, wmask)
  }

  wen := Bool(false)
  waddr := io.cpu.req.bits.addr
  wdata := io.cpu.req.bits.data
  wmask := FillInterleaved(8, io.cpu.req.bits.wmask)

  io.cpu.req.ready := (state === s_idle)

  when (state === s_idle && io.cpu.req.valid) {
    when (io.cpu.req.bits.rw) {
      wen := Bool(true)
    } .otherwise {
      raddr := io.cpu.req.bits.addr
      rtag := io.cpu.req.bits.tag
    }
  }

  io.cpu.resp.valid := Reg(next = io.cpu.req.fire() && !io.cpu.req.bits.rw)
  io.cpu.resp.bits.data := rdata
  io.cpu.resp.bits.tag := rtag

  val addr = Reg(UInt())
  val tag = Reg(UInt())
  val cnt = Reg(UInt(width = log2Up(memDataBeats)))

  when (state === s_idle && io.mem.req_cmd.valid) {
    state := Mux(io.mem.req_cmd.bits.rw, s_write, s_read)
    addr := io.mem.req_cmd.bits.addr
    tag := io.mem.req_cmd.bits.tag
    cnt := UInt(0)
  }
  when (state === s_read) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(memDataBeats-1)) {
      state := s_idle
    }
  }
  when (state === s_write && io.mem.req_data.valid) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(memDataBeats-1)) {
      state := s_idle
    }
  }

  io.mem.req_cmd.ready := (state === s_idle)
  io.mem.req_data.ready := (state === s_write)

  when (state === s_read) {
    raddr := Cat(addr, cnt)
  }
  when (io.mem.req_data.fire()) {
    wen := Bool(true)
    waddr := Cat(addr, cnt)
    wdata := io.mem.req_data.bits.data
    wmask := FillInterleaved(spadWidth, Bool(true))
  }

  io.mem.resp.valid := Reg(next = state === s_read)
  io.mem.resp.bits.data := rdata
  io.mem.resp.bits.tag := tag
}

class InstMemReq extends Bundle with PCUParameters
{
  val addr = UInt(width = addrBits)
}

class InstMemResp extends Bundle with PCUParameters
{
  val inst = Bits(width = coreInstBits)
}

class InstMemIO extends Bundle with PCUParameters
{
  val req = Valid(new InstMemReq)
  val resp = Valid(new InstMemResp).flip
  val invalidate = Decoupled(Bool())
}

class InstLineBuffer extends Module with PCUParameters
{
  val io = new Bundle {
    val cpu = new InstMemIO().flip
    val mem = new ScratchPadIO
  }

  val s_idle :: s_requested :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val nInst = spadWordBytes / coreInstBytes
  val nIdx = log2Up(nInst) + log2Up(coreInstBytes)
  val nTag = addrBits - nIdx

  val req_inst_onehot = UInt(1) << io.cpu.req.bits.addr(nIdx-1, log2Up(coreInstBytes))
  val req_tag = io.cpu.req.bits.addr(addrBits-1, nIdx)

  val tag = Reg(UInt(width = nTag))
  val tag_valid = Reg(init = Bool(false))
  val tag_hit = tag_valid && (tag === req_tag)

  val service_hit = (state === s_idle) && tag_hit
  val service_nohit = (state === s_idle) && !tag_hit

  val line = Vec.fill(nInst){Reg(Bits(width = coreInstBits))}

  // front side of line buffer
  io.cpu.resp.valid := service_hit
  io.cpu.resp.bits.inst := Mux1H(req_inst_onehot, line)
  io.cpu.invalidate.ready := (state === s_idle)

  when (io.cpu.invalidate.fire()) {
    tag_valid := Bool(false)
  }

  // back side of line buffer
  when (service_nohit && io.cpu.req.valid && io.mem.req.ready) {
    state := s_requested
    tag_valid := Bool(false)
    tag := req_tag
  }

  io.mem.req.valid := !this.reset && service_nohit && io.cpu.req.valid
  io.mem.req.bits.addr := io.cpu.req.bits.addr >> UInt(nIdx)
  io.mem.req.bits.rw := Bool(false)

  when (io.mem.resp.valid) {
    state := s_idle
    tag_valid := Bool(true)
    (0 until nInst).map(i => line(i) := io.mem.resp.bits.data((i+1)*coreInstBits-1, i*coreInstBits))
  }
}

class CtrlDpathIO extends Bundle with PCUParameters
{
  val j = Bool(OUTPUT)
  val br = Bool(OUTPUT)
  val sel_alu1 = UInt(OUTPUT, 2)
  val sel_alu2 = UInt(OUTPUT, 3)
  val sel_imm = UInt(OUTPUT, 3)
  val fn_alu = UInt(OUTPUT, SZ_ALU_FN)
  val wen = Bool(OUTPUT)
  val mem_valid = Bool(OUTPUT)
  val mem_rw = Bool(OUTPUT)
  val mem_type = UInt(OUTPUT, MT_SZ)
  val mul_valid = Bool(OUTPUT)

  val stallf = Bool(OUTPUT)
  val killf = Bool(OUTPUT)
  val stalldx = Bool(OUTPUT)
  val killdx = Bool(OUTPUT)

  val inst = Bits(INPUT, coreInstBits)
  val br_taken = Bool(INPUT)
  val mul_ready = Bool(INPUT)
  val clear_sb = Bool(INPUT)
}

class Control extends Module with PCUParameters
{
  val io = new Bundle {
    val dpath = new CtrlDpathIO
    val imem = new InstMemIO
    val dmem = new ScratchPadIO
  }

  io.imem.req.valid := Bool(true)

  val id_valid = Reg(init = Bool(false))

  when (!io.dpath.stalldx) {
    id_valid := !io.dpath.killf
  }

  val cs = DecodeLogic(io.dpath.inst,
                //  val j br f.i si s_alu1   s_alu2   imm     fn       wen sb mem rw mtype  mul
                //   |  |  |  |  |  |        |        |       |          |  |  |  |  |      |
                List(N, X, X, X, X, A1_X,    A2_X,    IMM_X,  FN_X,      X, X, X, X, MT_X,  X), Array(
      LUI->     List(Y, N, N, N, N, A1_ZERO, A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, X, MT_X,  N),
      AUIPC->   List(Y, N, N, N, N, A1_PC,   A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, X, MT_X,  N),

      JAL->     List(Y, Y, N, N, N, A1_PC,   A2_FOUR, IMM_UJ, FN_ADD,    Y, N, N, X, MT_X,  N),
      JALR->    List(Y, Y, N, N, N, A1_PC,   A2_FOUR, IMM_I,  FN_ADD,    Y, N, N, X, MT_X,  N),

      BEQ->     List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SEQ,    N, N, N, X, MT_X,  N),
      BNE->     List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SNE,    N, N, N, X, MT_X,  N),
      BLT->     List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLT,    N, N, N, X, MT_X,  N),
      BLTU->    List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLTU,   N, N, N, X, MT_X,  N),
      BGE->     List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGE,    N, N, N, X, MT_X,  N),
      BGEU->    List(Y, N, Y, N, N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGEU,   N, N, N, X, MT_X,  N),

      LB->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_B,  N),
      LBU->     List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_BU, N),
      LH->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_H,  N),
      LHU->     List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_HU, N),
      LW->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_W,  N),
      SB->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_B,  N),
      SH->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_H,  N),
      SW->      List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_W,  N),

      ADDI->    List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    Y, N, N, X, MT_X,  N),
      SLTI->    List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLT,    Y, N, N, X, MT_X,  N),
      SLTIU->   List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLTU,   Y, N, N, X, MT_X,  N),
      XORI->    List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_XOR,    Y, N, N, X, MT_X,  N),
      ORI->     List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_OR,     Y, N, N, X, MT_X,  N),
      ANDI->    List(Y, N, N, N, N, A1_RS1,  A2_IMM,  IMM_I,  FN_AND,    Y, N, N, X, MT_X,  N),
      SLLI->    List(Y, N, N, N, Y, A1_RS1,  A2_IMM,  IMM_I,  FN_SL,     Y, N, N, X, MT_X,  N),
      SRLI->    List(Y, N, N, N, Y, A1_RS1,  A2_IMM,  IMM_I,  FN_SR,     Y, N, N, X, MT_X,  N),
      SRAI->    List(Y, N, N, N, Y, A1_RS1,  A2_IMM,  IMM_I,  FN_SRA,    Y, N, N, X, MT_X,  N),

      ADD->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_ADD,    Y, N, N, X, MT_X,  N),
      SUB->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SUB,    Y, N, N, X, MT_X,  N),
      SLL->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SL,     Y, N, N, X, MT_X,  N),
      SLT->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLT,    Y, N, N, X, MT_X,  N),
      SLTU->    List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLTU,   Y, N, N, X, MT_X,  N),
      XOR->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_XOR,    Y, N, N, X, MT_X,  N),
      SRL->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SR,     Y, N, N, X, MT_X,  N),
      SRA->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_SRA,    Y, N, N, X, MT_X,  N),
      OR->      List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_OR,     Y, N, N, X, MT_X,  N),
      AND->     List(Y, N, N, N, N, A1_RS1,  A2_RS2,  IMM_X,  FN_AND,    Y, N, N, X, MT_X,  N),

      FENCE->   List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N), // nop
      FENCE_I-> List(Y, N, N, Y, N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N),

      MUL->     List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_MUL,    N, Y, N, X, MT_X,  Y),
      MULH->    List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_MULH,   N, Y, N, X, MT_X,  Y),
      MULHU->   List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_MULHU,  N, Y, N, X, MT_X,  Y),
      MULHSU->  List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_MULHSU, N, Y, N, X, MT_X,  Y),
      DIV->     List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_DIV,    N, Y, N, X, MT_X,  Y),
      DIVU->    List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_DIVU,   N, Y, N, X, MT_X,  Y),
      REM->     List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_REM,    N, Y, N, X, MT_X,  Y),
      REMU->    List(Y, N, N, N, N, A1_X,    A2_X,    IMM_X,  FN_REMU,   N, Y, N, X, MT_X,  Y)
    ))

  val (_id_inst_valid: Bool) :: (id_j: Bool) :: (id_br: Bool) :: (id_fence_i: Bool) :: (id_shift_imm: Bool) :: cs1 = cs
  val id_sel_alu1 :: id_sel_alu2 :: id_sel_imm :: id_fn_alu :: (id_wen: Bool) :: cs2 = cs1
  val (id_set_sb: Bool) :: (id_mem_valid: Bool) :: (id_mem_rw: Bool) :: id_mem_type :: (id_mul_valid: Bool) :: Nil = cs2

  val sb_stall = Reg(init = Bool(false))
  val id_shift_valid = !id_shift_imm || io.dpath.inst(25) === Bits(0) // checking whether shamt's bit 6 is a zero
  val id_inst_valid = _id_inst_valid && id_shift_valid // this is because we're using RV64I's shift instruction format
  val id_ok = !sb_stall && id_valid && id_inst_valid

  io.dpath.j := id_ok && id_j
  io.dpath.br := id_ok && id_br
  io.dpath.sel_alu1 := id_sel_alu1
  io.dpath.sel_alu2 := id_sel_alu2
  io.dpath.sel_imm := id_sel_imm
  io.dpath.fn_alu := id_fn_alu
  io.dpath.wen := id_ok && id_wen
  io.dpath.mem_valid := id_ok && id_mem_valid
  io.dpath.mem_rw := id_mem_rw
  io.dpath.mem_type := id_mem_type
  io.dpath.mul_valid := id_ok && id_mul_valid

  io.imem.invalidate.valid := id_ok && id_fence_i
  io.dmem.req.valid := io.dpath.mem_valid

  when (!io.dpath.killdx && id_set_sb) {
    sb_stall := Bool(true)
  }
  when (io.dpath.clear_sb) {
    sb_stall := Bool(false)
  }

  val fence_stall = io.imem.invalidate.valid && !io.imem.invalidate.ready
  val mem_stall = io.dmem.req.valid && !io.dmem.req.ready
  val mul_stall = io.dpath.mul_valid && !io.dpath.mul_ready

  val br_taken = io.dpath.br && io.dpath.br_taken
  io.dpath.stallf := !io.imem.resp.valid && !br_taken || io.imem.invalidate.valid || io.dpath.stalldx
  io.dpath.killf := !io.imem.resp.valid || io.imem.invalidate.valid || io.dpath.j || br_taken
  io.dpath.stalldx := sb_stall || fence_stall || mem_stall || mul_stall
  io.dpath.killdx := !id_ok || io.dpath.stalldx
}

// partially copied from Rocket's ALU
class ALU extends Module with PCUParameters
{
  val io = new Bundle {
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in1 = Bits(INPUT, xprLen)
    val in2 = Bits(INPUT, xprLen)
    val out = Bits(OUTPUT, xprLen)
    val adder_out = Bits(OUTPUT, xprLen)
  }

  // ADD, SUB
  val sum = io.in1 + Mux(isSub(io.fn), -io.in2, io.in2)

  // SLT, SLTU
  val cmp = cmpInverted(io.fn) ^
    Mux(cmpEq(io.fn), sum === UInt(0),
    Mux(io.in1(xprLen-1) === io.in2(xprLen-1), sum(xprLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xprLen-1), io.in1(xprLen-1))))

  // SLL, SRL, SRA
  val shamt = io.in2(4,0)
  val shin_r = io.in1
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xprLen-1), shin).toSInt >> shamt)(xprLen-1,0)
  val shout_l = Reverse(shout_r)

  io.out :=
    Mux(io.fn === FN_ADD || io.fn === FN_SUB,  sum,
    Mux(io.fn === FN_SR  || io.fn === FN_SRA,  shout_r,
    Mux(io.fn === FN_SL,                       shout_l,
    Mux(io.fn === FN_AND,                      io.in1 & io.in2,
    Mux(io.fn === FN_OR,                       io.in1 | io.in2,
    Mux(io.fn === FN_XOR,                      io.in1 ^ io.in2,
                /* all comparisons */          cmp))))))

  io.adder_out := sum
}

class Datapath extends Module with PCUParameters
{
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip
    val imem = new InstMemIO
    val dmem = new ScratchPadIO
  }

  val pc = Reg(init = UInt(0, addrBits))
  val id_br_target = UInt()

  when (!io.ctrl.stallf) {
    pc := Mux(io.ctrl.j || io.ctrl.br && io.ctrl.br_taken, id_br_target,
              pc + UInt(4))
  }

  io.imem.req.bits.addr := pc

  val id_pc = Reg(UInt(width = addrBits))
  val id_inst = Reg(Bits(width = coreInstBits))

  // !io.ctrl.killf is a power optimization (clock-gating)
  when (!io.ctrl.stalldx && !io.ctrl.killf) {
    id_pc := pc
    id_inst := io.imem.resp.bits.inst
  }

  // similar to Rocket's RF
  // this one doesn't have a write->read bypass
  class RegFile {
    private val rf = Mem(Bits(width = xprLen), 31)
    def read(addr: UInt) = Mux(addr != UInt(0), rf(~addr), Bits(0))
    def write(addr: UInt, data: Bits) = {
      when (addr != UInt(0)) {
        rf(~addr) := data
      }
    }
  }

  // copied from Rocket's datapath
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(31).toSInt
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel != IMM_U && sel != IMM_UJ, sign, inst(19,12).toSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, SInt(0),
              Mux(sel === IMM_UJ, inst(20).toSInt,
              Mux(sel === IMM_SB, inst(7).toSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), Bits(0))))
    
    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).toSInt
  }

  val rf = new RegFile
  val id_addr = Vec(id_inst(19, 15), id_inst(24,20))
  val id_rs = id_addr.map(rf.read _)
  val id_rd = id_inst(11, 7)
  val id_imm = imm(io.ctrl.sel_imm, id_inst)

  // ALU
  val alu = Module(new ALU)
  alu.io.fn := io.ctrl.fn_alu
  alu.io.in1 := MuxLookup(io.ctrl.sel_alu1, SInt(0), Seq(
      A1_RS1 -> id_rs(0).toSInt,
      A1_PC -> id_pc.toSInt
    ))
  alu.io.in2 := MuxLookup(io.ctrl.sel_alu2, SInt(0), Seq(
      A2_FOUR -> SInt(4),
      A2_RS2 -> id_rs(1).toSInt,
      A2_IMM -> id_imm
    ))

  // BRANCH TARGET
  // jalr only takes rs1, jump and branches take pc
  id_br_target := Mux(io.ctrl.j && io.ctrl.sel_imm === IMM_I, id_rs(0), id_pc) + id_imm

  // DMEM
  class StoreGen32(typ: Bits, addr: Bits, dat: Bits) {
    val byte = typ === MT_B || typ === MT_BU
    val half = typ === MT_H || typ === MT_HU
    val word = typ === MT_W
    val nw = log2Up(spadWordBytes)
    def mask =
      Mux(byte, Bits(  1) <<     addr(nw-1,0),
      Mux(half, Bits(  3) << Cat(addr(nw-1,1), Bits(0,1)),
      Mux(word, Bits( 15) << Cat(addr(nw-1,2), Bits(0,2)),
                Fill(nw, Bool(true)))))
    def data =
      Mux(byte, Fill(nw*4, dat( 7,0)),
      Mux(half, Fill(nw*2, dat(15,0)),
                Fill(nw,   dat(31,0))))
  }

  class LoadGen32(typ: Bits, addr: Bits, dat: Bits) {
    val t = new StoreGen32(typ, addr, dat)
    val sign = typ === MT_B || typ === MT_H || typ === MT_W

    val word = MuxLookup(addr(t.nw-1,2), Bits(0, 32),
      (0 until spadWordBytes/4).map( i => (UInt(i) -> dat(32*(i+1)-1, 32*i)) ))
    val halfShift = Mux(addr(1), word(31,16), word(15,0))
    val half = Cat(Mux(t.half, Fill(16, sign && halfShift(15)), word(31,16)), halfShift)
    val byteShift = Mux(addr(0), half(15,8), half(7,0))
    val byte = Cat(Mux(t.byte, Fill(24, sign && byteShift(7)), half(31,8)), byteShift)
  }

  val dmem_req_addr = alu.io.adder_out
  val dmem_reg_mem_type = Reg(UInt(width = MT_SZ))
  val dmem_reg_lowaddr = Reg(UInt(width = log2Up(spadWordBytes)))
  val dmem_reg_rd = Reg(UInt(width = 5))
  val dmem_sgen = new StoreGen32(io.ctrl.mem_type, dmem_req_addr, id_rs(1))
  val dmem_lgen = new LoadGen32(dmem_reg_mem_type, dmem_reg_lowaddr, io.dmem.resp.bits.data)

  when (io.ctrl.mem_valid && !io.ctrl.mem_rw) {
    dmem_reg_mem_type := io.ctrl.mem_type
    dmem_reg_lowaddr := dmem_req_addr
    dmem_reg_rd := id_rd
  }

  io.dmem.req.bits.addr := dmem_req_addr >> UInt(log2Up(spadWordBytes))
  io.dmem.req.bits.rw := io.ctrl.mem_rw
  io.dmem.req.bits.wmask := dmem_sgen.mask
  io.dmem.req.bits.data := dmem_sgen.data

  // MUL/DIV
  val muldiv = Module(new MulDiv, {case XprLen => 32})
  muldiv.io.req.valid := io.ctrl.mul_valid
  muldiv.io.req.bits.fn := io.ctrl.fn_alu
  muldiv.io.req.bits.dw := DW_64
  muldiv.io.req.bits.in1 := id_rs(0)
  muldiv.io.req.bits.in2 := id_rs(1)
  muldiv.io.req.bits.tag := id_rd
  muldiv.io.kill := Bool(false)
  muldiv.io.resp.ready := Bool(true)

  // WB
  val waddr = MuxCase(
    id_rd, Array(
      io.dmem.resp.valid -> dmem_reg_rd,
      muldiv.io.resp.valid -> muldiv.io.resp.bits.tag
    ))
  val wdata = MuxCase(
    alu.io.out, Array(
      io.dmem.resp.valid -> dmem_lgen.byte,
      muldiv.io.resp.valid -> muldiv.io.resp.bits.data
    ))

  when (io.ctrl.wen || io.dmem.resp.valid || muldiv.io.resp.valid) {
    rf.write(waddr, wdata)
  }

  // to control
  io.ctrl.inst := id_inst
  io.ctrl.br_taken := alu.io.out(0)
  io.ctrl.mul_ready := muldiv.io.req.ready
  io.ctrl.clear_sb := io.dmem.resp.valid || muldiv.io.resp.valid
}

class MemArbiter extends Module with PCUParameters
{
  val io = new Bundle {
    val imem = new ScratchPadIO().flip
    val dmem = new ScratchPadIO().flip
    val mem = new ScratchPadIO
  }

  io.imem.req.ready := io.mem.req.ready && !io.dmem.req.valid
  io.dmem.req.ready := io.mem.req.ready
  io.mem.req.valid := io.imem.req.valid || io.dmem.req.valid
  io.mem.req.bits := Mux(io.dmem.req.valid, io.dmem.req.bits, io.imem.req.bits)
  io.mem.req.bits.tag := io.dmem.req.valid

  io.imem.resp.valid := io.mem.resp.valid && (io.mem.resp.bits.tag === Bits(0))
  io.dmem.resp.valid := io.mem.resp.valid && (io.mem.resp.bits.tag === Bits(1))
  io.imem.resp.bits := io.mem.resp.bits
  io.dmem.resp.bits := io.mem.resp.bits
}

class Core(resetSignal: Bool = null) extends Module(_reset = resetSignal) with PCUParameters
{
  val io = new Bundle {
    val mem = new ScratchPadIO
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)
  val ibuf = Module(new InstLineBuffer)
  val arb = Module(new MemArbiter)

  ibuf.io.cpu <> ctrl.io.imem
  ibuf.io.cpu <> dpath.io.imem
  ctrl.io.dpath <> dpath.io.ctrl

  arb.io.imem <> ibuf.io.mem
  arb.io.dmem <> ctrl.io.dmem
  arb.io.dmem <> dpath.io.dmem
  io.mem <> arb.io.mem
}

class PCU extends Module with PCUParameters
{
  val io = new Bundle {
    val core_reset = Bool(INPUT)
    val spad = new MemPipeIO().flip
    val scr = new SCRIO
    val scr_busy = Bool(INPUT)
  }

  val core = Module(new Core(resetSignal = io.core_reset))
  val spad = Module(new ScratchPad)

  spad.io.cpu <> core.io.mem
  spad.io.mem <> io.spad
  
  io.scr.wen := Bool(false)
}