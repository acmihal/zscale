package zscale

import Chisel._
import junctions._

class BRAMIO extends Bundle {
    val en = Bool(OUTPUT)
    val we = UInt(OUTPUT, 4)
    val addr = UInt(OUTPUT, 12)
    val din = UInt(OUTPUT, 32)
    val dout = UInt(INPUT, 32)
}

class HASTIBRAM extends Module with HASTIConstants
{
    val io = new Bundle {
        val in = new HASTISlaveIO
        val out = new BRAMIO
    }

    // One wait state is required for writes.
    val s_ready :: s_wr_wait :: Nil = Enum(UInt(), 2)
    val state = Reg(init = s_ready)

    // Registered waddr and wsize for write operations
    val waddr = Reg(UInt(width = SZ_HADDR))
    val wsize = Reg(UInt(width = SZ_HSIZE))

    // BRAM is always enabled
    io.out.en := Bool(true)

    // Never respond with an error
    io.in.hresp := HRESP_OKAY

    // Read data out is directly connected to the bram
    io.in.hrdata := io.out.dout

    // Write data in is directly connected to the bram
    io.out.din := io.in.hwdata

    val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
          UInt(0) -> Bits(0x1),
          UInt(1) -> Bits(0x3)))
    val wmask_shift = wmask_lut << waddr(1,0)

    when (state === s_ready) {
        io.in.hreadyout := Bool(true)
        io.out.we := UInt(0, width=4)
        io.out.addr := io.in.haddr >> UInt(2)
        when (io.in.hreadyin && io.in.hsel && (io.in.htrans === HTRANS_NONSEQ)) {
            when (io.in.hwrite) {
                state := s_wr_wait
                waddr := io.in.haddr
                wsize := io.in.hsize
            }
        }
    }
    .otherwise {
        // In the write wait state.
        io.in.hreadyout := Bool(false)
        io.out.we := wmask_shift
        io.out.addr := waddr >> UInt(2)
        state := s_ready
    }

}

