package zscale

import Chisel._
import junctions._

class DDRIO extends Bundle
{
    val cmd = new Bundle {
        val en = Bool(OUTPUT)
        val instr = UInt(OUTPUT, 3)
        val bl = UInt(OUTPUT, 6)
        val byte_addr = UInt(OUTPUT, 30)
        val empty = Bool(INPUT)
        val full = Bool(INPUT)
    }
    val wr = new Bundle {
        val en = Bool(OUTPUT)
        val mask = UInt(OUTPUT, 4)
        val data = UInt(OUTPUT, 32)
        val full = Bool(INPUT)
        val empty = Bool(INPUT)
        val count = UInt(INPUT, 7)
        val underrun = Bool(INPUT)
        val error = Bool(INPUT)
    }
    val rd = new Bundle {
        val en = Bool(OUTPUT)
        val data = UInt(INPUT, 32)
        val full = Bool(INPUT)
        val empty = Bool(INPUT)
        val count = UInt(INPUT, 7)
        val overflow = Bool(INPUT)
        val error = Bool(INPUT)
    }
}

class HASTIDDR extends Module with HASTIConstants
{
    val io = new Bundle {
        val in = new HASTISlaveIO
        val out = new DDRIO
    }

    val DDRINSTR_WRITE = UInt(0, width=3)
    val DDRINSTR_READ  = UInt(1, width=3)
    val DDRINSTR_WRITEPRECHG = UInt(2, width=3)
    val DDRINSTR_READPRECHG = UInt(3, width=3)
    val DDRINSTR_REFRESH = UInt(4, width=3)

    val s_ready :: s_wr_wait :: s_rd_wait :: Nil = Enum(UInt(), 3)
    val state = Reg(init = s_ready)

    // Never respond with an error
    io.in.hresp := HRESP_OKAY

    // Read data out is the registered output of the read fifo
    val rdata = Reg(next = io.out.rd.data)
    io.in.hrdata := rdata

    // Write data in connects directly to the write fifo
    io.out.wr.data := io.in.hwdata

    // Register the command to be sent to the ddr controller
    val cmden = Reg(init = Bool(false))
    val wren = Reg(init = Bool(false))
    val rden = Reg(init = Bool(false))
    val instr = Reg(init = DDRINSTR_WRITE)
    val byte_addr = Reg(init = UInt(0, width=30))
    val wsize = Reg(UInt(width = SZ_HSIZE))

    // Keep the burst length at 1 word.
    val bl = Reg(init = UInt(0, width=6))

    // Zero-out the two lsbs of the address.
    io.out.cmd.byte_addr := Cat(byte_addr(29, 2), UInt(0, width=2))

    // Lookup table to get the write mask bits based on the hasti xfer size.
    // Note that the ddr control uses an inverted write mask, 0 means write.
    val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
            UInt(0) -> Bits(0x1),
            UInt(1) -> Bits(0x3)))
    val wmask_shift = wmask_lut << byte_addr(1,0)
    io.out.wr.mask := ~wmask_shift

    io.out.cmd.en := cmden
    io.out.cmd.instr := instr
    io.out.cmd.bl := bl
    io.out.wr.en := wren
    io.out.rd.en := rden

    when (state === s_ready) {
        io.in.hreadyout := Bool(true)
        cmden := Bool(false)
        rden := Bool(false)
        when (io.in.hreadyin && io.in.hsel && (io.in.htrans === HTRANS_NONSEQ)) {
            // Grab the address.
            byte_addr := io.in.haddr
            // Keep the burst length = 1 transfer
            bl := UInt(0)
            when (io.in.hwrite) {
                // Start a write transaction.
                instr := DDRINSTR_WRITE
                // Write to the write fifo in the next cycle.
                wren := Bool(true)
                // Register the size of the write to generate the write mask for the write fifo.
                wsize := io.in.hsize
                // Go to the wait state for the transaction to complete
                state := s_wr_wait
            }
            .otherwise {
                // Start a read transaction.
                instr := DDRINSTR_READ
                // Issue the read command in the next cycle.
                cmden := Bool(true)
                // Go to the wait state for the transaction to complete
                state := s_rd_wait
            }
        }
    }
    .elsewhen (state === s_wr_wait) {
        // In the write wait state. This is the data cycle of the hasti transaction.
        // Insert one hasti wait state so that we have time to write to the
        // ddr controller write fifo and then the command fifo.
        io.in.hreadyout := Bool(false)
        // Turn off the write queue write.
        wren := Bool(false)
        // Turn on the command queue write.
        cmden := Bool(true)
        // Go back to the ready state
        state := s_ready
    }
    .otherwise {
        // In the read wait state. Stay here until the ddr returns data.
        io.in.hreadyout := Bool(false)
        // Turn off the command queue write.
        cmden := Bool(false)
        unless (io.out.rd.empty) {
            rden := Bool(true)
            state := s_ready
        }
    }

}
