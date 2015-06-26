package zscale

import Chisel._

abstract trait HASTIConstants
{
  val SZ_HTRANS     = 2
  val HTRANS_IDLE   = UInt(0, SZ_HTRANS)
  val HTRANS_BUSY   = UInt(1, SZ_HTRANS)
  val HTRANS_NONSEQ = UInt(2, SZ_HTRANS)
  val HTRANS_SEQ    = UInt(3, SZ_HTRANS)

  val SZ_HBURST     = 3
  val HBURST_SINGLE = UInt(0, SZ_HBURST)
  val HBURST_INCR   = UInt(1, SZ_HBURST)
  val HBURST_WRAP4  = UInt(2, SZ_HBURST)
  val HBURST_INCR4  = UInt(3, SZ_HBURST)
  val HBURST_WRAP8  = UInt(4, SZ_HBURST)
  val HBURST_INCR8  = UInt(5, SZ_HBURST)
  val HBURST_WRAP16 = UInt(6, SZ_HBURST)
  val HBURST_INCR16 = UInt(7, SZ_HBURST)

  val SZ_HRESP      = 1
  val HRESP_OKAY    = UInt(0, SZ_HRESP)
  val HRESP_ERROR   = UInt(1, SZ_HRESP)

  val SZ_HSIZE = 3
  val SZ_HPROT = 3

  // TODO: Parameterize
  val SZ_HADDR = 32
  val SZ_HDATA = 32
}

class HASTIMasterIO extends Bundle
{
  val haddr     = UInt(OUTPUT, SZ_HADDR)
  val hwrite    = Bool(OUTPUT)
  val hsize     = UInt(OUTPUT, SZ_HSIZE)
  val hburst    = UInt(OUTPUT, SZ_HBURST)
  val hprot     = UInt(OUTPUT, SZ_HPROT)
  val htrans    = UInt(OUTPUT, SZ_HTRANS)
  val hmastlock = Bool(OUTPUT)

  val hwdata = Bits(OUTPUT, SZ_HDATA)
  val hrdata = Bits(INPUT, SZ_HDATA)

  val hready = Bool(INPUT)
  val hresp  = UInt(INPUT, SZ_HRESP)
}

class HASTISlaveIO extends Bundle
{
  val haddr     = UInt(INPUT, SZ_HADDR)
  val hwrite    = Bool(INPUT)
  val hsize     = UInt(INPUT, SZ_HSIZE)
  val hburst    = UInt(INPUT, SZ_HBURST)
  val hprot     = UInt(INPUT, SZ_HPROT)
  val htrans    = UInt(INPUT, SZ_HTRANS)
  val hmastlock = Bool(INPUT)

  val hwdata = Bits(INPUT, SZ_HDATA)
  val hrdata = Bits(OUTPUT, SZ_HDATA)

  val hsel      = Bool(INPUT)
  val hreadyin  = Bool(INPUT)
  val hreadyout = Bool(OUTPUT)
  val hresp     = UInt(OUTPUT, SZ_HRESP)
}