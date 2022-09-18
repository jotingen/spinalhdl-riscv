package riscv

import wishbone._

import spinal.core._
import spinal.lib._

//mvendorid
case class MVendorID()    extends Bundle {
  val Bank = Bits( 25 bits )
  val Offset = Bits( 7 bits )
}
case class MVendorIDReg() extends Bundle {
  val mvendorid = MVendorID() //Not registered, tied to 0
  mvendorid.Bank := 0
  mvendorid.Offset := 0

  def csrWrite( sel: Bits, data: Bits ): Unit = {}

  override def asBits: Bits = {
    return mvendorid.Bank ##
      mvendorid.Offset
  }
}

//marchid
case class MArchID()    extends Bundle {
  val ID = Bits( 32 bits )
}
case class MArchIDReg() extends Bundle {
  val marchid = MArchID() //Not registered, tied to 0
  marchid.ID := 0

  def csrWrite( sel: Bits, data: Bits ): Unit = {}

  override def asBits: Bits = {
    return marchid.ID
  }
}

//mimpid
case class MImpID()    extends Bundle {
  val Implementation = Bits( 32 bits )
}
case class MImpIDReg() extends Bundle {
  val mimpid = MImpID() //Not registered, tied to 0
  mimpid.Implementation := 0

  def csrWrite( sel: Bits, data: Bits ): Unit = {}

  override def asBits: Bits = {
    return mimpid.Implementation
  }
}

//mhartid
case class MHartID()    extends Bundle {
  val HartID = Bits( 32 bits )
}
case class MHartIDReg() extends Bundle {
  val mhartid = MHartID() //Not registered, tied to 0
  mhartid.HartID := 0

  def csrWrite( sel: Bits, data: Bits ): Unit = {}

  override def asBits: Bits = {
    return mhartid.HartID
  }
}

//mstatus
case class MStatus()    extends Bundle {
  val SD = Bool
  val TSR = Bool
  val TW = Bool
  val TVM = Bool
  val MXR = Bool
  val SUM = Bool
  val MPRV = Bool
  val XS = Bits( 2 bits )
  val FS = Bits( 2 bits )
  val MPP = Bits( 2 bits )
  val SPP = Bool
  val MPIE = Bool
  val SPIE = Bool
  val UPIE = Bool
  val MIE = Bool
  val SIE = Bool
  val UIE = Bool
}
case class MStatusReg() extends Bundle {
  val mstatus = Reg( MStatus() )
  mstatus.SD init ( False)
  mstatus.TSR init ( False)
  mstatus.TW init ( False)
  mstatus.TVM init ( False)
  mstatus.MXR init ( False)
  mstatus.SUM init ( False)
  mstatus.MPRV init ( False)
  mstatus.XS init ( 0)
  mstatus.FS init ( 0)
  mstatus.MPP init ( 0)
  mstatus.SPP init ( False)
  mstatus.MPIE init ( False)
  mstatus.SPIE init ( False)
  mstatus.UPIE init ( False)
  mstatus.MIE init ( False)
  mstatus.SIE init ( False)
  mstatus.UIE init ( False)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mstatus.SD := ( mstatus.SD & ~sel( 31 )) | ( data( 31 ) & sel( 31 ))
    mstatus.TSR := ( mstatus.TSR & ~sel( 23 )) | ( data( 23 ) & sel( 23 ))
    mstatus.TW := ( mstatus.TW & ~sel( 22 )) | ( data( 22 ) & sel( 22 ))
    mstatus.TVM := ( mstatus.TVM & ~sel( 21 )) | ( data( 21 ) & sel( 21 ))
    mstatus.MXR := ( mstatus.MXR & ~sel( 20 )) | ( data( 20 ) & sel( 20 ))
    mstatus.SUM := ( mstatus.SUM & ~sel( 19 )) | ( data( 19 ) & sel( 19 ))
    mstatus.MPRV := ( mstatus.MPRV & ~sel( 18 )) | ( data( 18 ) & sel( 18 ))
    mstatus.XS := ( mstatus.XS & ~sel( 17 downto 16 )) | ( data(
      17 downto 16
    ) & sel( 17 downto 16 ))
    mstatus.FS := ( mstatus.FS & ~sel( 15 downto 14 )) | ( data(
      15 downto 14
    ) & sel( 15 downto 14 ))
    mstatus.MPP := ( mstatus.MPP & ~sel( 13 downto 12 )) | ( data(
      13 downto 12
    ) & sel( 13 downto 12 ))
    mstatus.SPP := ( mstatus.SPP & ~sel( 9 )) | ( data( 9 ) & sel( 9 ))
    mstatus.MPIE := ( mstatus.MPIE & ~sel( 8 )) | ( data( 8 ) & sel( 8 ))
    mstatus.SPIE := ( mstatus.SPIE & ~sel( 5 )) | ( data( 5 ) & sel( 5 ))
    mstatus.UPIE := ( mstatus.UPIE & ~sel( 4 )) | ( data( 4 ) & sel( 4 ))
    mstatus.MIE := ( mstatus.MIE & ~sel( 3 )) | ( data( 3 ) & sel( 3 ))
    mstatus.SIE := ( mstatus.SIE & ~sel( 1 )) | ( data( 1 ) & sel( 1 ))
    mstatus.UIE := ( mstatus.UIE & ~sel( 0 )) | ( data( 0 ) & sel( 0 ))
  }

  override def asBits: Bits = {
    return mstatus.SD ##
      B"8'd0" ##
      mstatus.TSR ##
      mstatus.TW ##
      mstatus.TVM ##
      mstatus.MXR ##
      mstatus.SUM ##
      mstatus.MPRV ##
      mstatus.XS ##
      mstatus.FS ##
      mstatus.MPP ##
      B"2'd0" ##
      mstatus.SPP ##
      mstatus.MPIE ##
      B"1'd0" ##
      mstatus.SPIE ##
      mstatus.UPIE ##
      mstatus.MIE ##
      B"1'd0" ##
      mstatus.SIE ##
      mstatus.UIE
  }
}

//misa
case class MISA()    extends Bundle {
  case class MISAExtensions() extends Bundle {
    val Z = Bool //Reserved
    val Y = Bool //Reserved
    val X = Bool //Non-standard extensions present
    val W = Bool //Reserved
    val V = Bool //Tentatively reserved for Vector extension
    val U = Bool //User mode implemented
    val T = Bool //Tentatively reserved for Transactional Memory extension
    val S = Bool //Supervisor mode implemented
    val R = Bool //Reserved
    val Q = Bool //Quad-precision floating-point extension
    val P = Bool //Tentatively reserved for Packed-SIMD extension
    val O = Bool //Reserved
    val N = Bool //User-level interrupts supported
    val M = Bool //Integer Multiply/Divide extension
    val L = Bool //Tentatively reserved for Decimal Floating-Point extension
    val K = Bool //Reserved
    val J =
      Bool       //Tentatively reserved for Dynamically Translated Languages extension
    val I = Bool //RV32I/64I/128I base ISA
    val H = Bool //Hypervisor extension
    val G = Bool //Additional standard extensions present
    val F = Bool //Single-precision floating-point extension
    val E = Bool //RV32E base ISA
    val D = Bool //Double-precision floating-point extension
    val C = Bool //Compressed extension
    val B = Bool //Tentatively reserved for Bit-Manipulation extension
    val A = Bool //Atomic extension
  }
  val MXL = Bits( 2 bits )
  val Ext = MISAExtensions()
}
case class MISAReg() extends Bundle {
  val misa = Reg( MISA() )
  misa.MXL init ( B"2'h1")
  misa.Ext.Z init ( False) //Reserved
  misa.Ext.Y init ( False) //Reserved
  misa.Ext.X init ( False) //Non-standard extensions present
  misa.Ext.W init ( False) //Reserved
  misa.Ext.V init ( False) //Tentatively reserved for Vector extension
  misa.Ext.U init ( False) //User mode implemented
  misa.Ext.T init ( False) //Tentatively reserved for Transactional Memory extension
  misa.Ext.S init ( False) //Supervisor mode implemented
  misa.Ext.R init ( False) //Reserved
  misa.Ext.Q init ( False) //Quad-precision floating-point extension
  misa.Ext.P init ( False) //Tentatively reserved for Packed-SIMD extension
  misa.Ext.O init ( False) //Reserved
  misa.Ext.N init ( False) //User-level interrupts supported
  misa.Ext.M init ( True)  //Integer Multiply/Divide extension
  misa.Ext.L init ( False) //Tentatively reserved for Decimal Floating-Point extension
  misa.Ext.K init ( False) //Reserved
  misa.Ext.J init ( False) //Tentatively reserved for Dynamically Translated Languages extension
  misa.Ext.I init ( True)  //RV32I/64I/128I base ISA
  misa.Ext.H init ( False) //Hypervisor extension
  misa.Ext.G init ( False) //Additional standard extensions present
  misa.Ext.F init ( False) //Single-precision floating-point extension
  misa.Ext.E init ( False) //RV32E base ISA
  misa.Ext.D init ( False) //Double-precision floating-point extension
  misa.Ext.C init ( True)  //Compressed extension
  misa.Ext.B init ( False) //Tentatively reserved for Bit-Manipulation extension
  misa.Ext.A init ( False) //Atomic extension

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    misa.MXL := ( misa.MXL & ~sel( 31 downto 30 )) | ( data(
      31 downto 30
    ) & sel( 31 downto 30 ))
    misa.Ext.Z := ( misa.Ext.Z & ~sel( 25 )) | ( data( 25 ) & sel( 25 ))
    misa.Ext.Y := ( misa.Ext.Y & ~sel( 24 )) | ( data( 24 ) & sel( 24 ))
    misa.Ext.X := ( misa.Ext.X & ~sel( 23 )) | ( data( 23 ) & sel( 23 ))
    misa.Ext.W := ( misa.Ext.W & ~sel( 22 )) | ( data( 22 ) & sel( 22 ))
    misa.Ext.V := ( misa.Ext.V & ~sel( 21 )) | ( data( 21 ) & sel( 21 ))
    misa.Ext.U := ( misa.Ext.U & ~sel( 20 )) | ( data( 20 ) & sel( 20 ))
    misa.Ext.T := ( misa.Ext.T & ~sel( 19 )) | ( data( 19 ) & sel( 19 ))
    misa.Ext.S := ( misa.Ext.S & ~sel( 18 )) | ( data( 18 ) & sel( 18 ))
    misa.Ext.R := ( misa.Ext.R & ~sel( 17 )) | ( data( 17 ) & sel( 17 ))
    misa.Ext.Q := ( misa.Ext.Q & ~sel( 16 )) | ( data( 16 ) & sel( 16 ))
    misa.Ext.P := ( misa.Ext.P & ~sel( 15 )) | ( data( 15 ) & sel( 15 ))
    misa.Ext.O := ( misa.Ext.O & ~sel( 14 )) | ( data( 14 ) & sel( 14 ))
    misa.Ext.N := ( misa.Ext.N & ~sel( 13 )) | ( data( 13 ) & sel( 13 ))
    misa.Ext.M := ( misa.Ext.M & ~sel( 12 )) | ( data( 12 ) & sel( 12 ))
    misa.Ext.L := ( misa.Ext.L & ~sel( 11 )) | ( data( 11 ) & sel( 11 ))
    misa.Ext.K := ( misa.Ext.K & ~sel( 10 )) | ( data( 10 ) & sel( 10 ))
    misa.Ext.J := ( misa.Ext.J & ~sel( 9 )) | ( data( 9 ) & sel( 9 ))
    misa.Ext.I := ( misa.Ext.I & ~sel( 8 )) | ( data( 8 ) & sel( 8 ))
    misa.Ext.H := ( misa.Ext.H & ~sel( 7 )) | ( data( 7 ) & sel( 7 ))
    misa.Ext.G := ( misa.Ext.G & ~sel( 6 )) | ( data( 6 ) & sel( 6 ))
    misa.Ext.F := ( misa.Ext.F & ~sel( 5 )) | ( data( 5 ) & sel( 5 ))
    misa.Ext.E := ( misa.Ext.E & ~sel( 4 )) | ( data( 4 ) & sel( 4 ))
    misa.Ext.D := ( misa.Ext.D & ~sel( 3 )) | ( data( 3 ) & sel( 3 ))
    misa.Ext.C := ( misa.Ext.C & ~sel( 2 )) | ( data( 2 ) & sel( 2 ))
    misa.Ext.B := ( misa.Ext.B & ~sel( 1 )) | ( data( 1 ) & sel( 1 ))
    misa.Ext.A := ( misa.Ext.A & ~sel( 0 )) | ( data( 0 ) & sel( 0 ))
  }

  override def asBits: Bits = {
    return misa.MXL ##
      B"4'd0" ##
      misa.Ext.Z ##
      misa.Ext.Y ##
      misa.Ext.X ##
      misa.Ext.W ##
      misa.Ext.V ##
      misa.Ext.U ##
      misa.Ext.T ##
      misa.Ext.S ##
      misa.Ext.R ##
      misa.Ext.Q ##
      misa.Ext.P ##
      misa.Ext.O ##
      misa.Ext.N ##
      misa.Ext.M ##
      misa.Ext.L ##
      misa.Ext.K ##
      misa.Ext.J ##
      misa.Ext.I ##
      misa.Ext.H ##
      misa.Ext.G ##
      misa.Ext.F ##
      misa.Ext.E ##
      misa.Ext.D ##
      misa.Ext.C ##
      misa.Ext.B ##
      misa.Ext.A
  }
}

//medeleg
case class MEDeleg()    extends Bundle {
  val Exceptions = Bits( 32 bits )
}
case class MEDelegReg() extends Bundle {
  val medeleg = Reg( MEDeleg() ) //Not registered, tied to 0
  medeleg.Exceptions init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    medeleg.Exceptions := ( medeleg.Exceptions & ~sel( 31 downto 0 )) | ( data(
      31 downto 0
    ) & sel( 31 downto 0 ))
  }

  override def asBits: Bits = {
    return medeleg.Exceptions
  }
}

//mideleg
case class MIDeleg()    extends Bundle {
  val Interrupts = Bits( 32 bits )
}
case class MIDelegReg() extends Bundle {
  val mideleg = Reg( MIDeleg() ) //Not registered, tied to 0
  mideleg.Interrupts init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mideleg.Interrupts := ( mideleg.Interrupts & ~sel( 31 downto 0 )) | ( data(
      31 downto 0
    ) & sel( 31 downto 0 ))
  }

  override def asBits: Bits = {
    return mideleg.Interrupts
  }
}

//mie
case class MIE()    extends Bundle {
  val MEIE = Bool
  val SEIE = Bool
  val UEIE = Bool
  val MTIE = Bool
  val STIE = Bool
  val UTIE = Bool
  val MSIE = Bool
  val SSIE = Bool
  val USIE = Bool
}
case class MIEReg() extends Bundle {
  val mie = Reg( MIE() )
  mie.MEIE init ( False)
  mie.SEIE init ( False)
  mie.UEIE init ( False)
  mie.MTIE init ( False)
  mie.STIE init ( False)
  mie.UTIE init ( False)
  mie.MSIE init ( False)
  mie.SSIE init ( False)
  mie.USIE init ( False)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mie.MEIE := ( mie.MEIE & ~sel( 11 )) |
      ( data( 11 ) & sel( 11 ))
    mie.SEIE := ( mie.SEIE & ~sel( 9 )) |
      ( data( 9 ) & sel( 9 ))
    mie.UEIE := ( mie.UEIE & ~sel( 8 )) |
      ( data( 8 ) & sel( 8 ))
    mie.MTIE := ( mie.MTIE & ~sel( 7 )) |
      ( data( 7 ) & sel( 7 ))
    mie.STIE := ( mie.STIE & ~sel( 5 )) |
      ( data( 5 ) & sel( 5 ))
    mie.UTIE := ( mie.UTIE & ~sel( 4 )) |
      ( data( 4 ) & sel( 4 ))
    mie.MSIE := ( mie.MSIE & ~sel( 3 )) |
      ( data( 3 ) & sel( 3 ))
    mie.SSIE := ( mie.SSIE & ~sel( 1 )) |
      ( data( 1 ) & sel( 1 ))
    mie.USIE := ( mie.USIE & ~sel( 0 )) |
      ( data( 0 ) & sel( 0 ))
  }

  override def asBits: Bits = {
    return B"20'd0" ##
      mie.MEIE ##
      B"1'd0" ##
      mie.SEIE ##
      mie.UEIE ##
      mie.MTIE ##
      B"1'd0" ##
      mie.STIE ##
      mie.UTIE ##
      mie.MSIE ##
      B"1'd0" ##
      mie.SSIE ##
      mie.USIE
  }
}

//mtvec
case class MTVec()    extends Bundle {
  val Base = Bits( 30 bits )
  val Mode = Bits( 2 bits )
}
case class MTVecReg() extends Bundle {
  val mtvec = Reg( MTVec() )
  mtvec.Base init ( 0)
  mtvec.Mode init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mtvec.Base := ( mtvec.Base & ~sel( 31 downto 2 )) |
      ( data( 31 downto 2 ) & sel( 31 downto 2 ))
    mtvec.Mode := ( mtvec.Mode & ~sel( 1 downto 0 )) |
      ( data( 1 downto 0 ) & sel( 1 downto 0 ))
  }

  override def asBits: Bits = {
    return mtvec.Base ##
      mtvec.Mode
  }
}

//mcounteren
case class MCounterEn()    extends Bundle {
  val HPM31 = Bool
  val HPM30 = Bool
  val HPM29 = Bool
  val HPM28 = Bool
  val HPM27 = Bool
  val HPM26 = Bool
  val HPM25 = Bool
  val HPM24 = Bool
  val HPM23 = Bool
  val HPM22 = Bool
  val HPM21 = Bool
  val HPM20 = Bool
  val HPM19 = Bool
  val HPM18 = Bool
  val HPM17 = Bool
  val HPM16 = Bool
  val HPM15 = Bool
  val HPM14 = Bool
  val HPM13 = Bool
  val HPM12 = Bool
  val HPM11 = Bool
  val HPM10 = Bool
  val HPM9 = Bool
  val HPM8 = Bool
  val HPM7 = Bool
  val HPM6 = Bool
  val HPM5 = Bool
  val HPM4 = Bool
  val HPM3 = Bool
  val IR = Bool
  val TM = Bool
  val CY = Bool
}
case class MCounterEnReg() extends Bundle {
  val mcounteren = Reg( MCounterEn() )
  mcounteren.HPM31 init ( False)
  mcounteren.HPM30 init ( False)
  mcounteren.HPM29 init ( False)
  mcounteren.HPM28 init ( False)
  mcounteren.HPM27 init ( False)
  mcounteren.HPM26 init ( False)
  mcounteren.HPM25 init ( False)
  mcounteren.HPM24 init ( False)
  mcounteren.HPM23 init ( False)
  mcounteren.HPM22 init ( False)
  mcounteren.HPM21 init ( False)
  mcounteren.HPM20 init ( False)
  mcounteren.HPM19 init ( False)
  mcounteren.HPM18 init ( False)
  mcounteren.HPM17 init ( False)
  mcounteren.HPM16 init ( False)
  mcounteren.HPM15 init ( False)
  mcounteren.HPM14 init ( False)
  mcounteren.HPM13 init ( False)
  mcounteren.HPM12 init ( False)
  mcounteren.HPM11 init ( False)
  mcounteren.HPM10 init ( False)
  mcounteren.HPM9 init ( False)
  mcounteren.HPM8 init ( False)
  mcounteren.HPM7 init ( False)
  mcounteren.HPM6 init ( False)
  mcounteren.HPM5 init ( False)
  mcounteren.HPM4 init ( False)
  mcounteren.HPM3 init ( False)
  mcounteren.IR init ( False)
  mcounteren.TM init ( False)
  mcounteren.CY init ( False)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mcounteren.HPM31 := ( mcounteren.HPM31 & ~sel( 31 )) | ( data( 31 ) & sel(
      31
    ))
    mcounteren.HPM30 := ( mcounteren.HPM30 & ~sel( 30 )) | ( data( 30 ) & sel(
      30
    ))
    mcounteren.HPM29 := ( mcounteren.HPM29 & ~sel( 29 )) | ( data( 29 ) & sel(
      29
    ))
    mcounteren.HPM28 := ( mcounteren.HPM28 & ~sel( 28 )) | ( data( 28 ) & sel(
      28
    ))
    mcounteren.HPM27 := ( mcounteren.HPM27 & ~sel( 27 )) | ( data( 27 ) & sel(
      27
    ))
    mcounteren.HPM26 := ( mcounteren.HPM26 & ~sel( 26 )) | ( data( 26 ) & sel(
      26
    ))
    mcounteren.HPM25 := ( mcounteren.HPM25 & ~sel( 25 )) | ( data( 25 ) & sel(
      25
    ))
    mcounteren.HPM24 := ( mcounteren.HPM24 & ~sel( 24 )) | ( data( 24 ) & sel(
      24
    ))
    mcounteren.HPM23 := ( mcounteren.HPM23 & ~sel( 23 )) | ( data( 23 ) & sel(
      23
    ))
    mcounteren.HPM22 := ( mcounteren.HPM22 & ~sel( 22 )) | ( data( 22 ) & sel(
      22
    ))
    mcounteren.HPM21 := ( mcounteren.HPM21 & ~sel( 21 )) | ( data( 21 ) & sel(
      21
    ))
    mcounteren.HPM20 := ( mcounteren.HPM20 & ~sel( 20 )) | ( data( 20 ) & sel(
      20
    ))
    mcounteren.HPM19 := ( mcounteren.HPM19 & ~sel( 19 )) | ( data( 19 ) & sel(
      19
    ))
    mcounteren.HPM18 := ( mcounteren.HPM18 & ~sel( 18 )) | ( data( 18 ) & sel(
      18
    ))
    mcounteren.HPM17 := ( mcounteren.HPM17 & ~sel( 17 )) | ( data( 17 ) & sel(
      17
    ))
    mcounteren.HPM16 := ( mcounteren.HPM16 & ~sel( 16 )) | ( data( 16 ) & sel(
      16
    ))
    mcounteren.HPM15 := ( mcounteren.HPM15 & ~sel( 15 )) | ( data( 15 ) & sel(
      15
    ))
    mcounteren.HPM14 := ( mcounteren.HPM14 & ~sel( 14 )) | ( data( 14 ) & sel(
      14
    ))
    mcounteren.HPM13 := ( mcounteren.HPM13 & ~sel( 13 )) | ( data( 13 ) & sel(
      13
    ))
    mcounteren.HPM12 := ( mcounteren.HPM12 & ~sel( 12 )) | ( data( 12 ) & sel(
      12
    ))
    mcounteren.HPM11 := ( mcounteren.HPM11 & ~sel( 11 )) | ( data( 11 ) & sel(
      11
    ))
    mcounteren.HPM10 := ( mcounteren.HPM10 & ~sel( 10 )) | ( data( 10 ) & sel(
      10
    ))
    mcounteren.HPM9 := ( mcounteren.HPM9 & ~sel( 9 )) | ( data( 9 ) & sel( 9 ))
    mcounteren.HPM8 := ( mcounteren.HPM8 & ~sel( 8 )) | ( data( 8 ) & sel( 8 ))
    mcounteren.HPM7 := ( mcounteren.HPM7 & ~sel( 7 )) | ( data( 7 ) & sel( 7 ))
    mcounteren.HPM6 := ( mcounteren.HPM6 & ~sel( 6 )) | ( data( 6 ) & sel( 6 ))
    mcounteren.HPM5 := ( mcounteren.HPM5 & ~sel( 5 )) | ( data( 5 ) & sel( 5 ))
    mcounteren.HPM4 := ( mcounteren.HPM4 & ~sel( 4 )) | ( data( 4 ) & sel( 4 ))
    mcounteren.HPM3 := ( mcounteren.HPM3 & ~sel( 3 )) | ( data( 3 ) & sel( 3 ))
    mcounteren.IR := ( mcounteren.IR & ~sel( 2 )) | ( data( 2 ) & sel( 2 ))
    mcounteren.TM := ( mcounteren.TM & ~sel( 1 )) | ( data( 1 ) & sel( 1 ))
    mcounteren.CY := ( mcounteren.CY & ~sel( 0 )) | ( data( 0 ) & sel( 0 ))
  }

  override def asBits: Bits = {
    return mcounteren.HPM31 ##
      mcounteren.HPM30 ##
      mcounteren.HPM29 ##
      mcounteren.HPM28 ##
      mcounteren.HPM27 ##
      mcounteren.HPM26 ##
      mcounteren.HPM25 ##
      mcounteren.HPM24 ##
      mcounteren.HPM23 ##
      mcounteren.HPM22 ##
      mcounteren.HPM21 ##
      mcounteren.HPM20 ##
      mcounteren.HPM19 ##
      mcounteren.HPM18 ##
      mcounteren.HPM17 ##
      mcounteren.HPM16 ##
      mcounteren.HPM15 ##
      mcounteren.HPM14 ##
      mcounteren.HPM13 ##
      mcounteren.HPM12 ##
      mcounteren.HPM11 ##
      mcounteren.HPM10 ##
      mcounteren.HPM9 ##
      mcounteren.HPM8 ##
      mcounteren.HPM7 ##
      mcounteren.HPM6 ##
      mcounteren.HPM5 ##
      mcounteren.HPM4 ##
      mcounteren.HPM3 ##
      mcounteren.IR ##
      mcounteren.TM ##
      mcounteren.CY
  }
}

//mscratch
case class MScratch()    extends Bundle {
  val Scratch = Bits( 32 bits )
}
case class MScratchReg() extends Bundle {
  val mscratch = Reg( MScratch() )
  mscratch.Scratch init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mscratch.Scratch := ( mscratch.Scratch & ~sel( 31 downto 0 )) |
      ( data( 31 downto 0 ) & sel( 31 downto 0 ))
  }

  override def asBits: Bits = {
    return mscratch.Scratch
  }
}

//mepc
case class MEPC()    extends Bundle {
  val PC = Bits( 32 bits )
}
case class MEPCReg() extends Bundle {
  val mepc = Reg( MEPC() )
  mepc.PC init ( 0)

  def setPC( PC: UInt ): Unit = {
    mepc.PC := PC.asBits
  }

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mepc.PC := ( mepc.PC & ~sel( 31 downto 0 )) |
      ( data( 31 downto 0 ) & sel( 31 downto 0 ))
  }

  override def asBits: Bits = {
    return mepc.PC
  }
}

//mcause
case class MCause()    extends Bundle {
  val Interrupt = Bool
  val ExceptionCode = Bits( 31 bits )
}
case class MCauseReg() extends Bundle {
  val mcause = Reg( MCause() )
  mcause.Interrupt init ( False)
  mcause.ExceptionCode init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mcause.Interrupt := ( mcause.Interrupt & ~sel( 31 )) |
      ( data( 31 ) & sel( 31 ))
    mcause.ExceptionCode := ( mcause.ExceptionCode & ~sel( 30 downto 0 )) |
      ( data( 30 downto 0 ) & sel( 30 downto 0 ))
  }

  override def asBits: Bits = {
    return mcause.Interrupt ##
      mcause.ExceptionCode
  }
}

//mtval
case class MTVal()    extends Bundle {
  val TVal = Bits( 32 bits )
}
case class MTValReg() extends Bundle {
  val mtval = Reg( MTVal() )
  mtval.TVal init ( 0)

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    mtval.TVal := ( mtval.TVal & ~sel( 31 downto 0 )) |
      ( data( 31 downto 0 ) & sel( 31 downto 0 ))
  }

  override def asBits: Bits = {
    return mtval.TVal
  }
}

//mip
case class MIP()    extends Bundle {
  val MEIP = Bool
  val SEIP = Bool
  val UEIP = Bool
  val MTIP = Bool
  val STIP = Bool
  val UTIP = Bool
  val MSIP = Bool
  val SSIP = Bool
  val USIP = Bool
}
case class MIPReg() extends Bundle {
  val mip = Reg( MIP() )
  mip.MEIP init ( False)
  mip.SEIP init ( False)
  mip.UEIP init ( False)
  mip.MTIP init ( False)
  mip.STIP init ( False)
  mip.UTIP init ( False)
  mip.MSIP init ( False)
  mip.SSIP init ( False)
  mip.USIP init ( False)

  val MEIP = in( Bool )
  mip.MEIP := MEIP

  def csrWrite( sel: Bits, data: Bits ): Unit = {
    //mip.MEIP  := ( mip.MEIP  & ~sel( 11 )) | ( data( 11 ) & sel( 11 ))
    mip.SEIP := ( mip.SEIP & ~sel( 9 )) | ( data( 9 ) & sel( 9 ))
    mip.UEIP := ( mip.UEIP & ~sel( 8 )) | ( data( 8 ) & sel( 8 ))
    //ip.MTIP  := ( mip.MTIP  & ~sel( 7 )) | ( data( 7 ) & sel( 7 ))
    mip.STIP := ( mip.STIP & ~sel( 5 )) | ( data( 5 ) & sel( 5 ))
    mip.UTIP := ( mip.UTIP & ~sel( 4 )) | ( data( 4 ) & sel( 4 ))
    mip.MSIP := ( mip.MSIP & ~sel( 3 )) | ( data( 3 ) & sel( 3 ))
    mip.SSIP := ( mip.SSIP & ~sel( 1 )) | ( data( 1 ) & sel( 1 ))
    mip.USIP := ( mip.USIP & ~sel( 0 )) | ( data( 0 ) & sel( 0 ))
  }

  override def asBits: Bits = {
    return B"20'd0" ##
      mip.MEIP ##
      B"1'b0" ##
      mip.SEIP ##
      mip.UEIP ##
      mip.MTIP ##
      B"1'b0" ##
      mip.STIP ##
      mip.UTIP ##
      mip.MSIP ##
      B"1'b0" ##
      mip.SSIP ##
      mip.USIP
  }
}

class riscv_csr( config: riscv_config ) extends Component {

  val csrData = slave( WishBone( config.csrWishBoneConfig ) )

  val fsmCntl = in( FSMCntl() )

  val retired = in( Bool )
  val brTaken = in( Bool )
  val brNotTaken = in( Bool )
  val misfetch = in( Bool )

  csrData.stall := False

  val csrDataRsp = Reg( WishBoneRsp( config.csrWishBoneConfig ) )
  csrData.rsp <> csrDataRsp

  csrDataRsp.ack := csrData.req.cyc
  csrDataRsp.err := False
  csrDataRsp.rty := False
  csrDataRsp.data := 0
  csrDataRsp.tga := 0
  csrDataRsp.tgd := 0
  csrDataRsp.tgc := 0

  //User Counter/Timers

  //0xC00/0xC80 Counter
  val counter = Reg( UInt( 64 bits ) )
  counter init ( 0)
  counter := counter + 1

  //0xC01/0xC81 Timer
  // In ns, at 50MHz is 20 ticks per cycle
  val timer = Reg( UInt( 64 bits ) )
  timer init ( 0)
  timer := timer + 20

  //0xC02/0xC82 Retired Counter
  val retired_counter = Reg( UInt( 64 bits ) )
  retired_counter init ( 0)
  when( retired ) {
    retired_counter := retired_counter + 1
  } otherwise {
    retired_counter := retired_counter
  }

  //0xC03/0xC83 Branch Counter
  val branch_counter = Reg( UInt( 64 bits ) )
  branch_counter init ( 0)
  when( brTaken || brNotTaken ) {
    branch_counter := branch_counter + 1
  } otherwise {
    branch_counter := branch_counter
  }

  //0xC04/0xC84 Branch Miss Counter
  val brmiss_counter = Reg( UInt( 64 bits ) )
  brmiss_counter init ( 0)
  when( misfetch ) {
    brmiss_counter := brmiss_counter + 1
  } otherwise {
    brmiss_counter := brmiss_counter
  }

  //Machine Information Registers

  //0xF11 MRO mvendorid Vendor ID.
  val mvendoridReg = out( MVendorIDReg() )

  //0xF12 MRO marchid Architecture ID.
  val marchidReg = out( MArchIDReg() )

  //0xF13 MRO mimpid Implementation ID.
  val mimpidReg = out( MImpIDReg() )

  //0xF14 MRO mhartid Hardware thread ID.
  val mhartidReg = out( MHartIDReg() )

  //Machine Trap Setup

  //0x300 MRW mstatus Machine status register.
  val mstatusReg = out( MStatusReg() )

  //0x301 MRW misa ISA and extensions
  val misaReg = out( MISAReg() )

  //0x302 MRW medeleg Machine exception delegation register.
  val medelegReg = out( MEDelegReg() )

  //0x303 MRW mideleg Machine interrupt delegation register.
  val midelegReg = out( MIDelegReg() )

  //0x304 MRW mie Machine interrupt-enable register.
  val mieReg = out( MIEReg() )

  //0x305 MRW mtvec Machine trap-handler base address.
  val mtvecReg = out( MTVecReg() )

  //0x306 MRW mcounteren Machine counter enable.
  val mcounterenReg = out( MCounterEnReg() )

  //Machine Trap Handling

  //0x340 MRW mscratch Scratch register for machine trap handlers.
  val mscratchReg = out( MScratchReg() )

  //0x341 MRW mepc Machine exception program counter.
  val mepcReg = out( MEPCReg() )
  when( fsmCntl.MEPCVal ) {
    mepcReg.setPC( fsmCntl.MEPC )
  }

  //0x342 MRW mcause Machine trap cause.
  val mcauseReg = out( MCauseReg() )

  //0x343 MRW mtval Machine bad address or instruction.
  val mtvalReg = out( MTValReg() )

  //0x344 MRW mip Machine interrupt pending.
  val mipReg = out( MIPReg() )
  mipReg.MEIP <> fsmCntl.MEIP

  //Bus
  when( csrData.req.cyc ) {
    switch( csrData.req.adr ) {
      //Counter Lo
      is( U"hC00" ) {
        csrDataRsp.data := B( counter( 31 downto 0 ) )
        counter( 31 downto 0 ) := U(
          ( B( counter( 31 downto 0 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }
      //Counter Hi
      is( U"hC80" ) {
        csrDataRsp.data := B( counter( 63 downto 32 ) )
        counter( 63 downto 32 ) := U(
          ( B( counter( 63 downto 32 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }

      //Timer Lo
      is( U"hC01" ) {
        csrDataRsp.data := B( timer( 31 downto 0 ) )
        timer( 31 downto 0 ) := U(
          ( B( timer( 31 downto 0 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }
      //Timer Hi
      is( U"hC81" ) {
        csrDataRsp.data := B( timer( 63 downto 32 ) )
        timer( 63 downto 32 ) := U(
          ( B( timer( 63 downto 32 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }

      //Retired Counter Lo
      is( U"hC02" ) {
        csrDataRsp.data := B( retired_counter( 31 downto 0 ) )
        retired_counter( 31 downto 0 ) := U(
          ( B( retired_counter( 31 downto 0 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }
      //Retired Counter Hi
      is( U"hC82" ) {
        csrDataRsp.data := B( retired_counter( 63 downto 32 ) )
        retired_counter( 63 downto 32 ) := U(
          ( B( retired_counter( 63 downto 32 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }

      //Branch Counter Lo
      is( U"hC03" ) {
        csrDataRsp.data := B( branch_counter( 31 downto 0 ) )
        branch_counter( 31 downto 0 ) := U(
          ( B( branch_counter( 31 downto 0 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }
      //Branch Counter Hi
      is( U"hC83" ) {
        csrDataRsp.data := B( branch_counter( 63 downto 32 ) )
        branch_counter( 63 downto 32 ) := U(
          ( B( branch_counter( 63 downto 32 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }

      //BrMiss Counter Lo
      is( U"hC04" ) {
        csrDataRsp.data := B( brmiss_counter( 31 downto 0 ) )
        brmiss_counter( 31 downto 0 ) := U(
          ( B( brmiss_counter( 31 downto 0 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }
      //BrMiss Counter Hi
      is( U"hC84" ) {
        csrDataRsp.data := B( brmiss_counter( 63 downto 32 ) )
        brmiss_counter( 63 downto 32 ) := U(
          ( B( brmiss_counter( 63 downto 32 ) ) & ~csrData.req.sel) |
            ( csrData.req.data & csrData.req.sel)
        )
      }

      //0xF11 MRO mvendorid Vendor ID.
      is( U"hF11" ) {
        csrDataRsp.data := B( mvendoridReg )
        when( csrData.req.we ) {
          mvendoridReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0xF12 MRO marchid Architecture ID.
      is( U"hF12" ) {
        csrDataRsp.data := B( marchidReg )
        when( csrData.req.we ) {
          marchidReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0xF13 MRO mimpid Implementation ID.
      is( U"hF13" ) {
        csrDataRsp.data := B( mimpidReg )
        when( csrData.req.we ) {
          mimpidReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0xF14 MRO mhartid Hardware thread ID.
      is( U"hF14" ) {
        csrDataRsp.data := B( mhartidReg )
        when( csrData.req.we ) {
          mhartidReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x300 MRW mstatus Machine status register.
      is( U"h300" ) {
        csrDataRsp.data := B( mstatusReg )
        when( csrData.req.we ) {
          mstatusReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x301 MRW misa ISA and extensions
      is( U"h301" ) {
        csrDataRsp.data := B( misaReg )
        when( csrData.req.we ) {
          misaReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x302 MRW medeleg Machine exception delegation register.
      is( U"h302" ) {
        csrDataRsp.data := B( medelegReg )
        when( csrData.req.we ) {
          medelegReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x303 MRW mideleg Machine interrupt delegation register.
      is( U"h303" ) {
        csrDataRsp.data := B( midelegReg )
        when( csrData.req.we ) {
          midelegReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x304 MRW mie Machine interrupt-enable register.
      is( U"h304" ) {
        csrDataRsp.data := B( mieReg )
        when( csrData.req.we ) {
          mieReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x305 MRW mtvec Machine trap-handler base address.
      is( U"h305" ) {
        csrDataRsp.data := B( mtvecReg )
        when( csrData.req.we ) {
          mtvecReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x306 MRW mcounteren Machine counter enable.
      is( U"h306" ) {
        csrDataRsp.data := B( mcounterenReg )
        when( csrData.req.we ) {
          mcounterenReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //Machine Trap Handling

      //0x340 MRW mscratch Scratch register for machine trap handlers.
      is( U"h340" ) {
        csrDataRsp.data := B( mscratchReg )
        when( csrData.req.we ) {
          mscratchReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x341 MRW mepc Machine exception program counter.
      is( U"h341" ) {
        csrDataRsp.data := B( mepcReg )
        when( csrData.req.we ) {
          mepcReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x342 MRW mcause Machine trap cause.
      is( U"h342" ) {
        csrDataRsp.data := B( mcauseReg )
        when( csrData.req.we ) {
          mcauseReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x343 MRW mtval Machine bad address or instruction.
      is( U"h343" ) {
        csrDataRsp.data := B( mtvalReg )
        when( csrData.req.we ) {
          mtvalReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      //0x344 MRW mip Machine interrupt pending.
      is( U"h344" ) {
        csrDataRsp.data := B( mipReg )
        when( csrData.req.we ) {
          mipReg.csrWrite( csrData.req.sel, csrData.req.data )
        }
      }

      default {}
    }
  }
}
