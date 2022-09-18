package rvfimon

import spinal.core._
import spinal.lib._

case class RvfiMon() extends Bundle {
  val valid              = Bool         
  val order              = Bits(64 bits)
  val insn               = Bits(32 bits)
  val trap               = Bool         
  val halt               = Bool         
  val intr               = Bool         
  val mode               = Bits( 2 bits)
  val rs1_addr           = Bits( 5 bits)
  val rs2_addr           = Bits( 5 bits)
  val rs1_rdata          = Bits(32 bits)
  val rs2_rdata          = Bits(32 bits)
  val rd_addr            = Bits( 5 bits)
  val rd_wdata           = Bits(32 bits)
  val pc_rdata           = Bits(32 bits)
  val pc_wdata           = Bits(32 bits)
  val mem_addr           = Bits(32 bits)
  val mem_rmask          = Bits( 4 bits)
  val mem_wmask          = Bits( 4 bits)
  val mem_rdata          = Bits(32 bits)
  val mem_wdata          = Bits(32 bits)
  val csr_mcycle_rmask   = Bits(64 bits)
  val csr_mcycle_wmask   = Bits(64 bits)
  val csr_mcycle_rdata   = Bits(64 bits)
  val csr_mcycle_wdata   = Bits(64 bits)
  val csr_minstret_rmask = Bits(64 bits)
  val csr_minstret_wmask = Bits(64 bits)
  val csr_minstret_rdata = Bits(64 bits)
  val csr_minstret_wdata = Bits(64 bits)

}

