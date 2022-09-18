package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

case class riscv_config(
    bufferSize: Int,
    branchPredSize: Int,
    oneShotInst: Boolean,
    outOfOrder: Boolean,
    busWishBoneConfig: WishBoneConfig,
    csrWishBoneConfig: WishBoneConfig
)

//Hardware definition
class riscv_top extends Component {
  val config = riscv_config(
    bufferSize        = 8,
    branchPredSize    = 16,
    oneShotInst       = false,
    outOfOrder        = false,
    busWishBoneConfig = WishBoneConfig(
      adrWidth  = 32,
      selWidth  = 4,
      dataWidth = 32,
      tgaWidth  = 1,
      tgdWidth  = 1,
      tgcWidth  = 4
    ),
    csrWishBoneConfig = WishBoneConfig(
      adrWidth  = 12,
      selWidth  = 32,
      dataWidth = 32,
      tgaWidth  = 1,
      tgdWidth  = 1,
      tgcWidth  = 1
    )
  )

  val busInst = master( WishBone( config.busWishBoneConfig ) )
  val busData = master( WishBone( config.busWishBoneConfig ) )

  val IRQ = in( Bits( 32 bits ) )

  val rvfi = out( Vec( RvfiMon(), 6 ) )

  val fsm = new riscv_fsm( config )
  val ifu = new riscv_ifu( config )
  val idu = new riscv_idu()
  val exu = new riscv_exu( config )
  val csr = new riscv_csr( config )

  val idle = Bool()
  idle := ~ifu.inst.Vld &
    ~idu.instDecoded.Vld &
    exu.idle

  val csrData = WishBone( config.csrWishBoneConfig )

  fsm.idle <> idle
  fsm.exuCntl <> exu.cntl
  fsm.IRQ <> IRQ
  fsm.mstatus <> csr.mstatusReg.mstatus
  fsm.mie <> csr.mieReg.mie
  fsm.mip <> csr.mipReg.mip
  fsm.mtvec <> csr.mtvecReg.mtvec
  fsm.mepc <> csr.mepcReg.mepc

  ifu.fsmCntl <> fsm.cntl
  ifu.misfetch <> exu.misfetch
  ifu.misfetchPC <> exu.misfetchPC
  ifu.misfetchAdr <> exu.misfetchAdr
  ifu.brTaken <> exu.brTaken
  ifu.brNotTaken <> exu.brNotTaken
  ifu.brCompressed <> exu.brCompressed
  ifu.brPC <> exu.brPC
  ifu.freeze <> exu.freeze
  ifu.busInst <> busInst
  ifu.idle <> idle

  idu.fsmCntl <> fsm.cntl
  idu.freeze <> exu.freeze
  idu.inst <> ifu.inst

  exu.fsmCntl <> fsm.cntl
  exu.instDecoded <> idu.instDecoded
  exu.busData <> busData
  exu.csrData <> csrData
  exu.rvfi <> rvfi
  exu.mepc <> csr.mepcReg.mepc

  csr.fsmCntl <> fsm.cntl
  csr.csrData <> csrData
  csr.retired <> exu.retired
  csr.brTaken <> exu.brTaken
  csr.brNotTaken <> exu.brNotTaken
  csr.misfetch <> exu.misfetch
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object riscv_config
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig( resetKind = SYNC ),
      targetDirectory              = "target"
    )

//Generate the riscv's Verilog using the above custom configuration.
object riscv_top {
  def main( args: Array[String] ) {
    riscv_config.includeFormal.generateVerilog( new riscv_top )
    riscv_config.includeFormal.generateSystemVerilog( new riscv_formal )
  }
}
