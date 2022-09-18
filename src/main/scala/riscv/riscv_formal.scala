package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

class riscv_formal extends Component {
  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

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

  val riscv = new riscv_top()
  riscv.busInst <> busInst
  riscv.busData <> busData
  riscv.IRQ <> IRQ
  riscv.rvfi <> rvfi

  val assumesWBMasterInst =
    new WishBoneMasterAssumes( config.busWishBoneConfig );
  assumesWBMasterInst.req := busInst.req
  assumesWBMasterInst.stall := busInst.stall
  assumesWBMasterInst.rsp := busInst.rsp

  val assumesWBMasterData =
    new WishBoneMasterAssumes( config.busWishBoneConfig );
  assumesWBMasterData.req := busData.req
  assumesWBMasterData.stall := busData.stall
  assumesWBMasterData.rsp := busData.rsp

  GenerationFlags.formal {
    when( initstate() ) {
      assume( clockDomain.isResetActive )
    }
  }
}
