package wishbone

import spinal.core._
import spinal.lib._

case class WishBoneConfig(
    val adrWidth: Int,
    val selWidth: Int,
    val dataWidth: Int,
    val tgaWidth: Int,
    val tgdWidth: Int,
    val tgcWidth: Int
) {}

case class WishBoneReq( config: WishBoneConfig ) extends Bundle {
  val cyc = Bool
  val stb = Bool
  val we = Bool
  val adr = UInt( config.adrWidth bits )
  val sel = Bits( config.selWidth bits )
  val data = Bits( config.dataWidth bits )
  val tga = Bits( config.tgaWidth bits )
  val tgd = Bits( config.tgdWidth bits )
  val tgc = Bits( config.tgcWidth bits )

  def ReverseEndian(): WishBoneReq = {
    val req = WishBoneReq( config )
    req.cyc := this.cyc
    req.stb := this.stb
    req.we := this.we
    req.adr := this.adr
    req.sel := Reverse( this.sel )
    req.data := EndiannessSwap( this.data )
    req.tga := this.tga
    req.tgd := this.tgd
    req.tgc := this.tgc
    return req
  }

  def Requesting(): Bool = {
    return cyc & stb
  }

}

case class WishBoneRsp( config: WishBoneConfig ) extends Bundle {
  val ack = Bool
  val err = Bool
  val rty = Bool
  val data = Bits( config.dataWidth bits )
  val tga = Bits( config.tgaWidth bits )
  val tgd = Bits( config.tgdWidth bits )
  val tgc = Bits( config.tgcWidth bits )

  def ReverseEndian(): WishBoneRsp = {
    val rsp = WishBoneRsp( config )
    rsp.ack := this.ack
    rsp.err := this.err
    rsp.rty := this.rty
    rsp.data := EndiannessSwap( this.data )
    rsp.tga := this.tga
    rsp.tgd := this.tgd
    rsp.tgc := this.tgc
    return rsp
  }

  def Recieving(): Bool = {
    return ack | err | rty
  }
}

case class WishBone( config: WishBoneConfig ) extends Bundle with IMasterSlave {
  val req = WishBoneReq( config )
  val stall = Bool
  val rsp = WishBoneRsp( config )

  override def asMaster(): Unit = {
    out( req )
    in( stall )
    in( rsp )
  }

  val asserts = new WishBoneAsserts( config );
  asserts.req := req
  asserts.stall := stall
  asserts.rsp := rsp

}

class WishBoneMasterAssumes( config: WishBoneConfig ) extends Component {
  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  val req = in( WishBoneReq( config ) )
  val stall = in( Bool )
  val rsp = in( WishBoneRsp( config ) )

  //Signal toggles after initial reset, nothing matters before the first reset
  var reset_seen = Reg( Bool )
  reset_seen := reset_seen
  when( clockDomain.isResetActive ) {
    reset_seen := True
  }

  GenerationFlags.formal {
    //Set reset_seen to 0
    when( initstate() ) {
      assume( reset_seen === False )
    }
    //Ack and Sta;; need to be low until we get a reset and during reset
    when( !reset_seen | clockDomain.isResetActive ) {
      assume( rsp.Recieving() === False )
      assume( stall === False )
    }
  }
}

class WishBoneSlaveAssumes( config: WishBoneConfig ) extends Component {
  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  val req = in( WishBoneReq( config ) )
  val stall = in( Bool )
  val rsp = in( WishBoneRsp( config ) )

  //Signal toggles after initial reset, nothing matters before the first reset
  var reset_seen = Reg( Bool )
  reset_seen := reset_seen
  when( clockDomain.isResetActive ) {
    reset_seen := True
  }

  //Requests in flight
  val reqsInFlight = Reg( UInt( 16 bits ) )
  reqsInFlight init ( 0)
  reqsInFlight := reqsInFlight
  var reqSent = req.cyc && req.stb
  var reqRecieved = rsp.ack | rsp.err | rsp.rty
  //If both requesting and recieving, don't update
  when( !( ( req.Requesting() & !stall) & rsp.Recieving()) ) {
    when( req.Requesting() & !stall ) {
      reqsInFlight := reqsInFlight + 1
    }
    when( rsp.Recieving() ) {
      reqsInFlight := reqsInFlight - 1
    }
  }

  GenerationFlags.formal {
    //Set reset_seen to 0
    when( initstate() ) {
      assume( reset_seen === False )
    }
    //Cyc and Stb need to be low until we get a reset and during reset
    when( !reset_seen | clockDomain.isResetActive ) {
      assume( req.cyc === False )
      assume( req.stb === False )
    }
    //Cyc must be high if requests are in flight
    when( reqsInFlight =/= 0 ) {
      assume( req.cyc === True )
    }
  }
}

class WishBoneAsserts( config: WishBoneConfig ) extends Component {
  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  val req = in( WishBoneReq( config ) )
  val stall = in( Bool )
  val rsp = in( WishBoneRsp( config ) )

  //Signal toggles after initial reset, nothing matters before the first reset
  var reset_seen = Reg( Bool )
  reset_seen := reset_seen
  when( clockDomain.isResetActive ) {
    reset_seen := True
  }

  //Buuild queue to track requests in flight
  //Usign to make sure returned TGA/TGC/TGD values match and notihng is dropped/duplicated
  //Cause an error if overflowed, assume timeout
  var QUEUE_DEPTH = 16
  val reqQueue = Vec( Reg( WishBoneReq( config ) ), QUEUE_DEPTH )
  val reqQueueWrNdx = Reg( UInt( log2Up( QUEUE_DEPTH ) bits ) )
  val reqQueueRdNdx = Reg( UInt( log2Up( QUEUE_DEPTH ) bits ) )
  val reqOutgoing = WishBoneReq( config )
  reqQueueWrNdx init ( 0)
  reqQueueRdNdx init ( 0)

  def reqQueuePush( req: WishBoneReq ): Unit = {
    reqQueue( reqQueueWrNdx ) := req
    reqQueueWrNdx := reqQueueWrNdx + 1
  }

  def reqQueuePull(): Unit = {
    reqQueueRdNdx := reqQueueRdNdx + 1
  }
  reqOutgoing := reqQueue( reqQueueRdNdx )

  var transferIncoming = !clockDomain.isResetActive & req.Requesting() & !stall
  when( transferIncoming ) {
    reqQueuePush( req )
  }

  var transferOutgoing = !clockDomain.isResetActive & rsp.Recieving()
  when( transferOutgoing ) {
    reqQueuePull()
  }

  GenerationFlags.formal {
    when( initstate() ) {
      assume( reset_seen === False )
    }
    //During reset
    when( reset_seen ) {
      when( clockDomain.isResetActive ) {
        assert(
          assertion = req.cyc === False,
          message   = "Cyc must be low during reset",
          severity  = FAILURE
        )
        assert(
          assertion = req.stb === False,
          message   = "Stb must be low during reset",
          severity  = FAILURE
        )
        assert(
          assertion = stall === False,
          message   = "Stall must be low during reset",
          severity  = FAILURE
        )
        //Outside of reset
      } otherwise {
        when( reqQueueWrNdx =/= reqQueueRdNdx ) { //queue not empty
          assert(
            assertion = req.cyc,
            message   = "Cyc must be set when there are requests in flight",
            severity  = FAILURE
          )
        }

        when( transferOutgoing ) {
          for (selNdx <- 0 to widthOf( reqOutgoing.sel ) - 1) {
            var bitsPerSel =
              widthOf( reqOutgoing.data ) / widthOf( reqOutgoing.sel )
            var msbBit = bitsPerSel * selNdx + bitsPerSel - 1
            var lsbBit = bitsPerSel * selNdx
            when( !reqOutgoing.sel( selNdx ) ) {
              assert(
                assertion = rsp.data( msbBit downto lsbBit ) === 0,
                message   =
                  s"Response data (${msbBit} downto ${lsbBit}) must be 0 when sel bit (${selNdx}) was 0",
                severity  = FAILURE
              )
            }
          }
        }

        when( transferOutgoing ) {
          assert(
            assertion = reqOutgoing.tga === rsp.tga,
            message   = "Response tga must match initial request",
            severity  = FAILURE
          )
        }

        when( transferOutgoing ) {
          assert(
            assertion = reqOutgoing.tgc === rsp.tgc,
            message   = "Response tgc must match initial request",
            severity  = FAILURE
          )
        }

        when( transferOutgoing ) {
          assert(
            assertion = reqOutgoing.tgd === rsp.tgd,
            message   = "Response tgd must match initial request",
            severity  = FAILURE
          )
        }
      }
    }
  }

}
