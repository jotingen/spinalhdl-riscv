package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

class riscv_csu( config: riscv_config ) extends Component {
  val capture = in( Bool )
  val instDecoded = in( InstDecoded() )
  val x = in( Vec( Bits( 32 bits ), 32 ) )
  val busy = out( Bool )
  val rs1 = out( UInt( 5 bits ) )
  val rs2 = out( UInt( 5 bits ) )
  val rd = out( UInt( 5 bits ) )
  val done = out( Bool )
  val wr = out( Bool )
  val ndx = out( UInt( 5 bits ) )
  val data = out( Bits( 32 bits ) )
  val misfetch = out( Bool )
  val PC = out( UInt( 32 bits ) )
  val PCNext = out( UInt( 32 bits ) )
  val csrData = master( WishBone( config.csrWishBoneConfig ) )

  val order = in( UInt( 64 bits ) )
  val rvfi = out( RvfiMon() )

  val csrDataReq = Reg( WishBoneReq( config.csrWishBoneConfig ) )
  csrData.req <> csrDataReq
  csrDataReq.cyc init ( False)
  csrDataReq.stb init ( False)
  csrDataReq.we init ( False)
  csrDataReq.adr init ( 0)
  csrDataReq.sel init ( 0)
  csrDataReq.data init ( 0)
  csrDataReq.tga init ( 0)
  csrDataReq.tgd init ( 0)
  csrDataReq.tgc init ( 0)

  val csrDataRsp = Reg( WishBoneRsp( config.csrWishBoneConfig ) )

  val inst = Reg( InstDecoded() )
  inst.Vld init ( False)

  val rvfi_order = Reg( UInt( 64 bits ) )
  rvfi_order init ( 0)

  busy <> inst.Vld
  rs1 <> U( inst.Rs1 )
  rs2 <> U( inst.Rs2 )
  rd <> U( inst.Rd )

  PC := inst.Adr

  //Calculate new data
  val rs1Data = B( x( U( inst.Rs1 ) ) )
  val rs2Data = B( x( U( inst.Rs2 ) ) )

  object State extends SpinalEnum( defaultEncoding = binaryOneHot ) {
    //format: off
      val IDLE, PENDING, RCVD = newElement()
  }
  val state = Reg( State() )
  state init ( State.IDLE )

  //Default values
  state := state
  csrDataReq.cyc := False
  csrDataReq.stb := False
  csrDataReq.we := csrDataReq.we
  csrDataReq.adr := csrDataReq.adr
  csrDataReq.sel := csrDataReq.sel
  csrDataReq.data := csrDataReq.data
  csrDataReq.tga := csrDataReq.tga
  csrDataReq.tgd := csrDataReq.tgd
  csrDataReq.tgc := csrDataReq.tgc
  done := False
  ndx := U( inst.Rd )
  data := 0
  wr := False
  PCNext := inst.Adr + 4
  misfetch := inst.AdrNext =/= PCNext

  switch( state ) {
    //In idle state we recieve a valid and sned out a bus req
    is( State.IDLE ) {
      when( inst.Vld ) {
        state := State.PENDING

        //Always turn on bus req
        csrDataReq.cyc := True
        csrDataReq.stb := True

        csrDataReq.we  := False
        csrDataReq.adr := U(inst.CSR)

        switch( inst.Op ) {
          is( InstOp.CSRRW ) {
            csrDataReq.we := True
            csrDataReq.data := rs1Data
            csrDataReq.sel := B"hFFFFFFFF"
          }
          is( InstOp.CSRRS ) {
            when( rs1Data =/= 0 ) {
              csrDataReq.we := True
            }
            csrDataReq.data := B"hFFFFFFFF"
            csrDataReq.sel := rs1Data
          }
          is( InstOp.CSRRC ) {
            when( rs1Data =/= 0 ) {
              csrDataReq.we := True
            }
            csrDataReq.data := B"h00000000"
            csrDataReq.sel := rs1Data
          }
          is( InstOp.CSRRWI ) {
            csrDataReq.we := True
            csrDataReq.data := B"hFFFFFFFF"
            csrDataReq.sel := B( U(inst.Rs1, 32 bits) )
          }                                            
          is( InstOp.CSRRSI ) {                        
            when( rs1Data =/= 0 ) {
              csrDataReq.we := True
            }
            csrDataReq.data := B"hFFFFFFFF"            
            csrDataReq.sel := B( U(inst.Rs1, 32 bits) )
          }                                            
          is( InstOp.CSRRCI ) {                        
            when( rs1Data =/= 0 ) {
              csrDataReq.we := True
            }
            csrDataReq.data := B"h00000000"            
            csrDataReq.sel := B( U(inst.Rs1, 32 bits) )
          }
          default {
            //TODO generate error
            csrDataReq.cyc := False
            csrDataReq.stb := False
          }
        }
      }
    }

    //In pending state we are waiting to capture bus rsp
    is( State.PENDING ) {
      when( csrData.rsp.ack) {
        state := State.RCVD

        //Capture bus data
        csrDataRsp := csrData.rsp
      }
    }

    //In recieved state we update registers
    is( State.RCVD ) {
      state := State.IDLE
      done := True

      wr := True
      data := csrDataRsp.data

      inst.Vld := False

    }
  }
  when( inst.Rd === 0 ) {
    data := 0
  }

  //done := False
  //wr := False
  //ndx := U( inst.Rd )
  //PCNext := inst.Adr + 4
  //misfetch := inst.AdrNext =/= PCNext
  //when( inst.Vld ) {
  //  done := True
  //  wr := True
  //  inst.Vld := False
  //}

  when( capture ) {
    inst := instDecoded
    rvfi_order := order
  }

  rvfi.valid := inst.Vld && done
  rvfi.order := B( rvfi_order )
  rvfi.insn := inst.Data
  rvfi.trap := False
  rvfi.halt := False
  rvfi.intr := inst.Interrupt
  rvfi.mode := 0
  rvfi.rs1_addr := inst.Rs1
  rvfi.rs2_addr := inst.Rs2
  rvfi.rs1_rdata := rs1Data
  rvfi.rs2_rdata := rs2Data
  when( wr ) {
    rvfi.rd_addr := inst.Rd
    rvfi.rd_wdata := data
  } otherwise {
    rvfi.rd_addr := 0
    rvfi.rd_wdata := 0
  }
  rvfi.pc_rdata := B( inst.Adr )
  rvfi.pc_wdata := B( PCNext )
  rvfi.mem_addr := 0
  rvfi.mem_rmask := 0
  rvfi.mem_wmask := 0
  rvfi.mem_rdata := 0
  rvfi.mem_wdata := 0
  rvfi.csr_mcycle_rmask := 0
  rvfi.csr_mcycle_wmask := 0
  rvfi.csr_mcycle_rdata := 0
  rvfi.csr_mcycle_wdata := 0
  rvfi.csr_minstret_rmask := 0
  rvfi.csr_minstret_wmask := 0
  rvfi.csr_minstret_rdata := 0
  rvfi.csr_minstret_wdata := 0
}
