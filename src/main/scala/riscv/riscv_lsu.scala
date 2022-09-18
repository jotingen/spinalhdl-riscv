package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

class riscv_lsu( config: riscv_config ) extends Component {
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
  val busData = master( WishBone( config.busWishBoneConfig ) )

  val order = in( UInt( 64 bits ) )
  val rvfi = out( RvfiMon() )

  val inst = Reg( InstDecoded() )
  inst.Vld init ( False)

  val rvfi_order = Reg( UInt( 64 bits ) )
  rvfi_order init ( 0)

  val busDataReq = Reg( WishBoneReq( config.busWishBoneConfig ) )
  busData.req <> busDataReq
  busDataReq.cyc init ( False)
  busDataReq.stb init ( False)
  busDataReq.we init ( False)
  busDataReq.adr init ( 0)
  busDataReq.sel init ( 0)
  busDataReq.data init ( 0)
  busDataReq.tga init ( 0)
  busDataReq.tgd init ( 0)
  busDataReq.tgc init ( 0)

  val busDataRsp = Reg( WishBoneRsp( config.busWishBoneConfig ) )

  val pendingRsp = Reg( Bool )
  pendingRsp init ( False)

  busy <> inst.Vld
  rs1 <> U( inst.Rs1 )
  rs2 <> U( inst.Rs2 )
  rd <> U( inst.Rd )

  PC := inst.Adr

  //Calculate new data
  val rs1Data = B( x( U( inst.Rs1 ) ) )
  val rs2Data = B( x( U( inst.Rs2 ) ) )
  val adr = U( S( rs1Data ) + S( inst.Immed ) )

  object State extends SpinalEnum( defaultEncoding = binaryOneHot ) {
    //format: off
      val IDLE, PENDING, RCVD = newElement()
  }
  val state = Reg( State() )
  state init ( State.IDLE )

  //Default values
  state := state
  busDataReq.cyc := False
  busDataReq.stb := False
  busDataReq.we := busDataReq.we
  busDataReq.adr := busDataReq.adr
  busDataReq.sel := busDataReq.sel
  busDataReq.data := busDataReq.data
  busDataReq.tga := busDataReq.tga
  busDataReq.tgd := busDataReq.tgd
  busDataReq.tgc := busDataReq.tgc
  done := False
  ndx := U( inst.Rd )
  data := 0
  wr := False
  when(inst.Op === InstOp.CLWSP ||
       inst.Op === InstOp.CSWSP ||
       inst.Op === InstOp.CLW ||
       inst.Op === InstOp.CSW) {
  PCNext := inst.Adr + 2
       } otherwise {
  PCNext := inst.Adr + 4
       }
  misfetch := inst.AdrNext =/= PCNext

  switch( state ) {
    //In idle state we recieve a valid and sned out a bus req
    is( State.IDLE ) {
      when( inst.Vld ) {
        state := State.PENDING

        //Always turn on bus req
        busDataReq.cyc := True
        busDataReq.stb := True

        //Always generate aligned address
        busDataReq.adr := adr
        busDataReq.adr( 1 downto 0 ) := 0

        //Only Stores can write
        busDataReq.we := False
        busDataReq.we.setWhen(inst.Op === InstOp.SB ||
          inst.Op === InstOp.SH ||
          inst.Op === InstOp.SW ||
          inst.Op === InstOp.CSWSP ||
          inst.Op === InstOp.CSW)

        //Build up sel from address
        when(inst.Op === InstOp.SB ||
          inst.Op === InstOp.LB ||
          inst.Op === InstOp.LBU) {
            switch( B( adr( 1 downto 0 ) ) ) {
              is( B"2'd0" ) {
                busDataReq.sel := B"4'b0001"
              }
              is( B"2'd1" ) {
                busDataReq.sel := B"4'b0010"
              }
              is( B"2'd2" ) {
                busDataReq.sel := B"4'b0100"
              }
              is( B"2'd3" ) {
                busDataReq.sel := B"4'b1000"
              }
            }
          }.elsewhen(inst.Op === InstOp.SH ||
            inst.Op === InstOp.LH ||
            inst.Op === InstOp.LHU) {
              switch( B( adr( 1 ) ) ) {
                is( B"1'd0" ) {
                  busDataReq.sel := B"4'b0011"
                }
                is( B"1'd1" ) {
                  busDataReq.sel := B"4'b1100"
                }
              }
              }.otherwise {
                busDataReq.sel := B"4'b1111"
              }

              //Only Stores modify data
              when(inst.Op === InstOp.SB) {
                busDataReq.data := 0
                switch( B( adr( 1 downto 0 ) ) ) {
                  is( B"2'd0" ) {
                    busDataReq.data( 7 downto 0 ) := rs2Data( 7 downto 0 )
                  }
                  is( B"2'd1" ) {
                    busDataReq.data( 15 downto 8 ) := rs2Data( 7 downto 0 )
                  }
                  is( B"2'd2" ) {
                    busDataReq.data( 23 downto 16 ) := rs2Data( 7 downto 0 )
                  }
                  is( B"2'd3" ) {
                    busDataReq.data( 31 downto 24 ) := rs2Data( 7 downto 0 )
                  }
                }
                }.elsewhen(inst.Op === InstOp.SH) {
                  busDataReq.data := 0
                  switch( B( adr( 1 ) ) ) {
                    is( B"1'd0" ) {
                      busDataReq.data( 15 downto 0 ) := rs2Data( 15 downto 0 )
                    }
                    is( B"1'd1" ) {
                      busDataReq.data( 31 downto 16 ) := rs2Data( 15 downto 0 )
                    }
                  }
                  }.otherwise {
                    busDataReq.data := rs2Data
                  }
      }
    }

    //In pending state we are waiting to capture bus rsp
    is( State.PENDING ) {
      when( busData.rsp.ack) {
        state := State.RCVD

        //Capture bus data
        busDataRsp := busData.rsp
      }
    }

    //In recieved state we update registers
    is( State.RCVD ) {
      state := State.IDLE
      done := True

      inst.Vld := False

      //Only Loads can write registers
      wr := False
      wr.setWhen(inst.Op === InstOp.LB ||
        inst.Op === InstOp.LH ||
        inst.Op === InstOp.LW ||
        inst.Op === InstOp.LBU ||
        inst.Op === InstOp.LHU ||
        inst.Op === InstOp.CLWSP ||
        inst.Op === InstOp.CLW)

      switch( inst.Op ) {
        is( InstOp.LB ) {
          switch( B( adr( 1 downto 0 ) ) ) {
            is( B"2'd0" ) {
              data := B(
                S( busDataRsp.data( 7 downto 0 ).resized ),
                32 bits
              )
            }
            is( B"2'd1" ) {
              data := B(
                S( busDataRsp.data( 15 downto 8 ).resized ),
                32 bits
              )
            }
            is( B"2'd2" ) {
              data := B(
                S( busDataRsp.data( 23 downto 16 ).resized ),
                32 bits
              )
            }
            is( B"2'd3" ) {
              data := B(
                S( busDataRsp.data( 31 downto 24 ).resized ),
                32 bits
              )
            }
          }
        }
        is( InstOp.LH ) {
          switch( B( adr( 1 ) ) ) {
            is( B"1'd0" ) {
              data := B(
                S( busDataRsp.data( 15 downto 0 ).resized ),
                32 bits
              )
            }
            is( B"1'd1" ) {
              data := B(
                S( busDataRsp.data( 31 downto 16 ).resized ),
                32 bits
              )
            }
          }
        }
        is( InstOp.LW ) {
          data := busDataRsp.data
        }
        is( InstOp.CLWSP ) {
          data := busDataRsp.data
        }
        is( InstOp.CLW ) {
          data := busDataRsp.data
        }
        is( InstOp.LBU ) {
          switch( B( adr( 1 downto 0 ) ) ) {
            is( B"2'd0" ) {
              data := B(
                U( busDataRsp.data( 7 downto 0 ).resized ),
                32 bits
              )
            }
            is( B"2'd1" ) {
              data := B(
                U( busDataRsp.data( 15 downto 8 ).resized ),
                32 bits
              )
            }
            is( B"2'd2" ) {
              data := B(
                U( busDataRsp.data( 23 downto 16 ).resized ),
                32 bits
              )
            }
            is( B"2'd3" ) {
              data := B(
                U( busDataRsp.data( 31 downto 24 ).resized ),
                32 bits
              )
            }
          }
        }
        is( InstOp.LHU ) {
          switch( B( adr( 1 ) ) ) {
            is( B"1'd0" ) {
              data := B(
                U( busDataRsp.data( 15 downto 0 ).resized ),
                32 bits
              )
            }
            is( B"1'd1" ) {
              data := B(
                U( busDataRsp.data( 31 downto 16 ).resized ),
                32 bits
              )
            }
          }
        }
      }
    }
  }

  when( inst.Rd === 0 ) {
    data := 0
  }

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
  when(
    inst.Op === InstOp.SW || inst.Op === InstOp.SH || inst.Op === InstOp.SB || inst.Op === InstOp.CSWSP || inst.Op === InstOp.CSW
  ) {
    rvfi.rs2_addr := inst.Rs2
    rvfi.rs2_rdata := rs2Data
  } otherwise {
    rvfi.rs2_addr := 0
    rvfi.rs2_rdata := 0
  }
  rvfi.rs1_rdata := rs1Data
  rvfi.rd_addr := inst.Rd
  when( wr ) {
    rvfi.rd_addr := inst.Rd
    rvfi.rd_wdata := data
  } otherwise {
    rvfi.rd_addr := 0
    rvfi.rd_wdata := 0
  }
  rvfi.pc_rdata := B( inst.Adr )
  rvfi.pc_wdata := B( PCNext )
  rvfi.mem_addr := B( busDataReq.adr )
  when( busDataReq.we ) {
    rvfi.mem_rmask := 0
    rvfi.mem_wmask := busDataReq.sel
    rvfi.mem_wdata := busDataReq.data
  } otherwise {
    rvfi.mem_rmask := busDataReq.sel
    rvfi.mem_wmask := 0
    rvfi.mem_wdata := 0
  }
  rvfi.mem_rdata := busDataRsp.data
  rvfi.csr_mcycle_rmask := 0
  rvfi.csr_mcycle_wmask := 0
  rvfi.csr_mcycle_rdata := 0
  rvfi.csr_mcycle_wdata := 0
  rvfi.csr_minstret_rmask := 0
  rvfi.csr_minstret_wmask := 0
  rvfi.csr_minstret_rdata := 0
  rvfi.csr_minstret_wdata := 0

}
