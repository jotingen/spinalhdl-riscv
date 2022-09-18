package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

class riscv_dvu extends Component {
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

  val order = in( UInt( 64 bits ) )
  val rvfi = out( RvfiMon() )

  val inst = Reg( InstDecoded() )
  inst.Vld init ( False)

  val rvfi_order = Reg( UInt( 64 bits ) )
  rvfi_order init ( 0)

  val divider = new divider()
  val divider_unsigned = new divider_unsigned()

  val cycleMax = U( 10 )
  val cycle = Reg( UInt( 4 bits ) )

  busy <> inst.Vld
  rs1 <> U( inst.Rs1 )
  rs2 <> U( inst.Rs2 )
  rd <> U( inst.Rd )

  PC := inst.Adr

  //Calculate new data
  val rs1Data = B( x( U( inst.Rs1 ) ) )
  val rs2Data = B( x( U( inst.Rs2 ) ) )
  divider.numer <> S( rs1Data )
  divider_unsigned.numer <> U( rs1Data )
  divider.denom <> S( rs2Data )
  divider_unsigned.denom <> U( rs2Data )
  data := 0
  when( inst.Vld ) {
    switch( inst.Op ) {
      is( InstOp.DIV ) {
        when( cycle < cycleMax ) {
          cycle := cycle + 1
        } otherwise {
          data := B( divider.quotient )
        }
      }
      is( InstOp.DIVU ) {
        when( cycle < cycleMax ) {
          cycle := cycle + 1
        } otherwise {
          data := B( divider_unsigned.quotient )
        }
      }
      is( InstOp.REM ) {
        when( cycle < cycleMax ) {
          cycle := cycle + 1
        } otherwise {
          data := B( divider.remain )
        }
      }
      is( InstOp.REMU ) {
        when( cycle < cycleMax ) {
          cycle := cycle + 1
        } otherwise {
          data := B( divider_unsigned.remain )
        }
      }
      default {
        cycle := cycleMax
        data := B( "32'd0" )
      }
    }
  }
  when( inst.Rd === 0 ) {
    data := 0
  }

  done := False
  wr := False
  ndx := U( inst.Rd )
  PCNext := inst.Adr + 4
  misfetch := inst.AdrNext =/= PCNext
  when( inst.Vld && ( cycle >= cycleMax) ) {
    done := True
    wr := True
    inst.Vld := False
  }
  when( capture ) {
    cycle := 0
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
