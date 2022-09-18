package riscv

import spinal.core._
import spinal.lib._

case class FSMCntl() extends Bundle {
  val halt = Bool
  val flush = Bool
  val flushIntr = Bool
  val flushPC = UInt( 32 bits )
  val MEIP = Bool
  val MEPCVal = Bool
  val MEPC = UInt( 32 bits )
}

object RiscvState extends SpinalEnum {
  val sReset, sNormal, sMisfetch, sInterruptPend, sInterrupt, sInterruptRet =
    newElement()
}

class riscv_fsm( config: riscv_config ) extends Component {
  val idle = in( Bool )
  val exuCntl = in( EXUCntl() )
  val IRQ = in( Bits( 32 bits ) )
  val cntl = out( Reg( FSMCntl() ) )

  import RiscvState._

  val statePrev = Reg( RiscvState() )
  val state = Reg( RiscvState() )
  val stateNext = RiscvState()

  val mstatus = in( MStatus() )
  val mie = in( MIE() )
  cntl.MEIP := mstatus.MIE & mie.MEIE & IRQ.orR
  val mip = in( MIP() )
  val mtvec = in( MTVec() )
  val mepc = in( MEPC() )

  state init ( sReset)

  cntl.halt := False

  cntl.flush := False
  cntl.flushIntr := cntl.flushIntr
  cntl.flushPC := cntl.flushPC

  cntl.MEPCVal := False
  cntl.MEPC := cntl.MEPC
  when( exuCntl.instr.vld ) {
    cntl.MEPC := exuCntl.instr.PCNext
  }

  //Determine next state
  statePrev := statePrev
  switch( state ) {
    is( sReset ) {
      stateNext := sNormal
    }
    is( sNormal ) {
      stateNext := sNormal
      when( exuCntl.instr.misfetch ) {
        stateNext := sMisfetch
      } elsewhen ( cntl.MEIP) {
        stateNext := sInterruptPend
      }
      //Update in normal state
      statePrev := state
    }
    is( sInterruptPend ) {
      stateNext := sInterruptPend
      when( idle ) {
        stateNext := sInterrupt
      }
      when( exuCntl.instr.misfetch ) {
        stateNext := sMisfetch
      }
      //Update in interrupt pending state
      statePrev := state
    }
    is( sInterrupt ) {
      stateNext := sInterrupt
      when( exuCntl.instr.misfetch ) {
        stateNext := sMisfetch
      }
      when( exuCntl.instr.brInterruptReturn ) {
        stateNext := sInterruptRet
      }
      //Update in interrupt state
      statePrev := state
    }
    is( sInterruptRet ) {
      stateNext := sNormal
    }
    is( sMisfetch ) {
      stateNext := statePrev
      when( exuCntl.instr.brInterruptReturn ) {
        stateNext := sInterruptRet
      }
    }
  }

  state := stateNext

  //Prepare controls based off of next state
  switch( stateNext ) {
    is( sReset ) {
      cntl.flush := True
    }
    is( sNormal ) {}
    is( sInterruptPend ) {
      cntl.halt := True
      cntl.MEPCVal := True
      cntl.flush := True
      cntl.flushIntr := True
      cntl.flushPC := mtvec.Base.asUInt << 2
    }
    is( sInterrupt ) {}
    is( sInterruptRet ) {
      cntl.flush := True
      cntl.flushIntr := False
      cntl.flushPC := mepc.PC.asUInt
    }
    is( sMisfetch ) {
      cntl.flush := True
      cntl.flushIntr := False
      cntl.flushPC := exuCntl.instr.PCNext
    }
  }

  //Incoming misfetch must be a pulse
  val assert_misfetchD1 = Reg( Bool )
  assert_misfetchD1 := exuCntl.instr.misfetch
  assert(
    assertion = !( exuCntl.instr.misfetch && assert_misfetchD1),
    message   = "Misfetch signal must be a single pulse",
    severity  = ERROR
  )

  //Misfetch must not flag with idle
  assert(
    assertion = !( exuCntl.instr.misfetch && idle),
    message   = "Misfetch must not flag with idle",
    severity  = ERROR
  )

  //when( misfetch ) {
  //  statePrev := state
  //  when(brInterruptReturn) {
  //    statePrev := sNormal
  //  }
  //  state := sMisfetch
  //  flush := True
  //  flushIntr := False
  //  flushAdr := misfetchAdr
  //  cntl.flush := True
  //  cntl.flushIntr := False
  //  cntl.flushPC := misfetchAdr
  //} otherwise {
  //  switch( state ) {
  //    is( sReset ) {
  //      state := sNormal
  //    }
  //    is( sNormal ) {
  //      state := sNormal
  //      when( B(mip).orR ) {
  //        state := sInterruptPend
  //      }
  //    }
  //    is( sInterruptPend ) {
  //      cntl.halt := True
  //      halt := True
  //      state := sInterruptPend
  //      flush := True
  //      flushIntr := True
  //      flushAdr := mtvec.Base.asUInt << 2
  //      cntl.flush := True
  //      cntl.flushIntr := True
  //      cntl.flushPC := mtvec.Base.asUInt << 2
  //      when( idle ) {
  //        setMEPCVal := True
  //        cntl.MEPCVal := True
  //        state := sInterrupt
  //      }
  //    }
  //    is( sInterrupt ) {
  //      state := sInterrupt
  //      when(brInterruptReturn) {
  //        state := sNormal
  //      }
  //    }
  //    is( sMisfetch ) {
  //      state := statePrev
  //      when(brInterruptReturn) {
  //        state := sNormal
  //      }
  //    }
  //  }
  //}

}
