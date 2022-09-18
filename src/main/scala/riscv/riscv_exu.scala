package riscv

import rvfimon._
import wishbone._

import spinal.core._
import spinal.lib._

case class EXUCntlInstr() extends Bundle {
  val vld = Bool
  val misfetch = Bool
  val brTaken = Bool
  val brNotTaken = Bool
  val brCompressed = Bool
  val brInterruptReturn = Bool
  val PC = UInt( 32 bits )
  val PCNext = UInt( 32 bits )
}

case class EXUCntl() extends Bundle {
  val idle = Bool
  val freeze = Bool
  val instr = EXUCntlInstr()
}

//Hardware definition
class riscv_exu( config: riscv_config ) extends Component {
  val fsmCntl = in( FSMCntl() )
  val instDecoded = in( InstDecoded() )
  val mepc = in( MEPC() )
  val cntl = out( EXUCntl() )
  val freeze = out( Bool )
  val idle = out( Bool )
  val retired = out( Bool )
  val misfetch = out( Bool )
  val misfetchPC = out( UInt( 32 bits ) )
  val misfetchAdr = out( UInt( 32 bits ) )
  val brTaken = out( Bool )
  val brNotTaken = out( Bool )
  val brCompressed = out( Bool )
  val brPC = out( UInt( 32 bits ) )
  val brInterruptReturn = out( Bool )
  val busData = master( WishBone( config.busWishBoneConfig ) )
  val csrData = master( WishBone( config.csrWishBoneConfig ) )

  val rvfi = out( Vec( RvfiMon(), 6 ) )

  val order = Reg( UInt( 64 bits ) )
  order init ( 0)

  val x = Vec( Reg( Bits( 32 bits ) ), 32 )
  x( 0 ) init ( 0)
  //for(entry <- x) {
  //  entry init(0)
  //}

  val hazard = new Bool

  val alu = new riscv_alu()
  val aluOp = new Bool
  val aluHazard = new Bool
  alu.instDecoded <> instDecoded
  alu.x <> x
  alu.order <> order
  alu.rvfi <> rvfi( 0 )
  alu.capture := False

  val bru = new riscv_bru()
  val bruOp = new Bool
  val bruHazard = new Bool
  bru.instDecoded <> instDecoded
  bru.mepc <> mepc
  bru.x <> x
  bru.order <> order
  bru.rvfi <> rvfi( 1 )
  bru.capture := False

  val lsu = new riscv_lsu( config )
  val lsuOp = new Bool
  val lsuHazard = new Bool
  lsu.instDecoded <> instDecoded
  lsu.x <> x
  lsu.order <> order
  lsu.rvfi <> rvfi( 2 )
  lsu.busData <> busData
  lsu.capture := False

  val mpu = new riscv_mpu()
  val mpuOp = new Bool
  val mpuHazard = new Bool
  mpu.instDecoded <> instDecoded
  mpu.x <> x
  mpu.order <> order
  mpu.rvfi <> rvfi( 3 )
  mpu.capture := False

  val dvu = new riscv_dvu()
  val dvuOp = new Bool
  val dvuHazard = new Bool
  dvu.instDecoded <> instDecoded
  dvu.x <> x
  dvu.order <> order
  dvu.rvfi <> rvfi( 4 )
  dvu.capture := False

  val csu = new riscv_csu( config )
  val csuOp = new Bool
  val csuHazard = new Bool
  csu.instDecoded <> instDecoded
  csu.x <> x
  csu.order <> order
  csu.rvfi <> rvfi( 5 )
  csu.csrData <> csrData
  csu.capture := False

  idle := ~( alu.busy && ~alu.done) &
    ~( bru.busy && ~bru.done) &
    ~( lsu.busy && ~lsu.done) &
    ~( mpu.busy && ~mpu.done) &
    ~( dvu.busy && ~dvu.done) &
    ~( csu.busy && ~csu.done)
  cntl.idle := ~( alu.busy && ~alu.done) &
    ~( bru.busy && ~bru.done) &
    ~( lsu.busy && ~lsu.done) &
    ~( mpu.busy && ~mpu.done) &
    ~( dvu.busy && ~dvu.done) &
    ~( csu.busy && ~csu.done)

  retired := alu.done |
    bru.done |
    lsu.done |
    mpu.done |
    dvu.done |
    csu.done
  cntl.instr.vld := alu.done |
    bru.done |
    lsu.done |
    mpu.done |
    dvu.done |
    csu.done

  aluOp :=
    instDecoded.Op === InstOp.LUI ||
      instDecoded.Op === InstOp.AUIPC ||
      instDecoded.Op === InstOp.ADD ||
      instDecoded.Op === InstOp.ADDI ||
      instDecoded.Op === InstOp.SLTI ||
      instDecoded.Op === InstOp.SLTIU ||
      instDecoded.Op === InstOp.XORI ||
      instDecoded.Op === InstOp.ORI ||
      instDecoded.Op === InstOp.ANDI ||
      instDecoded.Op === InstOp.SLLI ||
      instDecoded.Op === InstOp.SRLI ||
      instDecoded.Op === InstOp.SRAI ||
      instDecoded.Op === InstOp.ADD ||
      instDecoded.Op === InstOp.SUB ||
      instDecoded.Op === InstOp.SLL ||
      instDecoded.Op === InstOp.SLT ||
      instDecoded.Op === InstOp.SLTU ||
      instDecoded.Op === InstOp.XOR ||
      instDecoded.Op === InstOp.SRL ||
      instDecoded.Op === InstOp.SRA ||
      instDecoded.Op === InstOp.OR ||
      instDecoded.Op === InstOp.AND ||
      instDecoded.Op === InstOp.CLI ||
      instDecoded.Op === InstOp.CLUI ||
      instDecoded.Op === InstOp.CADDI ||
      instDecoded.Op === InstOp.CADDI16SP ||
      instDecoded.Op === InstOp.CADDI4SPN ||
      instDecoded.Op === InstOp.CSLLI ||
      instDecoded.Op === InstOp.CSRLI ||
      instDecoded.Op === InstOp.CSRAI ||
      instDecoded.Op === InstOp.CANDI ||
      instDecoded.Op === InstOp.CMV ||
      instDecoded.Op === InstOp.CADD ||
      instDecoded.Op === InstOp.CAND ||
      instDecoded.Op === InstOp.COR ||
      instDecoded.Op === InstOp.CXOR ||
      instDecoded.Op === InstOp.CSUB ||
      instDecoded.Op === InstOp.CNOP
  bruOp :=
    instDecoded.Op === InstOp.JAL ||
      instDecoded.Op === InstOp.JALR ||
      instDecoded.Op === InstOp.BEQ ||
      instDecoded.Op === InstOp.BNE ||
      instDecoded.Op === InstOp.BLT ||
      instDecoded.Op === InstOp.BGE ||
      instDecoded.Op === InstOp.BLTU ||
      instDecoded.Op === InstOp.BGEU ||
      instDecoded.Op === InstOp.CJ ||
      instDecoded.Op === InstOp.CJAL ||
      instDecoded.Op === InstOp.CJR ||
      instDecoded.Op === InstOp.CJALR ||
      instDecoded.Op === InstOp.CBEQZ ||
      instDecoded.Op === InstOp.CBNEZ ||
      instDecoded.Op === InstOp.MRET
  lsuOp :=
    instDecoded.Op === InstOp.LB ||
      instDecoded.Op === InstOp.LH ||
      instDecoded.Op === InstOp.LW ||
      instDecoded.Op === InstOp.LBU ||
      instDecoded.Op === InstOp.LHU ||
      instDecoded.Op === InstOp.SB ||
      instDecoded.Op === InstOp.SH ||
      instDecoded.Op === InstOp.SW ||
      instDecoded.Op === InstOp.CLWSP ||
      instDecoded.Op === InstOp.CSWSP ||
      instDecoded.Op === InstOp.CLW ||
      instDecoded.Op === InstOp.CSW
  mpuOp :=
    instDecoded.Op === InstOp.MUL ||
      instDecoded.Op === InstOp.MULH ||
      instDecoded.Op === InstOp.MULHSU ||
      instDecoded.Op === InstOp.MULHU
  dvuOp :=
    instDecoded.Op === InstOp.DIV ||
      instDecoded.Op === InstOp.DIVU ||
      instDecoded.Op === InstOp.REM ||
      instDecoded.Op === InstOp.REMU
  csuOp :=
    instDecoded.Op === InstOp.CSRRW ||
      instDecoded.Op === InstOp.CSRRS ||
      instDecoded.Op === InstOp.CSRRC ||
      instDecoded.Op === InstOp.CSRRWI ||
      instDecoded.Op === InstOp.CSRRSI ||
      instDecoded.Op === InstOp.CSRRCI

  //Simple hazard checking for now
  aluHazard := alu.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === alu.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === alu.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === alu.rd))
  bruHazard := bru.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === bru.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === bru.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === bru.rd))
  lsuHazard := lsu.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === lsu.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === lsu.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === lsu.rd))
  mpuHazard := mpu.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === mpu.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === mpu.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === mpu.rd))
  dvuHazard := dvu.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === dvu.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === dvu.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === dvu.rd))
  csuHazard := csu.busy &&
    ( ( U( instDecoded.Rs1 ) =/= 0 && U( instDecoded.Rs1 ) === csu.rs1) ||
      ( U( instDecoded.Rs2 ) =/= 0 && U( instDecoded.Rs2 ) === csu.rs2) ||
      ( U( instDecoded.Rd ) =/= 0 && U( instDecoded.Rd ) === csu.rd))
  if (config.outOfOrder) {
    hazard := aluHazard || bruHazard || lsuHazard || mpuHazard || dvuHazard || csuHazard
  } else {
    hazard := alu.busy || bru.busy || lsu.busy || mpuHazard || dvu.busy || csu.busy
  }

  brTaken := False
  brNotTaken := False
  brCompressed := False
  brInterruptReturn := False
  cntl.instr.brTaken := False
  cntl.instr.brNotTaken := False
  cntl.instr.brCompressed := False
  cntl.instr.brInterruptReturn := False
  when( bru.done ) {
    brTaken := bru.taken
    brNotTaken := bru.nottaken
    brCompressed := bru.compressed
    brInterruptReturn := bru.interruptReturn
    cntl.instr.brTaken := bru.taken
    cntl.instr.brNotTaken := bru.nottaken
    cntl.instr.brCompressed := bru.compressed
    cntl.instr.brInterruptReturn := bru.interruptReturn
  }
  brPC := bru.PC

  //Detect any misfetches
  //TODO any reason why not just check bru?
  //TODO figure put how to mux UInt
  when( alu.done ) {
    misfetch := alu.misfetch
    misfetchPC := alu.PC
    misfetchAdr := alu.PCNext
    cntl.instr.misfetch := alu.misfetch
    cntl.instr.PC := alu.PC
    cntl.instr.PCNext := alu.PCNext
  } elsewhen
    ( bru.done) {
      misfetch := bru.misfetch
      misfetchPC := bru.PC
      misfetchAdr := bru.PCNext
      cntl.instr.misfetch := bru.misfetch
      cntl.instr.PC := bru.PC
      cntl.instr.PCNext := bru.PCNext
    } elsewhen
    ( lsu.done) {
      misfetch := lsu.misfetch
      misfetchPC := lsu.PC
      misfetchAdr := lsu.PCNext
      cntl.instr.misfetch := lsu.misfetch
      cntl.instr.PC := lsu.PC
      cntl.instr.PCNext := lsu.PCNext
    } elsewhen
    ( mpu.done) {
      misfetch := mpu.misfetch
      misfetchPC := mpu.PC
      misfetchAdr := mpu.PCNext
      cntl.instr.misfetch := mpu.misfetch
      cntl.instr.PC := mpu.PC
      cntl.instr.PCNext := mpu.PCNext
    } elsewhen
    ( dvu.done) {
      misfetch := dvu.misfetch
      misfetchPC := dvu.PC
      misfetchAdr := dvu.PCNext
      cntl.instr.misfetch := dvu.misfetch
      cntl.instr.PC := dvu.PC
      cntl.instr.PCNext := dvu.PCNext
    } elsewhen
    ( csu.done) {
      misfetch := csu.misfetch
      misfetchPC := csu.PC
      misfetchAdr := csu.PCNext
      cntl.instr.misfetch := csu.misfetch
      cntl.instr.PC := csu.PC
      cntl.instr.PCNext := csu.PCNext
    } otherwise {
      misfetch := False
      misfetchPC := 0
      misfetchAdr := 0
      cntl.instr.misfetch := False
      cntl.instr.PC := 0
      cntl.instr.PCNext := 0
    }

  freeze := False
  cntl.freeze := False
  when( instDecoded.Vld && ~cntl.instr.misfetch && ~fsmCntl.flush ) {
    when( aluOp ) {
      when( ( alu.busy && ~alu.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        alu.capture := True
      }
    }
    when( bruOp ) {
      when( ( bru.busy && ~bru.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        bru.capture := True
      }
    }
    when( lsuOp ) {
      when( ( lsu.busy && ~lsu.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        lsu.capture := True
      }
    }
    when( mpuOp ) {
      when( ( mpu.busy && ~mpu.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        mpu.capture := True
      }
    }
    when( dvuOp ) {
      when( ( dvu.busy && ~dvu.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        dvu.capture := True
      }
    }
    when( csuOp ) {
      when( ( csu.busy && ~csu.done) || ( hazard) ) {
        freeze := True
        cntl.freeze := True
      } otherwise {
        csu.capture := True
      }
    }
    when( ~cntl.freeze ) {
      order := order + 1
    }
  }

  when( alu.done && alu.wr && ( alu.ndx =/= 0) ) {
    x( alu.ndx ) := alu.data
  }
  when( bru.done && bru.wr && ( bru.ndx =/= 0) ) {
    x( bru.ndx ) := bru.data
  }
  when( lsu.done && lsu.wr && ( lsu.ndx =/= 0) ) {
    x( lsu.ndx ) := lsu.data
  }
  when( mpu.done && mpu.wr && ( mpu.ndx =/= 0) ) {
    x( mpu.ndx ) := mpu.data
  }
  when( dvu.done && dvu.wr && ( dvu.ndx =/= 0) ) {
    x( dvu.ndx ) := dvu.data
  }
  when( csu.done && csu.wr && ( csu.ndx =/= 0) ) {
    x( csu.ndx ) := csu.data
  }

}

class multiplier extends BlackBox {
  val clock = in( Bool )
  val dataa = in( SInt( 32 bits ) )
  val datab = in( SInt( 32 bits ) )
  val result = out( SInt( 64 bits ) )
  mapCurrentClockDomain( clock = clock )
}

class multiplier_unsigned extends BlackBox {
  val clock = in( Bool )
  val dataa = in( UInt( 32 bits ) )
  val datab = in( UInt( 32 bits ) )
  val result = out( UInt( 64 bits ) )
  mapCurrentClockDomain( clock = clock )
}

class multiplier_signed_unsigned extends BlackBox {
  val clock = in( Bool )
  val dataa = in( SInt( 32 bits ) )
  val datab = in( UInt( 33 bits ) )
  val result = out( SInt( 65 bits ) )
  mapCurrentClockDomain( clock = clock )
}

class divider extends BlackBox {
  val clock = in( Bool )
  val denom = in( SInt( 32 bits ) )
  val numer = in( SInt( 32 bits ) )
  val quotient = out( SInt( 32 bits ) )
  val remain = out( SInt( 32 bits ) )
  mapCurrentClockDomain( clock = clock )
}

class divider_unsigned extends BlackBox {
  val clock = in( Bool )
  val denom = in( UInt( 32 bits ) )
  val numer = in( UInt( 32 bits ) )
  val quotient = out( UInt( 32 bits ) )
  val remain = out( UInt( 32 bits ) )
  mapCurrentClockDomain( clock = clock )
}
