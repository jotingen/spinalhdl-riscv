package riscv

import wishbone._

import spinal.core._
import spinal.lib._

case class Inst() extends Bundle {
  val Vld = Bool
  val Interrupt = Bool
  val Adr = UInt( 32 bits )
  val AdrNext = UInt( 32 bits )
  val Data = Bits( 32 bits )
}

case class InstBuffEntry() extends Bundle {
  val AdrVld = Bool
  val Adr = UInt( 32 bits )
  val PairedWithNextEntry = Bool
  val DataVld = Bool
  val Data = Bits( 16 bits )
}

case class InstBuff( config: riscv_config ) extends Bundle {
  val wrNdx = Reg( UInt( log2Up( config.bufferSize ) bits ) )
  val wrDataNdx = Reg( UInt( log2Up( config.bufferSize ) bits ) )
  val rdNdx = Reg( UInt( log2Up( config.bufferSize ) bits ) )
  val buffer = Vec( Reg( InstBuffEntry() ), config.bufferSize )

  wrNdx init ( 0)
  wrDataNdx init ( 0)
  rdNdx init ( 0)
  for (entry <- buffer) {
    entry.AdrVld init ( False)
    entry.DataVld init ( False)
  }

  def Clear(): Unit = {
    wrDataNdx := wrNdx
    rdNdx := wrNdx
    for (entry <- buffer) {
      entry.AdrVld := False
      entry.DataVld := False
    }
  }
  def PushAdr( adr: UInt, sel: Bits, number: UInt ): Unit = {
    buffer( wrNdx ).AdrVld := True
    buffer( wrNdx ).Adr := adr
    buffer( wrNdx ).PairedWithNextEntry := sel( 1 downto 0 ).orR
    when( number === U"2'd2" ) {
      buffer( wrNdx + 1 ).AdrVld := True
      buffer( wrNdx + 1 ).Adr := adr + 2
      buffer( wrNdx + 1 ).PairedWithNextEntry := False
      wrNdx := wrNdx + 2
    } otherwise {
      wrNdx := wrNdx + 1
    }
  }
  def PushData( data: Bits ): Unit = {
    //Unaligned
    when( buffer( wrDataNdx ).Adr( 1 ) ) {
      buffer( wrDataNdx ).DataVld := True
      buffer( wrDataNdx ).Data := data( 31 downto 16 )
      wrDataNdx := wrDataNdx + 1
      //Aligned
    } otherwise {
      //If the two address spaces are not contiguous,
      //then we only wanted first half
      buffer( wrDataNdx ).DataVld := True
      buffer( wrDataNdx ).Data := data( 15 downto 0 )
      when( buffer( wrDataNdx ).PairedWithNextEntry ) {
        buffer( wrDataNdx + 1 ).DataVld := True
        buffer( wrDataNdx + 1 ).Data := data( 31 downto 16 )
        wrDataNdx := wrDataNdx + 2
      } otherwise {
        wrDataNdx := wrDataNdx + 1
      }
    }
  }
  def Pull(): Inst = {
    val inst = Inst()
    inst.Vld := buffer( rdNdx ).DataVld
    inst.Interrupt := False
    inst.Adr := buffer( rdNdx ).Adr
    inst.Data( 15 downto 0 ) := buffer( rdNdx ).Data
    //Normal instruction
    when( buffer( rdNdx ).Data( 1 downto 0 ) === B"2'b11" ) {
      inst.Data( 31 downto 16 ) := buffer( rdNdx + 1 ).Data
      inst.AdrNext :=
        buffer(
          rdNdx + 2
        ).Adr //Buffer always being filled, this should have been written
      buffer( rdNdx ).AdrVld := False
      buffer( rdNdx ).DataVld := False
      buffer( rdNdx + 1 ).AdrVld := False
      buffer( rdNdx + 1 ).DataVld := False
      rdNdx := rdNdx + 2
      //Compressed instruction
    } otherwise {
      inst.Data( 31 downto 16 ) := 0
      inst.AdrNext :=
        buffer(
          rdNdx + 1
        ).Adr //Buffer always being filled, this should have been written
      buffer( rdNdx ).AdrVld := False
      buffer( rdNdx ).DataVld := False
      rdNdx := rdNdx + 1
    }
    return inst
  }

  //Emtpy if first entry is invalid,
  //or if first entry is normal op and second entry invalid
  def Empty(): Bool =
    ~buffer( rdNdx ).DataVld ||
      ~buffer( rdNdx + 1 ).DataVld
  //  ( buffer( rdNdx ).Data( 1 downto 0 ) === B"2'b11" &&
  //    ~buffer( rdNdx + 1 ).DataVld)

  def Full(): Bool =
    buffer( wrNdx ).AdrVld ||
      buffer( wrNdx + 1 ).AdrVld
}

//Hardware definition
class riscv_ifu( config: riscv_config ) extends Component {
  val fsmCntl = in( FSMCntl() )
  val misfetch = in( Bool )
  val misfetchPC = in( UInt( 32 bits ) )
  val misfetchAdr = in( UInt( 32 bits ) )
  val brTaken = in( Bool )
  val brNotTaken = in( Bool )
  val brCompressed = in( Bool )
  val brPC = in( UInt( 32 bits ) )
  val freeze = in( Bool )
  val idle = in( Bool )
  val inst = out( Reg( Inst() ) )
  val busInst = master( WishBone( config.busWishBoneConfig ) )

  val busInstReq = Reg( WishBoneReq( config.busWishBoneConfig ) )
  busInst.req <> busInstReq

  busInstReq.cyc init ( False)
  busInstReq.stb init ( False)
  busInstReq.we init ( False)
  busInstReq.adr init ( 0)
  busInstReq.sel init ( 0)
  busInstReq.data init ( 0)
  busInstReq.tga init ( 0)
  busInstReq.tgd init ( 0)
  busInstReq.tgc init ( 0)

  val token = Reg( UInt( 4 bits ) )
  token init ( 1)

  val PC = Reg( UInt( 32 bits ) )
  PC init ( 0)

  val ibp = new riscv_ibp( config )
  ibp.adr <> PC
  ibp.misfetch <> misfetch
  ibp.misfetchPC <> misfetchPC
  ibp.misfetchAdr <> misfetchAdr
  ibp.brTaken <> brTaken
  ibp.brNotTaken <> brNotTaken
  ibp.brCompressed <> brCompressed
  ibp.brPC <> brPC

  val buf = InstBuff( config )
  val bufFull = Bool
  bufFull := buf.Full()
  val bufEmpty = Bool
  bufEmpty := buf.Empty()

  //Toggle on when flushing for interrupt
  val interrupt = Reg( Bool )
  interrupt init ( False)

  busInstReq.cyc := False
  busInstReq.stb := False
  //If were flushing, set up PC and clear buffer
  when( fsmCntl.flush ) {
    buf.Clear()
    token := token + 1
    PC := fsmCntl.flushPC
    when( fsmCntl.flushIntr ) {
      interrupt := True
    }
  } otherwise {
    //If we get a stall, hold same command
    when( busInst.stall ) {
      busInstReq := busInstReq
      //If buffer is full, do nothing
    } elsewhen ( buf.Full() ) {
      //Else continue requesting
    } otherwise {
      busInstReq.cyc := True
      busInstReq.stb := True
      busInstReq.we := False
      busInstReq.adr := PC
      busInstReq.data := 0
      busInstReq.tga := 0
      busInstReq.tgd := 0
      busInstReq.tgc := B( token )
      busInstReq.sel := ibp.sel
      PC := ibp.adrNext
      //Unaligned
      when( PC( 1 ) ) {
        buf.PushAdr( PC, ibp.sel, U"2'd1" )
      } otherwise {
        when( ibp.sel === B"4'b1111" ) {
          buf.PushAdr( PC, ibp.sel, U"2'd2" )
        } otherwise {
          buf.PushAdr( PC, ibp.sel, U"2'd1" )
        }
      }
    }
  }

  when(
    busInst.rsp.ack && ( U( busInst.rsp.tgc ) === token) && ~fsmCntl.flush
  ) {
    //Convert incoming little endian data to big endian to work with
    buf.PushData( EndiannessSwap( busInst.rsp.data ) )
  }

  val interruptedSent = Reg( Bool )
  interruptedSent init ( False)
  interruptedSent := interruptedSent

  //If were frozen, do nothing
  when( freeze ) {
    inst := inst
    //If were flushing, clear stage
  } elsewhen ( fsmCntl.flush) {
    inst.Vld := False
    //If were halted, clear and do nothing
  } elsewhen ( fsmCntl.halt) {
    inst.Vld := False
    //Else just take from buffer
  } otherwise {
    interruptedSent := False
    when( ~buf.Empty() ) {
      if (config.oneShotInst) {
        when( ~idle ) {
          inst.Vld := False
        } otherwise {
          inst := buf.Pull()
          when( interrupt ) {
            inst.Interrupt := True
            interruptedSent := True
          }
          interrupt := False
        }
      } else {
        inst := buf.Pull()
        when( interrupt ) {
          inst.Interrupt := True
          interruptedSent := True
        }
        interrupt := False
      }
    } otherwise {
      inst.Vld := False
    }
  }

}
