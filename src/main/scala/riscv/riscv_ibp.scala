package riscv

import wishbone._

import spinal.core._
import spinal.lib._

class mor1kx_cache_lru_accessfix( NUMWAYS: Int ) extends BlackBox {
  val generic =
    new Generic {
      val NUMWAYS = mor1kx_cache_lru_accessfix.this.NUMWAYS
    }

  val WIDTH =
    mor1kx_cache_lru_accessfix.this.NUMWAYS * ( mor1kx_cache_lru_accessfix.this.NUMWAYS - 1) >> 1

  val current = in( Bits( WIDTH bits ) )
  val update = out( Bits( WIDTH bits ) )
  val accessing = in( Bits( NUMWAYS bits ) )
  val lru_pre = out( Bits( NUMWAYS bits ) )
  val lru_post = out( Bits( NUMWAYS bits ) )
}

case class BrBuffEntry() extends Bundle {
  val Adr = UInt( 32 bits )
  val AdrNext = UInt( 32 bits )
  val Prediction = Bits( 4 bits )
  val Compressed = Bool()
}

//Hardware definition
class riscv_ibp( config: riscv_config ) extends Component {
  val misfetch = in( Bool )
  val misfetchPC = in( UInt( 32 bits ) )
  val misfetchAdr = in( UInt( 32 bits ) )
  val brTaken = in( Bool )
  val brNotTaken = in( Bool )
  val brCompressed = in( Bool )
  val brPC = in( UInt( 32 bits ) )
  val adr = in( UInt( 32 bits ) )
  val sel = out( Bits( 4 bits ) )
  val adrNext = out( UInt( 32 bits ) )
  val selNext = out( Bits( 4 bits ) )

  val lru = new mor1kx_cache_lru_accessfix( config.branchPredSize )
  val lruCurrent = Reg( Bits )
  val lruAccess = Bits( config.branchPredSize bits )
  lruCurrent init ( 0)

  val buffer = Vec( Reg( BrBuffEntry() ), config.branchPredSize )
  for (ndx <- buffer.indices) {
    buffer( ndx ).Adr init ( ndx * 4)
    buffer( ndx ).AdrNext init ( ndx * 4 + 4)
    buffer( ndx ).Prediction init ( B"4'b0010")
    buffer( ndx ).Compressed init ( False)
  }

  sel := B"4'b1111"

  lruAccess := 0

  //Helper signals for branch table
  val adrUnaligned = Bool
  val adrM2 = UInt( 32 bits )
  val adrM2Hit = Bool
  val adrM2Compressed = Bool
  val adrM2Taken = Bool
  val adrM2Pred = UInt( 32 bits )
  val adrM2Access = Bits( config.branchPredSize bits )
  val adrHit = Bool
  val adrCompressed = Bool
  val adrTaken = Bool
  val adrPred = UInt( 32 bits )
  val adrAccess = Bits( config.branchPredSize bits )
  val adrP2 = UInt( 32 bits )
  val adrP2Hit = Bool
  val adrP2Compressed = Bool
  val adrP2Taken = Bool
  val adrP2Pred = UInt( 32 bits )
  val adrP2Access = Bits( config.branchPredSize bits )
  val adrP4 = UInt( 32 bits )

  adrUnaligned := adr( 1 )
  adrM2 := adr - 2
  adrM2Hit := False
  adrM2Compressed := False
  adrM2Taken := False
  adrM2Pred := 0
  adrM2Access := 0
  adrHit := False
  adrCompressed := False
  adrTaken := False
  adrPred := 0
  adrAccess := 0
  adrP2 := adr + 2
  adrP2Hit := False
  adrP2Compressed := False
  adrP2Taken := False
  adrP2Pred := 0
  adrP2Access := 0
  adrP4 := adr + 4

  for (ndx <- buffer.indices) {
    when( buffer( ndx ).Adr === adrM2 ) {
      adrM2Hit := True
      adrM2Compressed := buffer( ndx ).Compressed
      adrM2Taken := buffer( ndx ).Prediction( 3 downto 2 ).orR
      adrM2Pred := buffer( ndx ).AdrNext
      adrM2Access( ndx ) := True
    }
    when( buffer( ndx ).Adr === adr ) {
      adrHit := True
      adrCompressed := buffer( ndx ).Compressed
      adrTaken := buffer( ndx ).Prediction( 3 downto 2 ).orR
      adrPred := buffer( ndx ).AdrNext
      adrAccess( ndx ) := True
    }
    when( buffer( ndx ).Adr === adrP2 ) {
      adrP2Hit := True
      adrP2Compressed := buffer( ndx ).Compressed
      adrP2Taken := buffer( ndx ).Prediction( 3 downto 2 ).orR
      adrP2Pred := buffer( ndx ).AdrNext
      adrP2Access( ndx ) := True
    }
  }

  val misfetchNonBr = Bool
  val misfetchBrTaken = Bool
  val misfetchBrNotTaken = Bool

  misfetchNonBr := misfetch && ~( brTaken || brNotTaken)
  misfetchBrTaken := misfetch && brTaken
  misfetchBrNotTaken := misfetch && brNotTaken

  when( misfetch && ( brTaken || brNotTaken) ) {

    //Default to lru value
    lruAccess := lru.lru_pre

    //Override if we have an address match
    val adrMatched = Bool
    adrMatched := False
    for (ndx <- buffer.indices) {
      when( buffer( ndx ).Adr === misfetchPC ) {
        adrMatched := True
        lruAccess := 0
        lruAccess( ndx ) := True
      }
    }

    //Update entry with new values
    for (ndx <- buffer.indices) {
      when( lruAccess( ndx ) ) {
        buffer( ndx ).Adr := misfetchPC
        buffer( ndx ).AdrNext := misfetchAdr
        when( adrMatched ) {
          when( brTaken ) {
            switch( buffer( ndx ).Prediction ) {
              is( B"4'b0001" ) {
                buffer( ndx ).Prediction := B"4'b0010"
              }
              is( B"4'b0010" ) {
                buffer( ndx ).Prediction := B"4'b0100"
              }
              is( B"4'b0100" ) {
                buffer( ndx ).Prediction := B"4'b1000"
              }
              is( B"4'b1000" ) {
                buffer( ndx ).Prediction := B"4'b1000"
              }
            }
          }
          when( brNotTaken ) {
            switch( buffer( ndx ).Prediction ) {
              is( B"4'b0001" ) {
                buffer( ndx ).Prediction := B"4'b0001"
              }
              is( B"4'b0010" ) {
                buffer( ndx ).Prediction := B"4'b0001"
              }
              is( B"4'b0100" ) {
                buffer( ndx ).Prediction := B"4'b0010"
              }
              is( B"4'b1000" ) {
                buffer( ndx ).Prediction := B"4'b0100"
              }
            }
          }
        } otherwise {
          //Make new prediction
          buffer( ndx ).Prediction := B"4'b0100"
        }
        buffer( ndx ).Compressed := brCompressed
      }
    }
    //Update prediction
    for (ndx <- buffer.indices) {
      when( buffer( ndx ).Adr === brPC ) {
        when( brTaken ) {
          switch( buffer( ndx ).Prediction ) {
            is( B"4'b0001" ) {
              buffer( ndx ).Prediction := B"4'b0010"
            }
            is( B"4'b0010" ) {
              buffer( ndx ).Prediction := B"4'b0100"
            }
            is( B"4'b0100" ) {
              buffer( ndx ).Prediction := B"4'b1000"
            }
            is( B"4'b1000" ) {
              buffer( ndx ).Prediction := B"4'b1000"
            }
          }
        }
        when( brNotTaken ) {
          switch( buffer( ndx ).Prediction ) {
            is( B"4'b0001" ) {
              buffer( ndx ).Prediction := B"4'b0001"
            }
            is( B"4'b0010" ) {
              buffer( ndx ).Prediction := B"4'b0001"
            }
            is( B"4'b0100" ) {
              buffer( ndx ).Prediction := B"4'b0010"
            }
            is( B"4'b1000" ) {
              buffer( ndx ).Prediction := B"4'b0100"
            }
          }
        }
      }
    }

    adrNext := misfetchAdr

  } otherwise {

    //Should not get hit, make 0 to find faster
    adrNext := 0 //adrP4
    sel := 0     //B"4'b1111"
    lruAccess := 0

    val caseSel = Bits( 10 bits )
    caseSel := adrUnaligned ##
      adrM2Hit ##
      adrM2Compressed ##
      adrM2Taken ##
      adrHit ##
      adrCompressed ##
      adrTaken ##
      adrP2Hit ##
      adrP2Compressed ##
      adrP2Taken

    when( caseSel === M"00--0--0--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }

    when( caseSel === M"00--0--10-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"00--0--110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := adrAccess
    }
    when( caseSel === M"00--0--111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }

    when( caseSel === M"00--100---" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"00--101---" ) {
      adrNext := adrPred
      sel := B"4'b1111"
      lruAccess := adrAccess
    }
    when( caseSel === M"00--1100--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"00--11010-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"00--110110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"00--110111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }
    when( caseSel === M"00--111---" ) {
      adrNext := adrPred
      sel := B"4'b1100"
      lruAccess := adrAccess
    }

    when( caseSel === M"01000--0--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"01000--10-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"01000--110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"01000--111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }
    when( caseSel === M"01010-----" ) {
      adrNext := adrM2Pred
      sel := B"4'b1100"
      lruAccess := adrM2Access
    }
    when( caseSel === M"011-0--0--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-0--10-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-0--110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-0--111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }

    when( caseSel === M"01001000--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"010010010-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"0100100110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"0100100111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }
    when( caseSel === M"0101100---" ) {
      adrNext := adrM2Pred
      sel := B"4'b1100"
      lruAccess := adrM2Access
    }
    when( caseSel === M"011-1000--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-10010-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-100110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-100111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }

    when( caseSel === M"0100101---" ) {
      adrNext := adrPred
      sel := B"4'b1111"
      lruAccess := adrAccess
    }
    when( caseSel === M"0101101---" ) {
      adrNext := adrM2Pred
      sel := B"4'b1100"
      lruAccess := adrM2Access
    }
    when( caseSel === M"011-101---" ) {
      adrNext := adrPred
      sel := B"4'b1111"
      lruAccess := adrAccess
    }

    when( caseSel === M"01001100--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"010011010-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"0100110110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"0100110111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }
    when( caseSel === M"0101110---" ) {
      adrNext := adrM2Pred
      sel := B"4'b1100"
      lruAccess := adrM2Access
    }
    when( caseSel === M"011-1100--" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-11010-" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-110110" ) {
      adrNext := adrP4
      sel := B"4'b1111"
      lruAccess := 0
    }
    when( caseSel === M"011-110111" ) {
      adrNext := adrP2Pred
      sel := B"4'b1111"
      lruAccess := adrP2Access
    }

    when( caseSel === M"0100111---" ) {
      adrNext := adrPred
      sel := B"4'b1100"
      lruAccess := adrAccess
    }
    when( caseSel === M"0101111---" ) {
      adrNext := adrM2Pred
      sel := B"4'b1100"
      lruAccess := adrM2Access
    }
    when( caseSel === M"011-111---" ) {
      adrNext := adrPred
      sel := B"4'b1100"
      lruAccess := adrAccess
    }

    when( caseSel === M"10--0-----" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }

    when( caseSel === M"10--100---" ) {
      adrNext := adr + 1
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"10--101---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"10--110---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"10--111---" ) {
      adrNext := adrPred
      sel := B"4'b0011"
      lruAccess := adrAccess
    }

    when( caseSel === M"11000-----" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"11010-----" ) {
      adrNext := adrM2Pred
      sel := B"4'b0011"
      lruAccess := adrM2Access
    }
    when( caseSel === M"111-0-----" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }

    when( caseSel === M"1100100---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"1101100---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"111-100---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }

    when( caseSel === M"1100101---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"1101101---" ) {
      adrNext := adrM2Pred
      sel := B"4'b0011"
      lruAccess := adrM2Access
    }
    when( caseSel === M"111-101---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }

    when( caseSel === M"1100110---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"1101110---" ) {
      adrNext := adrM2Pred
      sel := B"4'b0011"
      lruAccess := adrM2Access
    }
    when( caseSel === M"111-110---" ) {
      adrNext := adrP2
      sel := B"4'b0011"
      lruAccess := 0
    }

    when( caseSel === M"1100111---" ) {
      adrNext := adrPred
      sel := B"4'b0011"
      lruAccess := adrAccess
    }
    when( caseSel === M"1101111---" ) {
      adrNext := adrP2 //Weird case
      sel := B"4'b0011"
      lruAccess := 0
    }
    when( caseSel === M"111-111---" ) {
      adrNext := adrPred
      sel := B"4'b0011"
      lruAccess := adrAccess
    }

    //Update prediction
    for (ndx <- buffer.indices) {
      when( buffer( ndx ).Adr === brPC ) {
        when( brTaken ) {
          switch( buffer( ndx ).Prediction ) {
            is( B"4'b0001" ) {
              buffer( ndx ).Prediction := B"4'b0010"
            }
            is( B"4'b0010" ) {
              buffer( ndx ).Prediction := B"4'b0100"
            }
            is( B"4'b0100" ) {
              buffer( ndx ).Prediction := B"4'b1000"
            }
            is( B"4'b1000" ) {
              buffer( ndx ).Prediction := B"4'b1000"
            }
          }
        }
        when( brNotTaken ) {
          switch( buffer( ndx ).Prediction ) {
            is( B"4'b0001" ) {
              buffer( ndx ).Prediction := B"4'b0001"
            }
            is( B"4'b0010" ) {
              buffer( ndx ).Prediction := B"4'b0001"
            }
            is( B"4'b0100" ) {
              buffer( ndx ).Prediction := B"4'b0010"
            }
            is( B"4'b1000" ) {
              buffer( ndx ).Prediction := B"4'b0100"
            }
          }
        }
      }
    }

  }

  lru.accessing <> lruAccess
  lruCurrent := lru.update
  lru.current <> lruCurrent

}
