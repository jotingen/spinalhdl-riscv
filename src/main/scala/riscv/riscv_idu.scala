package riscv

import spinal.core._
import spinal.lib._

object InstOp extends SpinalEnum( defaultEncoding = binaryOneHot ) {
  //format: off
    val LUI, AUIPC,
        JAL, JALR,
        BEQ, BNE, BLT, BGE, BLTU, BGEU,
        LB, LH, LW, LBU, LHU,
        SB, SH, SW,
        ADDI,
        SLTI, SLTIU,
        XORI, ORI,
        ANDI,
        SLLI, SRLI, SRAI,
        ADD, SUB,
        SLL, SLT, SLTU,
        XOR,
        SRL, SRA,
        OR, AND,
        FENCE, FENCEI,
        ECALL,
        CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI,
        EBREAK,
        MUL, MULH, MULHSU, MULHU,
        DIV, DIVU, REM, REMU,
        TRAP,
        CLWSP,
        CSWSP,
        CLW,
        CSW,
        CJ, CJAL, CJR, CJALR, CBEQZ, CBNEZ, 
        CLI, CLUI,
        CADDI, CADDI16SP, CADDI4SPN,
        CSLLI, CSRLI, CSRAI, 
        CANDI,
        CMV, CADD,
        CAND, COR, CXOR, CSUB, 
        CNOP,
        CEBREAK,
        CTRAP,
        MRET = newElement()
        //format: on
}

case class InstDecoded() extends Bundle    {
  val Vld = Bool
  val Interrupt = Bool
  val Adr = UInt( 32 bits )
  val AdrNext = UInt( 32 bits )
  val Data = Bits( 32 bits )
  val Op = InstOp()
  val Rd = Bits( 5 bits )
  val Rs1 = Bits( 5 bits )
  val Rs2 = Bits( 5 bits )
  val Immed = Bits( 32 bits )
  val CSR = Bits( 12 bits )
  val Pred = Bits( 4 bits )
  val Succ = Bits( 4 bits )
}
//Hardware definition
class riscv_idu          extends Component {
  val fsmCntl = in( FSMCntl() )
  val inst = in( Inst() )
  val freeze = in( Bool )
  val instDecoded = out( Reg( InstDecoded() ) )

  instDecoded.Vld init ( False)

  when( fsmCntl.flush ) {
    //instDecoded := instDecoded
    instDecoded.Vld := False
  } elsewhen
    ( freeze) {
      instDecoded := instDecoded
    } elsewhen
    ( inst.Vld) {
      //Normal instructions
      when( inst.Data( 1 downto 0 ) === B"2'b11" ) {
        val opcode = Bits( 7 bits )
        val funct3 = Bits( 3 bits )
        val funct7 = Bits( 7 bits )
        val immedI = Bits( 32 bits )
        val immedS = Bits( 32 bits )
        val immedB = Bits( 32 bits )
        val immedU = Bits( 32 bits )
        val immedJ = Bits( 32 bits )
        val instData31Repeat = Vec(
          Bool,
          32
        ) //Create a vector for bit31, not sure how to do repititions in spinal yet
        opcode := inst.Data( 6 downto 0 )
        funct3 := inst.Data( 14 downto 12 )
        funct7 := inst.Data( 31 downto 25 )
        for (bit <- instData31Repeat) {
          bit := inst.Data( 31 )
        }
        immedI :=
          Cat(
            instData31Repeat.asBits( 31 downto 11 ),
            inst.Data( 30 downto 25 ),
            inst.Data( 24 downto 21 ),
            inst.Data( 20 )
          )
        immedS :=
          Cat(
            instData31Repeat.asBits( 31 downto 11 ),
            inst.Data( 30 downto 25 ),
            inst.Data( 11 downto 8 ),
            inst.Data( 7 )
          )
        immedB :=
          Cat(
            instData31Repeat.asBits( 31 downto 12 ),
            inst.Data( 7 ),
            inst.Data( 30 downto 25 ),
            inst.Data( 11 downto 8 ),
            B"1'h0"
          )
        immedU :=
          Cat(
            instData31Repeat.asBits( 31 ),
            inst.Data( 30 downto 20 ),
            inst.Data( 19 downto 12 ),
            B"12'h000"
          )
        immedJ :=
          Cat(
            instData31Repeat.asBits( 31 downto 20 ),
            inst.Data( 19 downto 12 ),
            inst.Data( 20 ),
            inst.Data( 30 downto 25 ),
            inst.Data( 24 downto 21 ),
            B"1'h0"
          )

        instDecoded.Vld := inst.Vld
        instDecoded.Interrupt := inst.Interrupt
        instDecoded.Adr := inst.Adr
        instDecoded.AdrNext := inst.AdrNext
        instDecoded.Data := inst.Data
        instDecoded.Rd := inst.Data( 11 downto 7 )
        instDecoded.Rs1 := inst.Data( 19 downto 15 )
        instDecoded.Rs2 := inst.Data( 24 downto 20 )
        instDecoded.CSR := inst.Data( 31 downto 20 )
        instDecoded.Pred := inst.Data( 27 downto 24 )
        instDecoded.Succ := inst.Data( 23 downto 20 )
        switch( opcode ) {
          is( B"7'b0110111" ) {
            instDecoded.Op := InstOp.LUI
            instDecoded.Immed := immedU
          }
          is( B"7'b0010111" ) {
            instDecoded.Op := InstOp.AUIPC
            instDecoded.Immed := immedU
          }
          is( B"7'b1101111" ) {
            instDecoded.Op := InstOp.JAL
            instDecoded.Immed := immedJ
          }
          is( B"7'b1100111" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                instDecoded.Op := InstOp.JALR
                instDecoded.Immed := immedI
              }
              default {
                instDecoded.Op := InstOp.TRAP
              }
            }
          }
          is( B"7'b1100011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                instDecoded.Op := InstOp.BEQ
                instDecoded.Immed := immedB
              }
              is( B"3'b001" ) {
                instDecoded.Op := InstOp.BNE
                instDecoded.Immed := immedB
              }
              is( B"3'b100" ) {
                instDecoded.Op := InstOp.BLT
                instDecoded.Immed := immedB
              }
              is( B"3'b101" ) {
                instDecoded.Op := InstOp.BGE
                instDecoded.Immed := immedB
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.BLTU
                instDecoded.Immed := immedB
              }
              is( B"3'b111" ) {
                instDecoded.Op := InstOp.BGEU
                instDecoded.Immed := immedB
              }
              default {
                instDecoded.Op := InstOp.TRAP
              }
            }
          }
          is( B"7'b0000011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                instDecoded.Op := InstOp.LB
                instDecoded.Immed := immedI
              }
              is( B"3'b001" ) {
                instDecoded.Op := InstOp.LH
                instDecoded.Immed := immedI
              }
              is( B"3'b010" ) {
                instDecoded.Op := InstOp.LW
                instDecoded.Immed := immedI
              }
              is( B"3'b100" ) {
                instDecoded.Op := InstOp.LBU
                instDecoded.Immed := immedI
              }
              is( B"3'b101" ) {
                instDecoded.Op := InstOp.LHU
                instDecoded.Immed := immedI
              }
              default {
                instDecoded.Op := InstOp.TRAP
              }
            }
          }
          is( B"7'b0100011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                instDecoded.Op := InstOp.SB
                instDecoded.Immed := immedS
              }
              is( B"3'b001" ) {
                instDecoded.Op := InstOp.SH
                instDecoded.Immed := immedS
              }
              is( B"3'b010" ) {
                instDecoded.Op := InstOp.SW
                instDecoded.Immed := immedS
              }
              default {
                instDecoded.Op := InstOp.TRAP
              }
            }
          }
          is( B"7'b0010011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                instDecoded.Op := InstOp.ADDI
                instDecoded.Immed := immedI
              }
              is( B"3'b010" ) {
                instDecoded.Op := InstOp.SLTI
                instDecoded.Immed := immedI
              }
              is( B"3'b011" ) {
                instDecoded.Op := InstOp.SLTIU
                instDecoded.Immed := immedI
              }
              is( B"3'b100" ) {
                instDecoded.Op := InstOp.XORI
                instDecoded.Immed := immedI
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.ORI
                instDecoded.Immed := immedI
              }
              is( B"3'b111" ) {
                instDecoded.Op := InstOp.ANDI
                instDecoded.Immed := immedI
              }
              is( B"3'b001" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SLLI
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b101" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SRLI
                  }
                  is( B"7'b0100000" ) {
                    instDecoded.Op := InstOp.SRAI
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              //default {
              //  instDecoded.Op     := InstOp.TRAP
              //}
            }
          }
          is( B"7'b0110011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.ADD
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.MUL
                  }
                  is( B"7'b0100000" ) {
                    instDecoded.Op := InstOp.SUB
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b001" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SLL
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.MULH
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b010" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SLT
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.MULHSU
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b011" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SLTU
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.MULHU
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b100" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.XOR
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.DIV
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b101" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.SRL
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.DIVU
                  }
                  is( B"7'b0100000" ) {
                    instDecoded.Op := InstOp.SRA
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b110" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.OR
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.REM
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b111" ) {
                switch( funct7 ) {
                  is( B"7'b0000000" ) {
                    instDecoded.Op := InstOp.AND
                  }
                  is( B"7'b0000001" ) {
                    instDecoded.Op := InstOp.REMU
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              //default {
              //  instDecoded.Op     := InstOp.TRAP
              //}
            }
          }
          is( B"7'b0001111" ) {
            instDecoded.Op := InstOp.FENCE
          }
          is( B"7'b1110011" ) {
            switch( funct3 ) {
              is( B"3'b000" ) {
                switch( inst.Data( 31 downto 20 ) ) {
                  is( B"12'b000000000000" ) {
                    instDecoded.Op := InstOp.ECALL
                  }
                  is( B"12'b000000000001" ) {
                    instDecoded.Op := InstOp.EBREAK
                  }
                  is( B"12'b001100000010" ) {
                    instDecoded.Op := InstOp.MRET
                  }
                  default {
                    instDecoded.Op := InstOp.TRAP
                  }
                }
              }
              is( B"3'b001" ) {
                instDecoded.Op := InstOp.CSRRW
              }
              is( B"3'b010" ) {
                instDecoded.Op := InstOp.CSRRS
              }
              is( B"3'b011" ) {
                instDecoded.Op := InstOp.CSRRC
              }
              is( B"3'b101" ) {
                instDecoded.Op := InstOp.CSRRWI
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.CSRRSI
              }
              is( B"3'b111" ) {
                instDecoded.Op := InstOp.CSRRCI
              }
              default {
                instDecoded.Op := InstOp.TRAP
              }
            }
          }
          default {
            instDecoded.Op := InstOp.TRAP
          }
        }

        //Compressed
      } otherwise {

        instDecoded.Vld := inst.Vld
        instDecoded.Interrupt := inst.Interrupt
        instDecoded.Adr := inst.Adr
        instDecoded.AdrNext := inst.AdrNext
        instDecoded.Data := inst.Data
        instDecoded.CSR := 0
        instDecoded.Pred := 0
        instDecoded.Succ := 0

        switch( inst.Data( 1 downto 0 ) ) {
          is( B"2'b00" ) {
            switch( inst.Data( 15 downto 13 ) ) {
              is( B"3'b000" ) {
                when( inst.Data( 12 downto 2 ) === 0 ) {
                  instDecoded.Op := InstOp.CTRAP
                } otherwise {
                  instDecoded.Op := InstOp.CADDI4SPN
                  instDecoded.Immed := B(
                    U(
                      Cat(
                        inst.Data( 10 downto 7 ),
                        inst.Data( 12 downto 11 ),
                        inst.Data( 5 ),
                        inst.Data( 6 ),
                        B"2'b00"
                      ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := Cat( B"2'b01", inst.Data( 4 downto 2 ) )
                  instDecoded.Rs1 := B"5'd2"
                  instDecoded.Rs2 := 0
                }
              }
              is( B"3'b010" ) {
                instDecoded.Op := InstOp.CLW
                instDecoded.Immed := B(
                  U(
                    Cat(
                      inst.Data( 5 ),
                      inst.Data( 12 downto 10 ),
                      inst.Data( 6 ),
                      B"2'b00"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := Cat( B"2'b01", inst.Data( 4 downto 2 ) )
                instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                instDecoded.Rs2 := 0
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.CSW
                instDecoded.Immed := B(
                  U(
                    Cat(
                      inst.Data( 5 ),
                      inst.Data( 12 downto 10 ),
                      inst.Data( 6 ),
                      B"2'b00"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 0
                instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                instDecoded.Rs2 := Cat( B"2'b01", inst.Data( 4 downto 2 ) )
              }
              default {
                instDecoded.Op := InstOp.CTRAP
              }
            }
          }
          is( B"2'b01" ) {
            switch( inst.Data( 15 downto 13 ) ) {
              is( B"3'b000" ) {
                when( inst.Data( 12 downto 2 ) === 0 ) {
                  instDecoded.Op := InstOp.CNOP
                } otherwise {
                  instDecoded.Op := InstOp.CADDI
                  instDecoded.Immed := B(
                    S(
                      Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := inst.Data( 11 downto 7 )
                  instDecoded.Rs1 := inst.Data( 11 downto 7 )
                  instDecoded.Rs2 := 0
                }
              }
              is( B"3'b001" ) {
                instDecoded.Op := InstOp.CJAL
                instDecoded.Immed := B(
                  S(
                    Cat(
                      inst.Data( 12 ),
                      inst.Data( 8 ),
                      inst.Data( 10 downto 9 ),
                      inst.Data( 6 ),
                      inst.Data( 7 ),
                      inst.Data( 2 ),
                      inst.Data( 11 ),
                      inst.Data( 5 downto 3 ),
                      B"1'b0"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 1
                instDecoded.Rs1 := 0
                instDecoded.Rs2 := 0
              }
              is( B"3'b010" ) {
                when( inst.Data( 11 downto 7 ) === 0 ) {
                  instDecoded.Op := InstOp.CNOP
                } otherwise {
                  instDecoded.Op := InstOp.CLI
                  instDecoded.Immed := B(
                    S(
                      Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := inst.Data( 11 downto 7 )
                  instDecoded.Rs1 := 0
                  instDecoded.Rs2 := 0
                }
              }
              is( B"3'b011" ) {
                when( inst.Data( 11 downto 7 ) === 0 ) {
                  instDecoded.Op := InstOp.CNOP
                } elsewhen ( inst.Data( 11 downto 7 ) === 2) {
                  instDecoded.Op := InstOp.CADDI16SP
                  instDecoded.Immed := B(
                    S(
                      Cat(
                        inst.Data( 12 ),
                        inst.Data( 4 downto 3 ),
                        inst.Data( 5 ),
                        inst.Data( 2 ),
                        inst.Data( 6 ),
                        B"4'b0000"
                      ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := inst.Data( 11 downto 7 )
                  instDecoded.Rs1 := inst.Data( 11 downto 7 )
                  instDecoded.Rs2 := 0
                } otherwise {
                  instDecoded.Op := InstOp.CLUI
                  instDecoded.Immed := B(
                    S(
                      Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ), B"12'd0" ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := inst.Data( 11 downto 7 )
                  instDecoded.Rs1 := 0
                  instDecoded.Rs2 := 0
                }
              }
              is( B"3'b100" ) {
                switch( inst.Data( 11 downto 10 ) ) {
                  is( B"2'b00" ) {
                    when( inst.Data( 12 ) ) {
                      instDecoded.Op := InstOp.CTRAP
                      instDecoded.Immed := 0
                      instDecoded.Rd := 0
                      instDecoded.Rs1 := 0
                      instDecoded.Rs2 := 0
                    } otherwise {
                      when( inst.Data( 6 downto 2 ) === 0 ) {
                        instDecoded.Op := InstOp.CTRAP
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      } otherwise {
                        instDecoded.Op := InstOp.CSRLI
                        instDecoded.Immed := B(
                          U(
                            Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                            32 bits
                          )
                        )
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := 0
                      }
                    }
                  }
                  is( B"2'b01" ) {
                    when( inst.Data( 12 ) ) {
                      instDecoded.Op := InstOp.CNOP
                      instDecoded.Immed := 0
                      instDecoded.Rd := 0
                      instDecoded.Rs1 := 0
                      instDecoded.Rs2 := 0
                    } otherwise {
                      when( inst.Data( 6 downto 2 ) === 0 ) {
                        instDecoded.Op := InstOp.CTRAP
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      } otherwise {
                        instDecoded.Op := InstOp.CSRAI
                        instDecoded.Immed := B(
                          U(
                            Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                            32 bits
                          )
                        )
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := 0
                      }
                    }
                  }
                  is( B"2'b10" ) {
                    instDecoded.Op := InstOp.CANDI
                    instDecoded.Immed := B(
                      S(
                        Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                        32 bits
                      )
                    )
                    instDecoded.Rd := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                    instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                    instDecoded.Rs2 := 0
                  }
                  is( B"2'b11" ) {
                    switch( Cat( inst.Data( 12 ), inst.Data( 6 downto 5 ) ) ) {
                      is( B"3'b000" ) {
                        instDecoded.Op := InstOp.CSUB
                        instDecoded.Immed := 0
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := Cat(
                          B"2'b01",
                          inst.Data( 4 downto 2 )
                        )
                      }
                      is( B"3'b001" ) {
                        instDecoded.Op := InstOp.CXOR
                        instDecoded.Immed := 0
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := Cat(
                          B"2'b01",
                          inst.Data( 4 downto 2 )
                        )
                      }
                      is( B"3'b010" ) {
                        instDecoded.Op := InstOp.COR
                        instDecoded.Immed := 0
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := Cat(
                          B"2'b01",
                          inst.Data( 4 downto 2 )
                        )
                      }
                      is( B"3'b011" ) {
                        instDecoded.Op := InstOp.CAND
                        instDecoded.Immed := 0
                        instDecoded.Rd := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs1 := Cat(
                          B"2'b01",
                          inst.Data( 9 downto 7 )
                        )
                        instDecoded.Rs2 := Cat(
                          B"2'b01",
                          inst.Data( 4 downto 2 )
                        )
                      }
                      default {
                        instDecoded.Op := InstOp.CTRAP
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      }
                    }
                  }
                }
              }
              is( B"3'b101" ) {
                instDecoded.Op := InstOp.CJ
                instDecoded.Immed := B(
                  S(
                    Cat(
                      inst.Data( 12 ),
                      inst.Data( 8 ),
                      inst.Data( 10 downto 9 ),
                      inst.Data( 6 ),
                      inst.Data( 7 ),
                      inst.Data( 2 ),
                      inst.Data( 11 ),
                      inst.Data( 5 downto 3 ),
                      B"1'b0"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 0
                instDecoded.Rs1 := 0
                instDecoded.Rs2 := 0
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.CBEQZ
                instDecoded.Immed := B(
                  S(
                    Cat(
                      inst.Data( 12 ),
                      inst.Data( 6 downto 5 ),
                      inst.Data( 2 ),
                      inst.Data( 11 downto 10 ),
                      inst.Data( 4 downto 3 ),
                      B"1'b0"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 0
                instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                instDecoded.Rs2 := 0
              }
              is( B"3'b111" ) {
                instDecoded.Op := InstOp.CBNEZ
                instDecoded.Immed := B(
                  S(
                    Cat(
                      inst.Data( 12 ),
                      inst.Data( 6 downto 5 ),
                      inst.Data( 2 ),
                      inst.Data( 11 downto 10 ),
                      inst.Data( 4 downto 3 ),
                      B"1'b0"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 0
                instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                instDecoded.Rs2 := 0
              }
              //default {
              //  instDecoded.Op := InstOp.CTRAP
              //}
            }
          }
          is( B"2'b10" ) {
            switch( inst.Data( 15 downto 13 ) ) {
              is( B"3'b000" ) {
                when( inst.Data( 12 ) ) {
                  instDecoded.Op := InstOp.CTRAP
                  instDecoded.Immed := 0
                  instDecoded.Rd := 0
                  instDecoded.Rs1 := 0
                  instDecoded.Rs2 := 0
                } otherwise {
                  when( inst.Data( 11 downto 7 ) === 0 ) {
                    instDecoded.Op := InstOp.CNOP
                    instDecoded.Immed := 0
                    instDecoded.Rd := 0
                    instDecoded.Rs1 := 0
                    instDecoded.Rs2 := 0
                  } otherwise {
                    instDecoded.Op := InstOp.CSLLI
                    instDecoded.Immed := B(
                      U(
                        Cat( inst.Data( 12 ), inst.Data( 6 downto 2 ) ),
                        32 bits
                      )
                    )
                    instDecoded.Rd := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                    instDecoded.Rs1 := Cat( B"2'b01", inst.Data( 9 downto 7 ) )
                    instDecoded.Rs2 := 0
                  }
                }
              }
              is( B"3'b010" ) {
                when( inst.Data( 11 downto 7 ) === 0 ) {
                  instDecoded.Op := InstOp.CTRAP
                  instDecoded.Immed := 0
                  instDecoded.Rd := 0
                  instDecoded.Rs1 := 0
                  instDecoded.Rs2 := 0
                } otherwise {
                  instDecoded.Op := InstOp.CLWSP
                  instDecoded.Immed := B(
                    U(
                      Cat(
                        inst.Data( 3 downto 2 ),
                        inst.Data( 12 ),
                        inst.Data( 6 downto 4 ),
                        B"2'b00"
                      ),
                      32 bits
                    )
                  )
                  instDecoded.Rd := inst.Data( 11 downto 7 )
                  instDecoded.Rs1 := 2
                  instDecoded.Rs2 := 0
                }
              }
              is( B"3'b100" ) {
                switch( B( inst.Data( 12 ) ) ) {
                  is( B"1'b0" ) {
                    when( inst.Data( 6 downto 2 ) === 0 ) {
                      instDecoded.Op := InstOp.CJR
                      instDecoded.Immed := 0
                      instDecoded.Rd := 0
                      instDecoded.Rs1 := inst.Data( 11 downto 7 )
                      instDecoded.Rs2 := inst.Data( 6 downto 2 )
                    } otherwise {
                      when( inst.Data( 11 downto 7 ) === 0 ) {
                        instDecoded.Op := InstOp.CNOP
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      } otherwise {
                        instDecoded.Op := InstOp.CMV
                        instDecoded.Immed := 0
                        instDecoded.Rd := inst.Data( 11 downto 7 )
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := inst.Data( 6 downto 2 )
                      }
                    }
                  }
                  is( B"1'b1" ) {
                    when( inst.Data( 6 downto 2 ) === 0 ) {
                      when( inst.Data( 11 downto 7 ) === 0 ) {
                        instDecoded.Op := InstOp.CEBREAK
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      } otherwise {
                        instDecoded.Op := InstOp.CJALR
                        instDecoded.Immed := 0
                        instDecoded.Rd := 1
                        instDecoded.Rs1 := inst.Data( 11 downto 7 )
                        instDecoded.Rs2 := inst.Data( 6 downto 2 )
                      }
                    } otherwise {
                      when( inst.Data( 11 downto 7 ) === 0 ) {
                        instDecoded.Op := InstOp.CNOP
                        instDecoded.Immed := 0
                        instDecoded.Rd := 0
                        instDecoded.Rs1 := 0
                        instDecoded.Rs2 := 0
                      } otherwise {
                        instDecoded.Op := InstOp.CADD
                        instDecoded.Immed := 0
                        instDecoded.Rd := inst.Data( 11 downto 7 )
                        instDecoded.Rs1 := inst.Data( 11 downto 7 )
                        instDecoded.Rs2 := inst.Data( 6 downto 2 )
                      }
                    }
                  }
                }
              }
              is( B"3'b110" ) {
                instDecoded.Op := InstOp.CSWSP
                instDecoded.Immed := B(
                  U(
                    Cat(
                      inst.Data( 8 downto 7 ),
                      inst.Data( 12 downto 9 ),
                      B"2'b00"
                    ),
                    32 bits
                  )
                )
                instDecoded.Rd := 0
                instDecoded.Rs1 := 2
                instDecoded.Rs2 := inst.Data( 6 downto 2 )
              }
              default {
                instDecoded.Op := InstOp.CTRAP
                instDecoded.Immed := 0
                instDecoded.Rd := 0
                instDecoded.Rs1 := 0
                instDecoded.Rs2 := 0
              }
            }
          }
          default {
            instDecoded.Op := InstOp.CTRAP
            instDecoded.Immed := 0
            instDecoded.Rd := 0
            instDecoded.Rs1 := 0
            instDecoded.Rs2 := 0
          }
        }
      }
    } otherwise {
      instDecoded.Vld := False
    }
  instDecoded.Vld init ( False)
}
