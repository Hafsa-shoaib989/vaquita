package vaquita.components
import chisel3._
import chisel3.util._
import vaquita.configparameter.VaquitaConfig


class VecControlUnit(implicit val config: VaquitaConfig) extends Module {
    val io = IO (new Bundle {
      val instr            = Input(UInt(32.W))
      val reg_write        = Output(Bool())
      val mem_write        = Output(Bool())
      val mem_read         = Output(Bool())
      val mem_to_reg       = Output(Bool())
      val operand_type     = Output(UInt(2.W))
      val vec_config       = Output(Bool())
      val store_vs3_to_mem = Output(Bool())
      val is_float         = Output(Bool())    // Indicates if floating point instruction
    })

    def checkCondition(opcode: UInt, funct3: UInt): Bool = {
        (opcode === "b1010111".U && (funct3 === "b111".U || funct3 === "b000".U || funct3 === "b100".U || funct3 === "b011".U)) ||
        (opcode === "b0100111".U) ||
        (opcode === "b0000111".U)
    }

    def setValues(memWrite: Bool, operandType: UInt, memRead: Bool, regWrite: Bool, memToReg: Bool, vecConfig: Bool, storeVs3ToMem: Bool, is_float: Bool): Unit = {
        io.mem_write        := memWrite
        io.operand_type     := operandType
        io.mem_read         := memRead
        io.reg_write        := regWrite
        io.mem_to_reg       := memToReg
        io.vec_config       := vecConfig
        io.store_vs3_to_mem := storeVs3ToMem
        io.is_float         := config.F.B  // Only enable F operations, if F parameter is true
    }

    // Default values
    setValues(false.B, "b11".U, false.B, false.B, false.B, false.B, false.B, false.B) 

    // Integer vector instructions
    switch(io.instr(6,0)) {
        is("b1010111".U) {   
        switch(io.instr(14,12)) {
            is("b111".U) { setValues(false.B, "b11".U, false.B, false.B, false.B, true.B, false.B, false.B) }//vec config
            is("b000".U) { setValues(false.B, "b00".U, false.B, true.B, false.B, false.B, false.B, false.B) }//vec to vec
            is("b100".U) { setValues(false.B, "b01".U, false.B, true.B, false.B, false.B, false.B, false.B) }//vec to scalar
            is("b011".U) { setValues(false.B, "b10".U, false.B, true.B, false.B, false.B, false.B, false.B) }//vec to immediate
        }
        }
        is("b0100111".U) { setValues(true.B,  "b00".U, false.B, false.B, false.B, false.B, true.B, false.B) } //vec store
        is("b0000111".U) { setValues(false.B, "b11".U, true.B, true.B,   true.B, false.B, false.B, false.B) } // vec load
    }
    

    // Floating point vector instructions
    when(config.F.B) { 
      switch(io.instr(6,0)) {  
        is("b010010".U) {  //opcode
        switch(io.instr(14,12)) {  //func3
            is("b001".U) { setValues(false.B, "b00".U, false.B, true.B, false.B, false.B, false.B, true.B) } //conversion (vec to vec)
        }
        }
        is("b1010111".U) {   
        switch(io.instr(14,12)) {  
            is("b001".U) { setValues(false.B, "b00".U, false.B, true.B, false.B, false.B, false.B, true.B) }//vec to vec
            is("b101".U) { setValues(false.B, "b01".U, false.B, true.B, false.B, false.B, false.B, true.B) }//vec to scalar
        }
        }
      }
    }
}