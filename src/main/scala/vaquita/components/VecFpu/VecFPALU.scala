package vaquita.components.VecFpu
import chisel3._
import chisel3.util._
import FPALUObj._
import hardfloat._
import vaquita.configparameter.VaquitaConfig
import VecFPParameters._

class VecFPALU(implicit val config: VaquitaConfig, val FPConfig: VecFPParameters) extends Module {
    val io = IO(new Bundle {
        val vs1_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        val vs2_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        val vs3_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))  // mask undisturbed ..and tail undisturbed 
        val vs0_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))  // for maksing which element 
        val sew          = Input(UInt(3.W))
        val vl_in        = Input(UInt(32.W)) //on how much elements i want to work ..body elements 
        val alu_ctrl     = Input(UInt(6.W))  // for arithmethic 
        val alu_ctrl_con = Input(UInt(11.W)) // for conversion 
        val mask_arith   = Input(Bool()) //want to apply masking or not?
        val vsd_out      = Output(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        // val exceptions   = Output(UInt(5.W))
    })
// first we see mask_arith , then vso_in, and then vs3_in 
// tailing work associated with ...vs3 and vl 

//convert into one array (string)...for making masking easy...
val vs0_mask = io.vs0_in.asUInt()(config.vlen,0)

val exception_reg = RegInit(0.U(5.W))
// io.exceptions := exception_reg

//CONVERSION INSTRUCTIONS
def intToFloat(vs2_in: SInt, signed: Bool): SInt = {
    val conv = Module(new INToRecFN(32, FPConfig.expWidth, FPConfig.sigWidth))
    conv.io.signedIn := signed
    conv.io.in := vs2_in.asUInt  
    conv.io.roundingMode := 0.U
    conv.io.detectTininess := consts.tininess_afterRounding
    exception_reg := conv.io.exceptionFlags
    fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, conv.io.out.asSInt).asSInt
}

def floatToInt(vs2_in: SInt, signed: Bool, roundingMode: UInt): SInt = {
    val recFN = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in)

    val conv = Module(new RecFNToIN(FPConfig.expWidth, FPConfig.sigWidth, 32))
    conv.io.in := recFN
    conv.io.roundingMode := roundingMode 
    conv.io.signedOut := signed
    exception_reg := conv.io.intExceptionFlags
    conv.io.out.asSInt
}

def Conversion(vs2_in: SInt): SInt = {
    MuxLookup(io.alu_ctrl_con, vs2_in, Seq(
        vfcvt_f_xu_v     -> intToFloat(vs2_in, signed = false.B),
        vfcvt_f_x_v      -> intToFloat(vs2_in, signed = true.B),
        vfcvt_xu_f_v     -> floatToInt(vs2_in, signed = false.B, roundingMode = 0.U),
        vfcvt_x_f_v      -> floatToInt(vs2_in, signed = true.B, roundingMode = 0.U),
        vfcvt_rtz_xu_f_v -> floatToInt(vs2_in, signed = false.B, roundingMode = 1.U), 
        vfcvt_rtz_x_f_v  -> floatToInt(vs2_in, signed = true.B, roundingMode = 1.U)    
    ))
}


//ARITHMETIC INSTRUCTIONS
def applyArithmeticOp(vs2_in: SInt, vs1_in: SInt, opType: UInt): SInt = {
    val result = Wire(SInt((FPConfig.expWidth + FPConfig.sigWidth).W))
    result := 0.S // Default

    opType match {
        case `vfadd` | `vfsub` | `vfrsub` =>
            val add = Module(new AddRecFN(FPConfig.expWidth, FPConfig.sigWidth))
            add.io.a := vs2_in.asUInt
            add.io.b := vs1_in.asUInt
            when (opType === vfsub || opType === vfrsub) {
                add.io.subOp := true.B
            } .otherwise {
                add.io.subOp := false.B
            } 
            add.io.roundingMode := 0.U
            add.io.detectTininess := consts.tininess_afterRounding
            exception_reg := add.io.exceptionFlags
            result := fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, add.io.out).asSInt

        case `vfmul` =>
            val mul = Module(new MulRecFN(FPConfig.expWidth, FPConfig.sigWidth))
            mul.io.a := vs2_in.asUInt
            mul.io.b := vs1_in.asUInt
            mul.io.roundingMode := 0.U
            mul.io.detectTininess := consts.tininess_afterRounding
            exception_reg := mul.io.exceptionFlags
            result := fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, mul.io.out).asSInt

        case vfdiv | vfrdiv =>
            val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
            div.io.a := vs2_in.asUInt
            div.io.b := vs1_in.asUInt
            div.io.sqrtOp := false.B
            div.io.inValid := true.B
            val internalReady = WireDefault(true.B)
            internalReady := div.io.inReady 
            div.io.roundingMode := 0.U
            div.io.detectTininess := consts.tininess_afterRounding
            exception_reg := div.io.exceptionFlags
            when(div.io.outValid_div || div.io.outValid_sqrt) {
                result := fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, div.io.out).asSInt
        }

        case _ =>
        result := 0.S
    }
    result
}

def Arithmetic(vs2_in: SInt, vs1_in: SInt): SInt = {
  MuxLookup(io.alu_ctrl, vs2_in, Seq(
    vfadd  -> applyArithmeticOp(vs2_in, vs1_in, vfadd),
    vfsub  -> applyArithmeticOp(vs2_in, vs1_in, vfsub),
    vfrsub -> applyArithmeticOp(vs1_in, vs2_in, vfrsub),
    vfmul  -> applyArithmeticOp(vs2_in, vs1_in, vfmul),  
    vfdiv  -> applyArithmeticOp(vs2_in, vs1_in, vfdiv),
    vfrdiv -> applyArithmeticOp(vs1_in, vs2_in, vfdiv),
    vfmin  -> Mux(vs2_in < vs1_in, vs2_in, vs1_in),
    vfmax  -> Mux(vs2_in > vs1_in, vs2_in, vs1_in)
  ))
}


// for sew 
def arith_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew32_b = WireInit(0.S(32.W))
    val vec_sew32_result = WireInit(0.S(config.XLEN.W))
    // when(io.alu_ctrl==="b010000".U || io.alu_ctrl==="b010010".U){
    //     vec_sew32_b := (Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt)).asSInt
    //     }.otherwise{

    // Define known conversion operations
    val isConversionOp = io.alu_ctrl_con === vfcvt_f_xu_v ||
                         io.alu_ctrl_con === vfcvt_f_x_v ||
                         io.alu_ctrl_con === vfcvt_xu_f_v ||
                         io.alu_ctrl_con === vfcvt_x_f_v ||
                         io.alu_ctrl_con === vfcvt_rtz_xu_f_v ||
                         io.alu_ctrl_con === vfcvt_rtz_x_f_v

    val computed_result = Mux(isConversionOp, Conversion(vs2.asSInt), Arithmetic(vs2.asSInt, vs1.asSInt))       // Compute result based on operation type

    vec_sew32_b := Mux(mask_bit_active_element===1.B,computed_result,Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
        // }
    vec_sew32_result := vec_sew32_b
    vec_sew32_result
}


// call main function 
val vl= 4
val tail = 0.B

when(io.sew==="b010".U){//sew = 32    
    // when(comp_bit === 0.B) {
    var vl_counter = 1
    for (i <- 0 until 8) {
        for (j <- 0 until config.count_lanes) {
        val idx = (i * config.count_lanes) + j
        val mask = vs0_mask(idx)
        
        io.vsd_out(i)(j) := Mux(io.vl_in >= vl_counter.U,
            arith_32(io.vs1_in(i)(j),io.vs2_in(i)(j), io.vs3_in(i)(j), mask),
            Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
        )     
        vl_counter = vl_counter + 1
        }
    }
}.otherwise{
    for (i <- 0 until 8) {
        for (j <- 0 until config.count_lanes) {
            io.vsd_out(i)(j) := 0.S
        }
    }
}

}




















// def Conversion(vs2_in: SInt, sew:Int): SInt = {
//     // int to float
//     val intToFloat = Module(new INToRecFN(sew, FPConfig.expWidth, FPConfig.sigWidth))
//     intToFloat.io.signedIn := false.B  // Unsigned input
//     intToFloat.io.in := vs2_in.asUInt
//     intToFloat.io.roundingMode := 0.U  // Round to nearest even
//     intToFloat.io.detectTininess := consts.tininess_afterRounding

//     io.exceptions := intToFloat.io.exceptionFlags

//     val recodedToFloat = fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, sew)
//     recodedToFloat.asUInt


//     // float to int 
//     val floatToInt = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, sew)
//     floatToInt.asUInt

//     val recodedToInt = Module(new RecFNToIN(FPConfig.expWidth, FPConfig.sigWidth, sew))
//     recodedToInt.io.in := 
//     recodedToInt.io.roundingMode := 0.U
//     recodedToInt.io.signedOut := false.B

//     io.exceptions := recodedToInt.io.intExceptionFlags
// }



// when(io.sew==="b000".U){
// //   when(comp_bit===0.B){
//     var vl_counter = 0
//     for (i <- 0 until 8) {
//         for (j <- 0 until config.count_lanes) {
//         val idx = (i * config.count_lanes) + j
//         io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+3.U,                                                        
//         arith_8(io.vs2_in(i)(j)(31,24).asSInt, vs0_mask(vl_counter+3)), //ye vl_counter vstart + body elements ka abata rha 
//         Mux(tail === 0.B, io.vs3_in(i)(j)(31,24).asSInt, Fill(8, 1.U).asSInt)),

//         Mux(io.vl_in > vl_counter.U +2.U,
//         arith_8(io.vs2_in(i)(j)(23,16).asSInt, vs0_mask(vl_counter+2)),
//         Mux(tail === 0.B, io.vs3_in(i)(j)(23,16).asSInt, Fill(8, 1.U).asSInt)),
        
//         Mux(io.vl_in > vl_counter.U+1.U,
//         arith_8(io.vs2_in(i)(j)(15,8).asSInt, vs0_mask(vl_counter+1)),
//         Mux(tail === 0.B, io.vs3_in(i)(j)(15,8).asSInt, Fill(8, 1.U).asSInt)),

//         Mux(io.vl_in > vl_counter.U,
//         arith_8(io.vs2_in(i)(j)(7,0).asSInt, vs0_mask(vl_counter)),
//         Mux(tail === 0.B, io.vs3_in(i)(j)(7,0).asSInt, Fill(8, 1.U).asSInt))
//         ).asSInt
//         vl_counter = vl_counter + 4
//         }
//     }

// }.elsewhen(io.sew==="b001".U){//sew=16
//     //   when(comp_bit===0.B){
//     val vec_sew16_b = WireInit(0.S(16.W))
//     dontTouch(vec_sew16_b)
//     val sew16_result = WireInit(0.S(config.XLEN.W))
//     var vl_counter = 0
//     for (i <- 0 until 8) {
//         for (j <- 0 until config.count_lanes) {
//         val idx = (i * config.count_lanes) + j
//         io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+1.U,
//         arith_16(io.vs2_in(i)(j)(31,16).asSInt, vs0_mask(vl_counter+1)),
//         Mux(tail === 0.B, io.vs3_in(i)(j)(31,16).asSInt, Fill(16, 1.U).asSInt)),
//         Mux(io.vl_in > vl_counter.U,
//         arith_16(io.vs2_in(i)(j)(15,0).asSInt, vs0_mask(vl_counter)),
//         Mux(tail === 0.B, io.vs3_in(i)(j)(15,0).asSInt, Fill(16, 1.U).asSInt))).asSInt
//         vl_counter = vl_counter + 2 //counter_of_2 * 2
//     }
// }

// }.else




























































//     def Arithmatic(vs1_in: SInt, vs2_in: SInt,vs3:SInt): SInt = {
//         val lookuptable = Seq(
//             vadd   -> (vs1_in + vs2_in),//add
//             vsub   -> (vs2_in - vs1_in),//sub
//             vrsub  -> (vs1_in - vs2_in),//rsub
//             vand   -> (vs1_in & vs2_in),// and
//             vor    -> (vs1_in | vs2_in),//or
//             vxor   -> (vs1_in ^ vs2_in),//xor
//         )
//         MuxLookup(io.alu_ctrl, 0.S, lookuptable)
//     }
//     def arith_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
//         val vsetvli_mask = 0.B
//         val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
//         val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
//         val vec_sew32_b = WireInit(0.S(32.W))
//         val vec_sew32_result = WireInit(0.S(config.XLEN.W))
//         // when(io.alu_ctrl==="b010000".U || io.alu_ctrl==="b010010".U){
//         //     vec_sew32_b := (Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt)).asSInt
//         //     }.otherwise{
//         vec_sew32_b := Mux(mask_bit_active_element===1.B,Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
//             // }
//         vec_sew32_result := vec_sew32_b
//         vec_sew32_result
//     }

//     def arith_16(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
//         val vsetvli_mask = 0.B
//         val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
//         val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
//         val vec_sew16_result = WireInit(0.S(16.W))
//         // when(io.alu_opcode==="b010000".U || io.alu_opcode==="b010010".U){
//         //     vec_sew16_result := (Arithmatic(vs1.asSInt, vs2.asSInt,vs3,16,mask_vs0.asUInt)).asSInt
//         //     }.otherwise{
//         vec_sew16_result := Mux(mask_bit_active_element===1.B,Arithmatic(vs1.asSInt, vs2.asSInt,vs3,16,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(16,1.U).asSInt)).asSInt
//             // }
//         vec_sew16_result
//     }

//     def arith_8(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
//         dontTouch(mask_vs0)
//         val vsetvli_mask = 0.B
//         val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
//         val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
//         val vec_sew8_result = WireInit(0.S(8.W))
//         dontTouch(vec_sew8_result)
//         // when(io.alu_opcode==="b010000".U || io.alu_opcode==="b010010".U){
//         //     vec_sew8_result := (Arithmatic(vs1.asSInt, vs2.asSInt,vs3,8,mask_vs0.asUInt)).asSInt
//         //     }.otherwise{
//         vec_sew8_result := Mux(mask_bit_active_element===1.B,Arithmatic(vs1.asSInt, vs2.asSInt,vs3,8,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(16,1.U).asSInt)).asSInt
//             // }
//         vec_sew8_result 
//     }



//   // call main function
//   //comparision adn arithmatic
//     // var count_mask = 0.U
//     // val comp_bit = "b011000".U === io.alu_opcode || "b011001".U === io.alu_opcode || "b011010".U === io.alu_opcode || "b011011".U === io.alu_opcode || "b011100".U === io.alu_opcode || "b011101".U === io.alu_opcode || "b011110".U === io.alu_opcode || "b011111".U === io.alu_opcode
//     //code changes
//     when(io.sew==="b000".U){
//     //   when(comp_bit===0.B){
//         var vl_counter = 0
//         for (i <- 0 until 8) {
//           for (j <- 0 until config.count_lanes) {
//             val idx = (i * config.count_lanes) + j
//             io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+3.U,                                                        
//             arith_8(io.vs1_in(i)(j)(31,24).asSInt, io.vs2_in(i)(j)(31,24).asSInt, io.vs3_in(i)(j)(31,24).asSInt, vs0_mask(vl_counter+3)), //ye vl_counter vstart + body elements ka abata rha 
//             Mux(tail === 0.B, io.vs3_in(i)(j)(31,24).asSInt, Fill(8, 1.U).asSInt)),

//             Mux(io.vl_in > vl_counter.U +2.U,
//             arith_8(io.vs1_in(i)(j)(23,16).asSInt, io.vs2_in(i)(j)(23,16).asSInt, io.vs3_in(i)(j)(23,16).asSInt, vs0_mask(vl_counter+2)),
//             Mux(tail === 0.B, io.vs3_in(i)(j)(23,16).asSInt, Fill(8, 1.U).asSInt)),
            
//             Mux(io.vl_in > vl_counter.U+1.U,
//             arith_8(io.vs1_in(i)(j)(15,8).asSInt, io.vs2_in(i)(j)(15,8).asSInt, io.vs3_in(i)(j)(15,8).asSInt, vs0_mask(vl_counter+1)),
//             Mux(tail === 0.B, io.vs3_in(i)(j)(15,8).asSInt, Fill(8, 1.U).asSInt)),

//             Mux(io.vl_in > vl_counter.U,
//             arith_8(io.vs1_in(i)(j)(7,0).asSInt, io.vs2_in(i)(j)(7,0).asSInt, io.vs3_in(i)(j)(7,0).asSInt, vs0_mask(vl_counter)),
//             Mux(tail === 0.B, io.vs3_in(i)(j)(7,0).asSInt, Fill(8, 1.U).asSInt))
//             ).asSInt
//             vl_counter = vl_counter + 4
//           }
//         }
//     //   }.otherwise{
//     //     var vl_counter1 = 1
//     //     var counter2 = 0
//     //     for (j <- 0 until config.count_lanes) {
//     //       io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(8,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
//     //       vl_counter1    = vl_counter1 + 32
//     //       counter2 = counter2 + 1
//     //       }
//     //       for (i <- 1 until 8) {
//     //         for (j <- 0 until config.count_lanes) {
//     //           io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
//     //         }
//     //       }
//     //     }
//       }.elsewhen(io.sew==="b001".U){//sew=16
//     //   when(comp_bit===0.B){
//         val vec_sew16_b = WireInit(0.S(16.W))
//         dontTouch(vec_sew16_b)
//         val sew16_result = WireInit(0.S(config.XLEN.W))
//         var vl_counter = 0
//         for (i <- 0 until 8) {
//           for (j <- 0 until config.count_lanes) {
//             val idx = (i * config.count_lanes) + j
//             io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+1.U,
//             arith_16(io.vs1_in(i)(j)(31,16).asSInt, io.vs2_in(i)(j)(31,16).asSInt, io.vs3_in(i)(j)(31,16).asSInt, vs0_mask(vl_counter+1)),
//             Mux(tail === 0.B, io.vs3_in(i)(j)(31,16).asSInt, Fill(16, 1.U).asSInt)),
//             Mux(io.vl_in > vl_counter.U,
//             arith_16(io.vs1_in(i)(j)(15,0).asSInt, io.vs2_in(i)(j)(15,0).asSInt, io.vs3_in(i)(j)(15,0).asSInt, vs0_mask(vl_counter)),
//             Mux(tail === 0.B, io.vs3_in(i)(j)(15,0).asSInt, Fill(16, 1.U).asSInt))).asSInt
//             vl_counter = vl_counter + 2 //counter_of_2 * 2
//           }
//         }
//     //   }.otherwise{
//     //     var vl_counter1 = 1
//     //     var counter2 = 0
//     //     for (j <- 0 until config.count_lanes) {
//     //       io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(16,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
//     //       vl_counter1    = vl_counter1 + 32
//     //       counter2 = counter2 + 1
//     //       }
//     //       for (i <- 1 until 8) {
//     //         for (j <- 0 until config.count_lanes) {
//     //           io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
//     //         }
//     //       }
//     //     }
//       }.elsewhen(io.sew==="b010".U B){//sew = 32    
//         // when(comp_bit === 0.B) {
//         var vl_counter = 1
//         for (i <- 0 until 8) {
//           for (j <- 0 until config.count_lanes) {
//             val idx = (i * config.count_lanes) + j
//             val mask = vs0_mask(idx)
           
//             io.vsd_out(i)(j) := Mux(io.vl_in >= vl_counter.U,
//               MuxLookup(io.alu_opcode, arith_32(io.vs1_in(i)(j),io.vs2_in(i)(j), io.vs3_in(i)(j), mask), Seq(
//                 "b001110".U ->    0.S//Mux(io.vs1_in(i)(j) < 8.S,vslideup(io.vs2_in(i)(j), slide_value,mask,io.vs3_in(i)(j),"b001110".U),io.vs3_in(i)(j)),
//                 // "b001111".U -> vslidedown(io.vs1_in(i)(j), vl_counter.U)
//                 // "b001100".U -> vrgather(io.vs1_in(i)(j), io.vs2_in(i)(j))
//               )),
//               Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
//             )     
//             vl_counter = vl_counter + 1
//           }
//         }
//     //   }.otherwise{
//     //     var vl_counter1 = 1
//     //     var counter2 = 0
//     //     for (j <- 0 until config.count_lanes) {
//     //       io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(32,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
//     //       vl_counter1    = vl_counter1 + 32
//     //       counter2 = counter2 + 1
//     //       }
//     //       for (i <- 1 until 8) {
//     //         for (j <- 0 until config.count_lanes) {
//     //           io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
//     //         }
//     //       }
//     //     }
//       }



// }
