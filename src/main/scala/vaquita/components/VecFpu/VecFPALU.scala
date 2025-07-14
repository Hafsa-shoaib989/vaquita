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
        val vl_in        = Input(UInt(32.W))  // on how much elements i want to work ..body elements 
        val alu_ctrl     = Input(UInt(6.W))   // for arithmethic 
        val alu_ctrl_con = Input(UInt(11.W))  // for conversion 
        val alu_ctrl_scalarM = Input(UInt(11.W))  // for scalar move 
        val mask_arith   = Input(Bool())      // want to apply masking or not?
        val vsd_out      = Output(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        // val exceptions   = Output(UInt(5.W))
    }) 

// dontTouch(io.vs1_in)
// dontTouch(io.vs2_in)

val vs0_mask = io.vs0_in.asUInt()(config.vlen,0)  // convert into one array (string), for making masking easy.

val exception_reg = RegInit(0.U(5.W))


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
val roundingMode = 0.U
val detectTininess = consts.tininess_afterRounding

def applyArithmeticOp(vs1_in: SInt, vs2_in: SInt, opType: UInt, vsd: SInt): SInt = {
    val recOut = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))
    val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt)
    val recB = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
    val recC = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vsd.asUInt)

    // ************  for testing ..will remove once passed 
    val preservedOpType = WireDefault(opType)
    dontTouch(preservedOpType)
    dontTouch(recOut)
    dontTouch(recA)
    dontTouch(recB)
    // ******************

    opType match {
        case `vfadd` | `vfsub` | `vfrsub` =>
            val add = Module(new AddRecFN(FPConfig.expWidth, FPConfig.sigWidth))
            add.io.a := recA
            add.io.b := recB
            when (opType === vfsub || opType === vfrsub) {
                add.io.subOp := true.B
            } .otherwise {
                add.io.subOp := false.B
            } 
            add.io.roundingMode := roundingMode
            add.io.detectTininess := detectTininess
            recOut := add.io.out.asSInt
            exception_reg := add.io.exceptionFlags

        // case `vfmul` =>
        //     val mul = Module(new MulRecFN(FPConfig.expWidth, FPConfig.sigWidth))
        //     mul.io.a := recA
        //     mul.io.b := recB
        //     mul.io.roundingMode := roundingMode
        //     mul.io.detectTininess := detectTininess
        //     recOut := mul.io.out.asSInt
        //     exception_reg := mul.io.exceptionFlags

        // case `vfdiv` | `vfrdiv` =>
        //     val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
        //     div.io.a := recA
        //     div.io.b := recB
        //     div.io.sqrtOp := false.B
        //     div.io.inValid := true.B
        //     val internalReady = WireDefault(true.B)
        //     internalReady := div.io.inReady 
        //     div.io.roundingMode := roundingMode
        //     div.io.detectTininess := detectTininess
        //     when(div.io.outValid_div || div.io.outValid_sqrt) {
        //         recOut := div.io.out.asSInt
        //         exception_reg := div.io.exceptionFlags
        //     }

        // case `vfmin` | `vfmax` =>
        //     val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
        //     val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
        //     val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recB)
            
        //     cmp.io.a := recA
        //     cmp.io.b := recB
        //     cmp.io.signaling := true.B 

        //     val bothNaN = rawA.i/ case `vfmul` =>
        //     val mul = Module(new MulRecFN(FPConfig.expWidth, FPConfig.sigWidth))
        //     mul.io.a := recA
        //     mul.io.b := recB
        //     mul.io.roundingMode := roundingMode
        //     mul.io.detectTininess := detectTininess
        //     recOut := mul.io.out.asSInt
        //     exception_reg := mul.io.exceptionFlags

        // case `vfdiv` | `vfrdiv` =>
        //     val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
        //     div.io.a := recA
        //     div.io.b := recB
        //     div.io.sqrtOp := false.B
        //     div.io.inValid := true.B
        //     val internalReady = WireDefault(true.B)
        //     internalReady := div.io.inReady 
        //     div.io.roundingMode := roundingMode
        //     div.io.detectTininess := detectTininess
        //     when(div.io.outValid_div || div.io.outValid_sqrt) {
        //         recOut := div.io.out.asSInt
        //         exception_reg := div.io.exceptionFlags
        //     }sNaN && rawB.isNaN
        //     val oneNaN  = rawA.isNaN ^ rawB.isNaN
        //     val isMin = (opType === vfmin)

        //     recOut := MuxCase(recB.asSInt, Seq(
        //             bothNaN -> FPConfig.canon_nan.asSInt,
        //             oneNaN  -> Mux(rawA.isNaN, recA.asSInt, recB.asSInt),
        //             true.B  -> Mux(isMin,
        //                             Mux(cmp.io.lt || cmp.io.eq, recB.asSInt, recA.asSInt), //min
        //                             Mux(cmp.io.gt || cmp.io.eq, recB.asSInt, recA.asSInt))  //max
        //     ))

        //     exception_reg := cmp.io.exceptionFlags

        // case `vfmacc` | `vfnmacc` | `vfmsac` | `vfnmsac` | `vfmadd` | `vfnmadd` | `vfmsub` | `vfnmsub` =>
        //     val fma = Module(new MulAddRecFN(FPConfig.expWidth, FPConfig.sigWidth))
        //     val op = WireDefault("b00".U(2.W))

        //     when (opType === vfmacc || opType === vfmadd) {
        //         op := "b00".U
        //     } .elsewhen (opType === vfmsac || opType === vfmsub) {
        //         op := "b01".U
        //     } .elsewhen (opType === vfnmsac || opType === vfnmsub) {
        //         op := "b10".U
        //     } .elsewhen (opType === vfnmacc || opType === vfnmadd) {
        //         op := "b11".U
        //     }

        //     val recA_ma = WireDefault(recA)
        //     val recB_ma = WireDefault(recA)
        //     val recC_ma = WireDefault(recA)

        //     when (opType === vfmadd || opType === vfmsub || opType === vfnmadd || opType === vfnmsub) {
        //         recA_ma := recA  // vs1
        //         recB_ma := recC  // vd
        //         recC_ma := recB  // vs2
        //     } .otherwise {
        //         recA_ma := recA  // vs1
        //         recB_ma := recB  // vs2
        //         recC_ma := recC  // vd
        //     }

        //     fma.io.op := op
        //     fma.io.a := recA_ma
        //     fma.io.b := recB_ma
        //     fma.io.c := recC_ma
        //     fma.io.roundingMode := roundingMode
        //     fma.io.detectTininess := detectTininess
        //     recOut := fma.io.out.asSInt
        //     exception_reg := fma.io.exceptionFlags

        case _ =>
        recOut := 0.S
    }
    fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recOut).asSInt
}

def Arithmetic(vs1_in: SInt, vs2_in: SInt, vsd: SInt): SInt = {
    MuxLookup(io.alu_ctrl, vs2_in, Seq(
        vfadd           -> applyArithmeticOp(vs2_in, vs1_in, vfadd, vsd),
        vfsub           -> applyArithmeticOp(vs2_in, vs1_in, vfsub, vsd),
        vfrsub          -> applyArithmeticOp(vs1_in, vs2_in, vfrsub, vsd),
        vfmul           -> applyArithmeticOp(vs2_in, vs1_in, vfmul, vsd),  
        vfdiv           -> applyArithmeticOp(vs2_in, vs1_in, vfdiv, vsd),
        vfrdiv          -> applyArithmeticOp(vs1_in, vs2_in, vfrdiv, vsd),
        vfmin           -> applyArithmeticOp(vs2_in, vs1_in, vfmin, vsd),
        vfmax           -> applyArithmeticOp(vs2_in, vs1_in, vfmax, vsd),
        vfmacc          -> applyArithmeticOp(vs1_in, vs2_in, vfmacc, vsd),
        vfnmacc         -> applyArithmeticOp(vs1_in, vs2_in, vfnmacc, vsd),
        vfmsac          -> applyArithmeticOp(vs1_in, vs2_in, vfmsac, vsd),
        vfnmsac         -> applyArithmeticOp(vs1_in, vs2_in, vfnmsac, vsd),
        vfmadd          -> applyArithmeticOp(vs1_in, vs2_in, vfmadd, vsd),
        vfnmadd         -> applyArithmeticOp(vs1_in, vs2_in, vfnmadd, vsd),
        vfmsub          -> applyArithmeticOp(vs1_in, vs2_in, vfmsub, vsd),
        vfnmsub         -> applyArithmeticOp(vs1_in, vs2_in, vfnmsub, vsd)
    ))
}

// for sew's
def sew_arit_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew32_b = WireInit(0.S(32.W))
    val vec_sew32_result = WireInit(0.S(config.XLEN.W))

    // Define known operations
    val isConversionOp = io.alu_ctrl_con === vfcvt_f_xu_v || io.alu_ctrl_con === vfcvt_f_x_v || io.alu_ctrl_con === vfcvt_xu_f_v || io.alu_ctrl_con === vfcvt_x_f_v || io.alu_ctrl_con === vfcvt_rtz_xu_f_v || io.alu_ctrl_con === vfcvt_rtz_x_f_v
    val isUnaryArithmeticOp = io.alu_ctrl_con === vfsqrt || io.alu_ctrl_con === vfclass
    val isSignInject = io.alu_ctrl === vfsgnj || io.alu_ctrl === vfsgnjn || io.alu_ctrl === vfsgnjx
    
    // Vfmerge/Vfmv instruction
    val isVfmergeOrVfmv = io.alu_ctrl === vfmv_vfmerge
    val vs2_is_v0 = vs2 === 0.S
    val isVfmv = isVfmergeOrVfmv && io.mask_arith && vs2_is_v0
    val isVfmerge = isVfmergeOrVfmv && !io.mask_arith 

     
    // ************  for testing ..will remove once passed 
     val computed_result = Mux(isConversionOp, Conversion(vs2.asSInt),Arithmetic(vs1.asSInt, vs2.asSInt, vs3.asSInt))
     vec_sew32_b := Mux(mask_bit_active_element===1.B,computed_result,Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
    // *****************

    vec_sew32_result := vec_sew32_b
    vec_sew32_result
}


// calling main function 
val vl= 4
val tail = 0.B
val comp_bit = io.alu_ctrl === vmfeq || io.alu_ctrl === vmfne || io.alu_ctrl === vmflt || io.alu_ctrl === vmfle || io.alu_ctrl === vmfgt || io.alu_ctrl === vmfge
val reduc_osum_bit = io.alu_ctrl === vfredosum
val reduc_usum_bit = io.alu_ctrl === vfredusum
val reduc_bit = reduc_osum_bit || reduc_usum_bit
val sm_f_s = io.alu_ctrl_con === vfmv_f_s 
val sm_s_f = io.alu_ctrl_scalarM === vfmv_s_f
val scalar_move_bit = sm_f_s || sm_s_f
val reduc_max_bit = io.alu_ctrl === vfredmax
val reduc_min_bit = io.alu_ctrl === vfredmin
val reduc_op_bit = io.alu_ctrl === vfredmax || io.alu_ctrl === vfredmin


when(io.sew==="b010".U){ // sew = 32   
    when(!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit) {    //Arithmetic instructions
        var vl_counter = 1
        for (i <- 0 until 8) {
            for (j <- 0 until config.count_lanes) {
            val idx = (i * config.count_lanes) + j
            val mask = vs0_mask(idx)

            io.vsd_out(i)(j) := Mux(io.vl_in >= vl_counter.U,                          
                sew_arit_32( io.vs1_in(i)(j), io.vs2_in(i)(j), io.vs3_in(i)(j), mask),     
                Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            )     
            vl_counter = vl_counter + 1
        }
    }

    }.otherwise{     //comp_bit === 1.B
        // ************  for testing ..will remove once passed 
        var vl_counter = 1
            for (i <- 0 until 8) {
            for (j <- 0 until config.count_lanes) {
                val idx = (i * config.count_lanes) + j
                val mask = vs0_mask(idx)
                io.vsd_out(i)(j) :=0.S 
                vl_counter = vl_counter + 1
            }
            }
        // ***************    
        
        }
}.otherwise{
    var vl_counter = 1
    for (i <- 0 until 8) {
    for (j <- 0 until config.count_lanes) {
        // val idx = (i * config.count_lanes) + j
        // val mask = vs0_mask(idx)
        io.vsd_out(i)(j) :=0.S 
        vl_counter = vl_counter + 1
    }
    }
}  

}
