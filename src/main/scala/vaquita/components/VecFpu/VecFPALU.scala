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
        val vs3_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))       // mask undisturbed ..and tail undisturbed 
        val vs0_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))       // for maksing which element 
        val sew          = Input(UInt(3.W))
        val vl_in        = Input(UInt(32.W))         // on how much elements i want to work (body elements) 
        val rs1_in       = Input(UInt(32.W))
        val alu_ctrl     = Input(UInt(6.W))          // for arithmethic instructions
        val alu_ctrl_con = Input(UInt(11.W))         // for conversion/unary instructions 
        val alu_ctrl_scalarM = Input(UInt(11.W))     // for scalar move 
        val mask_arith   = Input(Bool())             // want to apply masking or not?
        val vsd_out      = Output(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        val valid_dive   = Output(Bool())
        // val exceptions   = Output(UInt(5.W))
    }) 

val vs0_mask = io.vs0_in.asUInt()(config.vlen,0)      // convert into one array (string), for making masking easy.
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

        // case `vfmin` | `vfmax` =>
        //     val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
        //     val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
        //     val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recB)
            
        //     cmp.io.a := recA
        //     cmp.io.b := recB
        //     cmp.io.signaling := true.B 

        //     val bothNaN = rawA.isNaN && rawB.isNaN
        //     val oneNaN  = rawA.isNaN ^ rawB.isNaN
        //     val bothZero = rawA.isZero && rawB.isZero
        //     val aNegZero = rawA.isZero && rawA.sign       // signedZero logic
        //     val bNegZero = rawB.isZero && rawB.sign

        //     val isMin = (opType === vfmin)
        //     val minCondition = cmp.io.lt || (bothZero && aNegZero && isMin) || (bothZero && !aNegZero && !isMin)
        //     val maxCondition = cmp.io.gt || (bothZero && !aNegZero && !isMin) || (bothZero && aNegZero && isMin)

        //     recOut := MuxCase(recB.asSInt, Seq(
        //         bothNaN -> FPConfig.canon_nan.asSInt,
        //         oneNaN  -> Mux(rawA.isNaN, recB.asSInt, recA.asSInt),
        //         isMin   -> Mux(minCondition, recA.asSInt, recB.asSInt),
        //         !isMin  -> Mux(maxCondition, recA.asSInt, recB.asSInt)
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


def Arithmetic(vs1_in: SInt, vs2_in: SInt, vsd: SInt, mask_vs0: Bool): SInt = {
    val result = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))
        result := 
        MuxLookup(io.alu_ctrl, vs2_in, Seq(
            vfadd           -> applyArithmeticOp(vs2_in, vs1_in, vfadd, vsd),
            vfsub           -> applyArithmeticOp(vs2_in, vs1_in, vfsub, vsd),
            vfrsub          -> applyArithmeticOp(vs1_in, vs2_in, vfrsub, vsd),
            vfmul           -> applyArithmeticOp(vs2_in, vs1_in, vfmul, vsd),  
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
    result
}


// // DIVISION / SQRT INSTRUCTIONS
// def Division(vs1_in: SInt, vs2_in: SInt, mask_vs0: Bool, counter: UInt, isSqrt: Bool): SInt = {
//     val recOut1 = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))     
//     val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt)
//     val recB = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
//     val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
    
//     when(isSqrt) {               
//         div.io.a := recB
//         div.io.b := 0.U
//         div.io.sqrtOp := true.B
//     }.elsewhen(io.alu_ctrl === vfdiv) {       
//         div.io.a := recB
//         div.io.b := recA
//         div.io.sqrtOp := false.B
//     }.elsewhen(io.alu_ctrl === vfrdiv){
//         div.io.a := recA
//         div.io.b := recB
//         div.io.sqrtOp := false.B
//     }.otherwise {
//         div.io.a := 0.U
//         div.io.b := 0.U
//         div.io.sqrtOp := false.B
//     }

//     when((io.alu_ctrl === vfdiv || io.alu_ctrl === vfrdiv || io.alu_ctrl_con === vfsqrt) && (div.io.inReady ^ (div.io.outValid_div || div.io.outValid_sqrt)) && ((mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B)  && (counter === 0.U)) {
//         div.io.inValid := true.B
//     }.otherwise {
//         div.io.inValid := false.B
//     }

//     div.io.roundingMode := roundingMode
//     div.io.detectTininess := detectTininess
    
//     when (div.io.outValid_div || div.io.outValid_sqrt) {
//         recOut1 := div.io.out.asSInt
//     }.otherwise {
//         recOut1 := 0.S
//     }

//     exception_reg := div.io.exceptionFlags
//     val fN_div_value = fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recOut1).asSInt
//     val outValid = Mux(isSqrt, div.io.outValid_sqrt, div.io.outValid_div)
//     val conc = WireDefault(Cat(outValid, fN_div_value)).asSInt
//     conc
// }


//COMPARISON INSTRUCTIONS
// def applyComparisonOp(vs1_in: SInt, vs2_in: SInt, opType: UInt): Bool = {
//     val rec_vs1 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt)
//     val rec_vs2 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
//     val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
//     cmp.io.a := rec_vs2
//     cmp.io.b := rec_vs1

//     // Signaling: only vmfeq and vmfne raise invalid exception only on signaling NaN...not on quiet NaN's
//     // Others (vmflt, vmfle, etc) raise exception on both signaling & quiet NaNs.
//     when (opType === vmfeq || opType === vmfne) {
//         cmp.io.signaling := false.B
//     }.elsewhen (opType === vmflt || opType === vmfle || opType === vmfgt || opType === vmfge) {
//         cmp.io.signaling := true.B
//     }.otherwise {
//         cmp.io.signaling := false.B
//     } 

//     val result_comp = WireDefault(false.B)
//     val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, rec_vs2) 
//     val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, rec_vs1) 
//     val anyNaN = rawA.isNaN || rawB.isNaN

//     switch(opType) {
//         is(vmfeq) { result_comp := Mux(anyNaN, false.B, cmp.io.eq) }
//         is(vmfne) { result_comp := anyNaN || !(cmp.io.eq && !anyNaN) }
//         is(vmflt) { result_comp := Mux(anyNaN, false.B, cmp.io.lt) }
//         is(vmfle) { result_comp := Mux(anyNaN, false.B, !(cmp.io.gt)) }
//         is(vmfgt) { result_comp := Mux(anyNaN, false.B, cmp.io.gt) }
//         is(vmfge) { result_comp := Mux(anyNaN, false.B, !(cmp.io.lt)) }
//     }

//     exception_reg := cmp.io.exceptionFlags
//     result_comp
// }

// def comparison_operators(vs1_in: SInt, vs2_in: SInt): Bool = {
//     MuxLookup(io.alu_ctrl, false.B, Seq(
//         vmfeq -> applyComparisonOp(vs1_in, vs2_in, vmfeq),
//         vmfne -> applyComparisonOp(vs1_in, vs2_in, vmfne),
//         vmflt -> applyComparisonOp(vs1_in, vs2_in, vmflt),
//         vmfle -> applyComparisonOp(vs1_in, vs2_in, vmfle),
//         vmfgt -> applyComparisonOp(vs1_in, vs2_in, vmfgt),
//         vmfge -> applyComparisonOp(vs1_in, vs2_in, vmfge)
//     ))
// }
	
// def main_comp(
// 	vs1: Vec[Vec[SInt]],
// 	vs2: Vec[Vec[SInt]],
// 	vs3: Vec[Vec[SInt]],
// 	mask: UInt,
// 	rs1 : UInt,
// 	vl : UInt,
// 	mask_arith25 : Bool,
// 	sew_lanes : Int,
// 	sew : Int
// 	): Vec[Vec[SInt]] = {
// 	val result_val = WireInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(sew_lanes)(0.S(sew.W))))))
// 	val vsetvli_mask = Wire(Bool())
// 	vsetvli_mask := 0.B
// 	val tail = Wire(Bool())
// 	// val comp_bool = WireInit(VecInit(Seq.fill(config.vlen)(false.B)))
//     val comp_bool = WireInit(VecInit((0 until config.vlen).map(i => vs3(0).asUInt(i).asBool)))
// 	val comp_wire = Wire(UInt(config.vlen.W))
// 	comp_wire := comp_bool.asUInt
// 	tail := 0.B
// 	var elem_idx = 0
// 	for (i <- 0 until 8) {
// 	    for (j <- 0 until (sew_lanes)) {
		
// 		val mask_bit_active_element = (mask(elem_idx) === 1.B && mask_arith25 === 0.B) || mask_arith25 === 1.B
// 		val mask_bit_undisturb = mask(elem_idx) === 0.B && mask_arith25 === 0.B && vsetvli_mask === 0.B

//         comp_bool(elem_idx) := Mux(
// 		(vl > elem_idx.U),
// 		Mux(mask_bit_active_element,comparison_operators(vs1(i)(j).asSInt, vs2(i)(j).asSInt), Mux(mask_bit_undisturb, (vs3(0).asUInt)(elem_idx).asBool, 1.B)),
// 		Mux(tail === 0.B, (vs3(0).asUInt)(elem_idx).asBool, 1.B)
// 		)
// 		elem_idx = elem_idx +1
// 	    }
// 	}
// 	var high = sew-1
// 	var low  = 0
// 	for (j <- 0 until sew_lanes) {
// 	    result_val(0)(j) := comp_wire(high, low).asSInt
// 	    high += sew
// 	    low  += sew
// 	}
// 	for (i <- 1 until 8) {
// 	    for (j <- 0 until sew_lanes) {
// 		result_val(i)(j) := Mux(tail === 0.B, vs3(i)(j).asUInt, Fill(sew, 1.U)).asSInt //vs3(i)(j).asUInt
// 	    }
// 	}
// 	result_val
// 	}


//SIGN INJECTION INSTRUCTIONS
def signInject(vs1_in: SInt, vs2_in: SInt): SInt = {
    val sign_inject_result = WireDefault(vs2_in)
    val sign_vs1 = vs1_in.asUInt()(31)
    val sign_vs2 = vs2_in.asUInt()(31)

    val new_sign = MuxLookup(io.alu_ctrl, sign_vs1, Seq(
        vfsgnj  -> sign_vs1,
        vfsgnjn -> ~sign_vs1,
        vfsgnjx -> (sign_vs1 ^ sign_vs2)
    ))
    val magnitude = vs2_in.asUInt()(30, 0)  // remove sign bit
    val final_bits = Cat(new_sign, magnitude)
    sign_inject_result := final_bits.asSInt
    sign_inject_result
}


// CLASSIFY INSTRUCTIONS
def ArithmeticUnary(vs2_in: SInt): SInt = {
    val recOut = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))
    val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
    val classify = classifyRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
    recOut := Cat(0.U((config.XLEN - 10).W), classify.asUInt).asSInt
    recOut
}



// for sew's
def sew_arit_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool): SInt ={
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew32_b = WireInit(0.S(32.W))
    val vec_sew32_result = WireInit(0.S(config.XLEN.W))

    val isConversionOp = io.alu_ctrl_con === vfcvt_f_xu_v || io.alu_ctrl_con === vfcvt_f_x_v || io.alu_ctrl_con === vfcvt_xu_f_v || io.alu_ctrl_con === vfcvt_x_f_v || io.alu_ctrl_con === vfcvt_rtz_xu_f_v || io.alu_ctrl_con === vfcvt_rtz_x_f_v
    val isUnaryArithmeticOp = io.alu_ctrl_con === vfclass
    val isSignInject = io.alu_ctrl === vfsgnj || io.alu_ctrl === vfsgnjn || io.alu_ctrl === vfsgnjx
    
    // Vfmerge/Vfmv instruction
    val isVfmergeOrVfmv = io.alu_ctrl === vfmv_vfmerge
    val vs2_is_v0 = vs2 === 0.S
    val isVfmv = isVfmergeOrVfmv && io.mask_arith && vs2_is_v0
    val isVfmerge = isVfmergeOrVfmv && !io.mask_arith 

     
    // ************  for testing ..will remove once passed 
     val computed_result = Mux(isConversionOp, Conversion(vs2.asSInt), Mux(isUnaryArithmeticOp,
                                ArithmeticUnary(vs2.asSInt), Mux(isSignInject,
                                signInject(vs1.asSInt, vs2.asSInt), Arithmetic(vs1.asSInt, vs2.asSInt, vs3.asSInt, mask_vs0))))
    
    // val computed_result = Mux(isConversionOp, Conversion(vs2.asSInt), Arithmetic(vs1.asSInt, vs2.asSInt, vs3.asSInt, mask_vs0))

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
val div_bit = io.alu_ctrl === vfdiv || io.alu_ctrl === vfrdiv
val sqrt_bit = io.alu_ctrl_con === vfsqrt

io.valid_dive := 0.B

when(io.sew==="b010".U){ // sew = 32   
    when(!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit && !div_bit && !sqrt_bit) {            //Arithmetic instructions
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


    // //************FOR OPTIMIZATION ..REMOVING MULTIPLY
    // when(!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit && !div_bit && !sqrt_bit) {            //Arithmetic instructions
    //     var vl_counter = 0  //***************
    //     for (i <- 0 until 8) {
    //         for (j <- 0 until config.count_lanes) {
    //         // val idx = (i * config.count_lanes) + j
    //         // val mask = vs0_mask(idx)
    //         io.vsd_out(i)(j) := Mux(io.vl_in > vl_counter.U, //*********** = hataya                         
    //             sew_arit_32( io.vs1_in(i)(j), io.vs2_in(i)(j), io.vs3_in(i)(j), vs0_mask(vl_counter)), //**********  
    //             Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
    //         )     
    //         vl_counter = vl_counter + 1
    //     }
    // }

    // }.elsewhen (!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit && (div_bit || sqrt_bit)) {     //Div/Sqrt instructions
    //     var vl_counter = 0
    //     val values = RegInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(config.count_lanes)(0.S(config.XLEN.W))))))
    //     val div_result = WireInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(config.count_lanes)(0.S((config.XLEN + 1).W))))))
    //     val div_counter = RegInit(0.U(32.W))

    //     when (div_bit || sqrt_bit) {
    //         div_counter := div_counter + 1.U
    //     }.elsewhen (div_counter === 31.U) {
    //         div_counter := 0.U
    //     }.otherwise {
    //         div_counter := 0.U
    //     }

    //     for (i <- 0 until 8) {
    //         for (j <- 0 until config.count_lanes) {
    //             val idx = (i * config.count_lanes) + j
    //             val mask_fordiv = vs0_mask(idx)      
    //             div_result(i)(j) := Division(io.vs1_in(i)(j), io.vs2_in(i)(j), mask_fordiv, div_counter, sqrt_bit)    
    //             when (div_result(i)(j)(32)) {
    //                 values(i)(j) := div_result(i)(j)(31,0).asSInt
    //             }
    //         }
    //     }

    //     when (div_counter === 31.U) {
    //         div_counter := 0.U
    //         io.valid_dive := 1.B
    //         for (i <- 0 until 8) {
    //             for (j <- 0 until config.count_lanes) {
    //                 val idx = (i * config.count_lanes) + j
    //                 val mask = vs0_mask(idx)
    //                 val vsetvli_mask = 0.B
    //                 val mask_bit_active_element = (vs0_mask(idx) === 1.B && io.mask_arith === 0.B) || io.mask_arith === 1.B
    //                 val mask_bit_undisturb = vs0_mask(idx) === 0.B && io.mask_arith === 0.B && vsetvli_mask === 0.B
    //                 io.vsd_out(i)(j) := Mux(io.vl_in > vl_counter.U,
    //                                     Mux(mask_bit_active_element, values(i)(j).asSInt, Mux(mask_bit_undisturb, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)),
    //                                     Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
    //                                 )
    //                 vl_counter = vl_counter + 1
    //             }
    //         }
    //     }.otherwise {
    //         io.valid_dive := 0.B
    //         for (i <- 0 until 8) {
    //             for (j <- 0 until config.count_lanes) {
    //                 io.vsd_out(i)(j) := 11.S 
    //             }
    //         }
    //     }  


    }.otherwise{     //comp_bit === 1.B           
        // io.vsd_out := main_comp(io.vs1_in, io.vs2_in,io.vs3_in,vs0_mask,io.rs1_in,io.vl_in,io.mask_arith,config.count_lanes,32)

        //once passed.... then removed ************
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
        val idx = (i * config.count_lanes) + j
        val mask = vs0_mask(idx)
        io.vsd_out(i)(j) :=0.S 
        vl_counter = vl_counter + 1
    }
    }
}  

}
