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
        val mask_arith   = Input(Bool())      // want to apply masking or not?
        val vsd_out      = Output(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
        // val exceptions   = Output(UInt(5.W))
    }) 

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
val add = Module(new AddRecFN(FPConfig.expWidth, FPConfig.sigWidth))
val mul = Module(new MulRecFN(FPConfig.expWidth, FPConfig.sigWidth))
val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
val fma = Module(new MulAddRecFN(FPConfig.expWidth, FPConfig.sigWidth))

val roundingMode = 0.U
val detectTininess = consts.tininess_afterRounding

add.io.a := 0.U; add.io.b := 0.U; add.io.subOp := false.B  //add
add.io.roundingMode := roundingMode
add.io.detectTininess := detectTininess
mul.io.a := 0.U; mul.io.b := 0.U    //mul
mul.io.roundingMode := roundingMode
mul.io.detectTininess := detectTininess
div.io.a := 0.U; div.io.b := 0.U    //div
div.io.sqrtOp := false.B
div.io.inValid := false.B
div.io.roundingMode := roundingMode
div.io.detectTininess := detectTininess
cmp.io.a := 0.U; cmp.io.b := 0.U    //cmp
cmp.io.signaling := false.B
fma.io.a := 0.U; fma.io.b := 0.U; fma.io.c := 0.U   //fma
fma.io.op := 0.U
fma.io.roundingMode := roundingMode
fma.io.detectTininess := detectTininess

def applyArithmeticOp(vs1_in: SInt, vs2_in: SInt, opType: UInt, vsd: SInt): SInt = {
    val recOut = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))

    val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt)
    val recB = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
    val recC = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vsd.asUInt)

    opType match {
        case `vfadd` | `vfsub` | `vfrsub` =>
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

        case `vfmul` =>
            mul.io.a := recA
            mul.io.b := recB
            mul.io.roundingMode := roundingMode
            mul.io.detectTininess := detectTininess
            recOut := mul.io.out.asSInt
            exception_reg := mul.io.exceptionFlags

        case `vfdiv` | `vfrdiv` | `vfsqrt` =>
            div.io.b := recB
            when (opType === vfdiv || opType === vfrdiv) {  
                div.io.a := recA
                div.io.sqrtOp := false.B
            } .otherwise {
                div.io.a := 0.U
                div.io.sqrtOp := true.B
            }
            div.io.inValid := true.B
            val internalReady = WireDefault(true.B)
            internalReady := div.io.inReady 
            div.io.roundingMode := roundingMode
            div.io.detectTininess := detectTininess
            when(div.io.outValid_div || div.io.outValid_sqrt) {
                recOut := div.io.out.asSInt
                exception_reg := div.io.exceptionFlags
            }

        case `vfmin` | `vfmax` =>
            val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
            val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recB)
            
            cmp.io.a := recA
            cmp.io.b := recB
            cmp.io.signaling := true.B 

            val bothNaN = rawA.isNaN && rawB.isNaN
            val oneNaN  = rawA.isNaN ^ rawB.isNaN
            val isMin = (opType === vfmin)

            recOut := MuxCase(recB.asSInt, Seq(
                    bothNaN -> FPConfig.canon_nan.asSInt,
                    oneNaN  -> Mux(rawA.isNaN, recA.asSInt, recB.asSInt),
                    true.B  -> Mux(isMin,
                                    Mux(cmp.io.lt || cmp.io.eq, recB.asSInt, recA.asSInt), //min
                                    Mux(cmp.io.gt || cmp.io.eq, recB.asSInt, recA.asSInt))  //max
            ))

            exception_reg := cmp.io.exceptionFlags

        case `vfmacc` | `vfnmacc` | `vfmsac` | `vfnmsac` | `vfmadd` | `vfnmadd` | `vfmsub` | `vfnmsub` =>
            val op = WireDefault("b00".U(2.W))

            when (opType === vfmacc || opType === vfmadd) {
                op := "b00".U
            } .elsewhen (opType === vfmsac || opType === vfmsub) {
                op := "b01".U
            } .elsewhen (opType === vfnmsac || opType === vfnmsub) {
                op := "b10".U
            } .elsewhen (opType === vfnmacc || opType === vfnmadd) {
                op := "b11".U
            }

            val recA_ma = WireDefault(recA)
            val recB_ma = WireDefault(recA)
            val recC_ma = WireDefault(recA)

            when (opType === vfmadd || opType === vfmsub || opType === vfnmadd || opType === vfnmsub) {
                recA_ma := recA  // vs1
                recB_ma := recC  // vd
                recC_ma := recB  // vs2
            } .otherwise {
                recA_ma := recA  // vs1
                recB_ma := recB  // vs2
                recC_ma := recC  // vd
            }

            fma.io.op := op
            fma.io.a := recA_ma
            fma.io.b := recB_ma
            fma.io.c := recC_ma
            fma.io.roundingMode := roundingMode
            fma.io.detectTininess := detectTininess
            recOut := fma.io.out.asSInt
            exception_reg := fma.io.exceptionFlags

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

def applyArithmeticUnaryOp(vs2_in: SInt, opType: UInt): SInt = {
    val recOut = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))

    val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
    opType match {
        case `vfsqrt` =>
            div.io.a := recA
            div.io.b := 0.U
            div.io.sqrtOp := true.B
            div.io.inValid := true.B
            val internalReady = WireDefault(true.B)
            internalReady := div.io.inReady 
            div.io.roundingMode := roundingMode
            div.io.detectTininess := detectTininess
            when(div.io.outValid_div || div.io.outValid_sqrt) {
                recOut := div.io.out.asSInt
                exception_reg := div.io.exceptionFlags
            }

        case `vfclass` =>
            val classify = classifyRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
            recOut := Cat(0.U((config.XLEN - 10).W), classify.asUInt).asSInt

        case _ =>
        recOut := 0.S
    }
    fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recOut).asSInt
}

def ArithmeticUnary(vs2_in: SInt): SInt = {
    MuxLookup(io.alu_ctrl_con, vs2_in, Seq(
        vfsqrt -> applyArithmeticUnaryOp(vs2_in, vfsqrt),
        vfclass -> applyArithmeticUnaryOp(vs2_in, vfclass)
    ))
}


//COMPARISION INSTRUCTIONS
def comp_elem_fn(sew:Int,counter:UInt):SInt={
    val cat_element      = WireInit(0.S(32.W))
    val comp_fn_value    = comp_func(sew).asSInt
    val comp_shift       = 0
    val output_comp_Data = VecInit(Seq.tabulate(config.count_lanes)(i => comp_fn_value(32 * (i + 1) - 1, 32 * i)))
    val comp_1bt_cn      = WireInit(VecInit(Seq.fill(32)(0.U(32.W))))
    for (i <- 1 to 31) {
    comp_1bt_cn(i) := ((io.vl_in) - (32.U * counter))  //subtract counter from vl
    when(comp_1bt_cn(i) === i.U) {
        cat_element  := Cat(io.vs3_in(0)(counter)(31,i), output_comp_Data(counter)(i-1, 0)).asSInt  //tailing logic 
    }.elsewhen(comp_1bt_cn(i)===32.U || (comp_1bt_cn(i)/32.U)>0.U){   // masking for body elements
        cat_element := output_comp_Data(counter).asSInt 
    }
    }
    cat_element
}

def applyComparisonOp(vs2_in: SInt, vs1_in: SInt, opType: UInt): Bool = {
    val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
    cmp.io.a := vs2_in.asUInt
    cmp.io.b := vs1_in.asUInt

    // Signaling: only vmfeq and vmfne raise invalid exception only on signaling NaN...not on quiet NaN's
    // Others (vmflt, vmfle, etc) raise exception on both signaling & quiet NaNs.
    when (opType === vmfeq || opType === vmfne) {
        cmp.io.signaling := false.B
    }.elsewhen (opType === vmflt || opType === vmfle || opType === vmfgt || opType === vmfge) {
        cmp.io.signaling := true.B
    }.otherwise {
        cmp.io.signaling := false.B
    } 

    val result = WireDefault(false.B)
    val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt) 
    val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt) 
    val anyNaN = rawA.isNaN || rawB.isNaN

    switch(opType) {
        is(vmfeq) { result := Mux(anyNaN, false.B, cmp.io.eq) }
        is(vmfne) { result := !(cmp.io.eq) || anyNaN }
        is(vmflt) { result := Mux(anyNaN, false.B, cmp.io.lt) }
        is(vmfle) { result := Mux(anyNaN, false.B, !(cmp.io.gt)) }
        is(vmfgt) { result := Mux(anyNaN, false.B, cmp.io.gt) }
        is(vmfge) { result := Mux(anyNaN, false.B, !(cmp.io.lt)) }
    }

    exception_reg := cmp.io.exceptionFlags
    result
}

def Comparison(vs2_in: SInt, vs1_in: SInt): Bool = {
    MuxLookup(io.alu_ctrl, false.B, Seq(
        vmfeq -> applyComparisonOp(vs2_in, vs1_in, vmfeq),
        vmfne -> applyComparisonOp(vs2_in, vs1_in, vmfne),
        vmflt -> applyComparisonOp(vs2_in, vs1_in, vmflt),
        vmfle -> applyComparisonOp(vs2_in, vs1_in, vmfle),
        vmfgt -> applyComparisonOp(vs2_in, vs1_in, vmfgt),
        vmfge -> applyComparisonOp(vs2_in, vs1_in, vmfge)
    ))
}

def comp_func(sew: Int): UInt = {
    val elementsPerLane = config.vlen / sew
    val comparison_vec_bit_wires = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
    val comp_1b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
    val comp_0b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
    val comp_vs3 = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
    val vs3_bit = io.vs3_in.asUInt
    var counter = 0
    for (i <- 0 until config.count_lanes) {
    for (elem_idx <- 0 until elementsPerLane) {   //for sew/masking
        val startBit = elem_idx * sew   //tells elements bits a/cc to sew 
        val endBit = (elem_idx + 1) * sew - 1    //define that (e.g: sew=8, it define (15:8)(7:0))
        if (endBit < io.vs1_in(i).getWidth && endBit < io.vs2_in(i).getWidth) {
        val vs1_elem = io.vs1_in(i).asUInt()(endBit, startBit)
        val vs2_elem = io.vs2_in(i).asUInt()(endBit, startBit)   //in this finally extract these bits: (15:8)(7:0)
        val rec_vs1 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_elem.asUInt)
        val rec_vs2 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_elem.asUInt)
        val comparison = Comparison(rec_vs1.asSInt, rec_vs2.asSInt)
        comp_1b(counter) := (io.mask_arith && comparison) || (!io.mask_arith && comparison && vs0_mask(counter))
        comp_vs3(counter) := (!vs0_mask(counter) && !io.mask_arith)
        comp_0b(counter) := (io.mask_arith && !comparison)
        comparison_vec_bit_wires(counter) := MuxCase(0.B, Array(
            (comp_0b(counter) === 1.B) -> 0.B,
            (comp_vs3(counter) === 1.B) -> vs3_bit(counter),
            (comp_1b(counter) === 1.B) -> 1.B
        ))
        counter += 1
        }
    }
    }
    comparison_vec_bit_wires.asUInt
}


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


//MOVE & MERGE INSTRUCTIONS
def vfmerge_vfm_or_vfmv_vf(is_vfmv: Bool, vs1: SInt, vs2: SInt, mask_vs0: Bool): SInt = {
    Mux(is_vfmv, vs1, Mux(mask_vs0, vs1, vs2))      // vfmv.v.f: always scalar; vfmerge.vfm: mask decides
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

    when(isVfmergeOrVfmv && (isVfmv || isVfmerge)) {
        vec_sew32_b := vfmerge_vfm_or_vfmv_vf(isVfmv, vs1, vs2, mask_vs0)
    }.otherwise {     
        val computed_result = Mux(isConversionOp,
                            Conversion(vs2.asSInt),
                            Mux(isUnaryArithmeticOp,
                                ArithmeticUnary(vs2.asSInt),
                                Mux(isSignInject,
                                signInject(vs1.asSInt, vs2.asSInt),
                                Arithmetic(vs1.asSInt, vs2.asSInt, vs3.asSInt)
                            )
                    )
                ) 
        vec_sew32_b := Mux(mask_bit_active_element===1.B,computed_result,Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
    }

    vec_sew32_result := vec_sew32_b
    vec_sew32_result
}



// calling main function 
val vl= 4
val tail = 0.B
val comp_bit = io.alu_ctrl === vmfeq || io.alu_ctrl === vmfne || io.alu_ctrl === vmflt || io.alu_ctrl === vmfle || io.alu_ctrl === vmfgt || io.alu_ctrl === vmfge
// val reduc_bit = io.alu_ctrl === vmfeq

when(io.sew==="b010".U){ // sew = 32    
    // when(comp_bit === 0.B && reduc_bit === 0.B) {
    when(comp_bit === 0.B) {
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
//     }.elsewhen (comp_bit === 0.B && reduc_bit === 1.B) {
//         var vl_counter = 1
//         val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
//         // val mask_bit_inactive = mask_vs0===0.B && io.mask_arith===0.B
//         val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
//         val acc = Wire(SInt(32.W))
//         val startVal = io.vs1_in(0)(0)
//         acc := startVal
// // io.vsd_out(0)(0) := startVal

//         for (i <- 0 until 8) {
//             for (j <- 0 until config.count_lanes) {
//             // val idx = (i * config.count_lanes) + j
//             // val mask = vs0_mask(idx)
//                 when (mask_bit_active_element) {
//                     io.vsd_out(0)(0) := Mux(io.vl_in >= vl_counter.U,                          
//                                             reduction(cp_vs1, io.vs2_in(i)(j)), Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
//                                             )    
//                 }.elsewhen ()
//             if io.vl_in === 0 || (i && j === 0) {
//             }
//             if io.mask_arith===0.B || io.vs0_in[i]===1.U {
                
//             }
//             vl_counter = vl_counter + 1
//         }
//     }
    }.otherwise{
        var vl_counter1 = 1
        var counter2 = 0  
        for (j <- 0 until config.count_lanes) {
            io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_elem_fn(32,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
            vl_counter1    = vl_counter1 + 32
            counter2 = counter2 + 1  //increment until it reaches vl, when reaches: then tailing applied
            }
            for (i <- 1 until 8) {
            for (j <- 0 until config.count_lanes) {
                io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            }
            }
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
