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
        val valid_dive   = Output(Bool())
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

        case `vfmul` =>
            val mul = Module(new MulRecFN(FPConfig.expWidth, FPConfig.sigWidth))
            mul.io.a := recA
            mul.io.b := recB
            mul.io.roundingMode := roundingMode
            mul.io.detectTininess := detectTininess
            recOut := mul.io.out.asSInt
            exception_reg := mul.io.exceptionFlags

        case `vfmin` | `vfmax` =>
            val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
            val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recA)
            val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recB)
            
            cmp.io.a := recA
            cmp.io.b := recB
            cmp.io.signaling := true.B 

            val bothNaN = rawA.isNaN && rawB.isNaN
            val oneNaN  = rawA.isNaN ^ rawB.isNaN
            val bothZero = rawA.isZero && rawB.isZero
            val aNegZero = rawA.isZero && rawA.sign       // signedZero logic
            val bNegZero = rawB.isZero && rawB.sign

            val isMin = (opType === vfmin)
            val minCondition = cmp.io.lt || (bothZero && aNegZero && isMin) || (bothZero && !aNegZero && !isMin)
            val maxCondition = cmp.io.gt || (bothZero && !aNegZero && !isMin) || (bothZero && aNegZero && isMin)

            recOut := MuxCase(recB.asSInt, Seq(
                bothNaN -> FPConfig.canon_nan.asSInt,
                oneNaN  -> Mux(rawA.isNaN, recB.asSInt, recA.asSInt),
                isMin   -> Mux(minCondition, recA.asSInt, recB.asSInt),
                !isMin  -> Mux(maxCondition, recA.asSInt, recB.asSInt)
            ))
            exception_reg := cmp.io.exceptionFlags

        case `vfmacc` | `vfnmacc` | `vfmsac` | `vfnmsac` | `vfmadd` | `vfnmadd` | `vfmsub` | `vfnmsub` =>
            val fma = Module(new MulAddRecFN(FPConfig.expWidth, FPConfig.sigWidth))
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

// def Division(vs1_in: SInt, vs2_in: SInt, mask_vs0: Bool): (SInt,Bool) = {
//     val recOut1 = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))
//     val recA = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt)
//     val recB = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt)
//     val div = Module(new DivSqrtRecFN_small(FPConfig.expWidth, FPConfig.sigWidth, 0))
//     div.io.a := recA
//     div.io.b := recB
//     div.io.sqrtOp := false.B
//     when((io.alu_ctrl === vfdiv || io.alu_ctrl === vfrdiv) && (div.io.inReady ^ div.io.outValid_div) && !(mask_vs0===0.B && io.mask_arith===0.B)) {
//         div.io.inValid := true.B
//     }.otherwise {
//         div.io.inValid := false.B
//     }
//     div.io.roundingMode := roundingMode
//     div.io.detectTininess := detectTininess
//     dontTouch(div.io.inReady)
   
//     recOut1 := div.io.out.asSInt
//     exception_reg := div.io.exceptionFlags

//     (fNFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, recOut1).asSInt, div.io.outValid_div)
// }

def Arithmetic(vs1_in: SInt, vs2_in: SInt, vsd: SInt, mask_vs0: Bool): (SInt, Bool) = {
    val result = WireDefault(0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W))
    val flag = WireInit(0.B) 
    when (!(io.alu_ctrl === vfdiv || io.alu_ctrl === vfrdiv)) {
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
    flag := 0.B
    // }.elsewhen(io.alu_ctrl === vfdiv || io.alu_ctrl === vfrdiv) {
    //     val (w, v) = Division(vs2_in, vs1_in, mask_vs0)
    //     result := w
    //     flag := v
    }.otherwise {
        result := 0.S
        flag := 0.B
    }
    (result, flag)
}





// //COMPARISION INSTRUCTIONS
// def comp_elem_fn(sew:Int,counter:UInt):SInt={
//     val cat_element      = WireInit(0.S(32.W))
//     val comp_fn_value    = comp_func(sew).asSInt
//     val comp_shift       = 0
//     val output_comp_Data = VecInit(Seq.tabulate(config.count_lanes)(i => comp_fn_value(32 * (i + 1) - 1, 32 * i)))
//     val comp_1bt_cn      = WireInit(VecInit(Seq.fill(32)(0.U(32.W))))
//     for (i <- 1 to 31) {
//     comp_1bt_cn(i) := ((io.vl_in) - (32.U * counter))  //subtract counter from vl
//     when(comp_1bt_cn(i) === i.U) {
//         cat_element  := Cat(io.vs3_in(0)(counter)(31,i), output_comp_Data(counter)(i-1, 0)).asSInt  //tailing logic 
//     }.elsewhen(comp_1bt_cn(i)===32.U || (comp_1bt_cn(i)/32.U)>0.U){   // masking for body elements
//         cat_element := output_comp_Data(counter).asSInt 
//     }
//     }
//     cat_element
// }

// def applyComparisonOp(vs1_in: SInt, vs2_in: SInt, opType: UInt): Bool = {
//     val cmp = Module(new CompareRecFN(FPConfig.expWidth, FPConfig.sigWidth))
//     cmp.io.a := vs1_in.asUInt
//     cmp.io.b := vs2_in.asUInt

//     // Signaling: only vmfeq and vmfne raise invalid exception only on signaling NaN...not on quiet NaN's
//     // Others (vmflt, vmfle, etc) raise exception on both signaling & quiet NaNs.
//     when (opType === vmfeq || opType === vmfne) {
//         cmp.io.signaling := false.B
//     }.elsewhen (opType === vmflt || opType === vmfle || opType === vmfgt || opType === vmfge) {
//         cmp.io.signaling := true.B
//     }.otherwise {
//         cmp.io.signaling := false.B
//     } 

//     val result = WireDefault(false.B)
//     val rawA = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_in.asUInt) 
//     val rawB = rawFloatFromRecFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_in.asUInt) 
//     val anyNaN = rawA.isNaN || rawB.isNaN

//     switch(opType) {
//         is(vmfeq) { result := Mux(anyNaN, false.B, cmp.io.eq) }
//         is(vmfne) { result := !(cmp.io.eq) || anyNaN }
//         is(vmflt) { result := Mux(anyNaN, false.B, cmp.io.lt) }
//         is(vmfle) { result := Mux(anyNaN, false.B, !(cmp.io.gt)) }
//         is(vmfgt) { result := Mux(anyNaN, false.B, cmp.io.gt) }
//         is(vmfge) { result := Mux(anyNaN, false.B, !(cmp.io.lt)) }
//     }

//     exception_reg := cmp.io.exceptionFlags
//     result
// }

// def Comparison(vs1_in: SInt, vs2_in: SInt): Bool = {
//     MuxLookup(io.alu_ctrl, false.B, Seq(
//         vmfeq -> applyComparisonOp(vs1_in, vs2_in, vmfeq),
//         vmfne -> applyComparisonOp(vs1_in, vs2_in, vmfne),
//         vmflt -> applyComparisonOp(vs1_in, vs2_in, vmflt),
//         vmfle -> applyComparisonOp(vs1_in, vs2_in, vmfle),
//         vmfgt -> applyComparisonOp(vs1_in, vs2_in, vmfgt),
//         vmfge -> applyComparisonOp(vs1_in, vs2_in, vmfge)
//     ))
// }

// def comp_func(sew: Int): UInt = {
//     val elementsPerLane = config.vlen / sew
//     val comparison_vec_bit_wires = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
//     val comp_1b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
//     val comp_0b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
//     val comp_vs3 = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
//     val vs3_bit = io.vs3_in.asUInt
//     var counter = 0
//     for (i <- 0 until config.count_lanes) {
//     for (elem_idx <- 0 until elementsPerLane) {   //for sew/masking
//         val startBit = elem_idx * sew   //tells elements bits a/cc to sew 
//         val endBit = (elem_idx + 1) * sew - 1    //define that (e.g: sew=8, it define (15:8)(7:0))
//         if (endBit < io.vs1_in(i).getWidth && endBit < io.vs2_in(i).getWidth) {
//         val vs1_elem = io.vs1_in(i).asUInt()(endBit, startBit)
//         val vs2_elem = io.vs2_in(i).asUInt()(endBit, startBit)   //in this finally extract these bits: (15:8)(7:0)
//         val rec_vs1 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs1_elem.asUInt)
//         val rec_vs2 = recFNFromFN(FPConfig.expWidth, FPConfig.sigWidth, vs2_elem.asUInt)
//         val comparison = Comparison(rec_vs1.asSInt, rec_vs2.asSInt)
//         comp_1b(counter) := (io.mask_arith && comparison) || (!io.mask_arith && comparison && vs0_mask(counter))
//         comp_vs3(counter) := (!vs0_mask(counter) && !io.mask_arith)
//         comp_0b(counter) := (io.mask_arith && !comparison)
//         comparison_vec_bit_wires(counter) := MuxCase(0.B, Array(
//             (comp_0b(counter) === 1.B) -> 0.B,
//             (comp_vs3(counter) === 1.B) -> vs3_bit(counter),
//             (comp_1b(counter) === 1.B) -> 1.B
//         ))
//         counter += 1
//         }
//     }
//     }
//     comparison_vec_bit_wires.asUInt
// }





// for sew's
def sew_arit_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool): (SInt, Bool)={
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
     val (arithmetic_result, valid_div_out) = Arithmetic(vs1.asSInt, vs2.asSInt, vs3.asSInt, mask_vs0)
     val computed_result = Mux(isConversionOp, Conversion(vs2.asSInt),arithmetic_result)
     vec_sew32_b := Mux(mask_bit_active_element===1.B,computed_result,Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
    // *****************

    vec_sew32_result := vec_sew32_b
    (vec_sew32_result, valid_div_out)
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

val divBusy   = WireInit(VecInit(Seq.fill(8) {VecInit(Seq.fill(config.count_lanes)(false.B))}))
val divValues = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(config.count_lanes) {0.S((FPConfig.expWidth + FPConfig.sigWidth + 1).W)})}))
io.valid_dive := 0.B

when(io.sew==="b010".U){ // sew = 32   
    when(!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit && !div_bit) {    //Arithmetic instructions
        var vl_counter = 1
        for (i <- 0 until 8) {
            for (j <- 0 until config.count_lanes) {
            val idx = (i * config.count_lanes) + j
            val mask = vs0_mask(idx)
            val (a,b) = sew_arit_32( io.vs1_in(i)(j), io.vs2_in(i)(j), io.vs3_in(i)(j), mask)
            io.vsd_out(i)(j) := Mux(io.vl_in >= vl_counter.U,                          
                a,   
                Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            )     
            vl_counter = vl_counter + 1
        }
    }

    }.elsewhen (!reduc_bit && !reduc_op_bit && !comp_bit && !scalar_move_bit && div_bit) {     //Div instructions
        var vl_counter = 1
        when (io.vl_in >= vl_counter.U) {
            for (i <- 0 until 8) {
                for (j <- 0 until config.count_lanes) {
                val idx = (i * config.count_lanes) + j
                val mask = vs0_mask(idx)
                val (a, div_b) = sew_arit_32(io.vs1_in(i)(j), io.vs2_in(i)(j), io.vs3_in(i)(j), mask)
                dontTouch(div_b)
                when (div_b === 1.B || (mask===0.B && io.mask_arith===0.B)) {
                    divBusy(i)(j) := 1.B
                    divValues(i)(j) := a
                }
                // .otherwise {
                //     divBusy(i)(j) := DontCare
                //     divValues(i)(j) := DontCare
                // }   
                vl_counter = vl_counter + 1
                }
            }
            val allDivBusy = divBusy.flatten.reduce(_ && _)
            io.valid_dive := allDivBusy

            //iss mein sahi chl rhaaaaa (bus clock cycles dekhni ...sahi clk pe araah ya nhi)
            // io.vsd_out <> divValues     

            when (io.valid_dive) {
                io.vsd_out <> divValues
            }.otherwise {
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
                        
        }.otherwise{
            var vl_counter = 1
            for (i <- 0 until 8) {
            for (j <- 0 until config.count_lanes) {
                io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt) 
                vl_counter = vl_counter + 1
            }
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
         }
        // ***************    
        
        // var vl_counter1 = 1
        // var counter2 = 0  
        // for (j <- 0 until config.count_lanes) {
        //     io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_elem_fn(32,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
        //     vl_counter1    = vl_counter1 + 32
        //     counter2 = counter2 + 1  //increment until it reaches vl, when reaches: then tailing applied
        //     }
        //     for (i <- 1 until 8) {
        //     for (j <- 0 until config.count_lanes) {
        //         io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
        //     }
        //     }
        
        // }
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
