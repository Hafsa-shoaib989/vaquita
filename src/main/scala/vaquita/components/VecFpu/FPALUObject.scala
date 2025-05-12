package vaquita.components.VecFpu
import chisel3._
import chisel3.util._


object FPALUObj{
    // VFUNARY0             
    val vfcvt_f_xu_v       = Cat("b010010".U(6.W), "b00010".U(5.W))       //vfcvt.f.xu.v    //concatenation func6 + vs1
    val vfcvt_f_x_v        = Cat("b010010".U(6.W), "b00011".U(5.W))       //vfcvt.f.x.v
    val vfcvt_xu_f_v       = Cat("b010010".U(6.W), "b00000".U(5.W))       //vfcvt.xu.f.v
    val vfcvt_x_f_v        = Cat("b010010".U(6.W), "b00001".U(5.W))       //vfcvt.x.f.v
    val vfcvt_rtz_xu_f_v   = Cat("b010010".U(6.W), "b00110".U(5.W))       //vfcvt.rtz.xu.f.v
    val vfcvt_rtz_x_f_v    = Cat("b010010".U(6.W), "b00111".U(5.W))       //vfcvt.rtz.x.f.v

}

