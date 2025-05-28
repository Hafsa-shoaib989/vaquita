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
    val vfadd              = 0.U(6.W)
    val vfsub              = 2.U(6.W)
    val vfrsub             = 39.U(6.W)
    val vfmul              = 36.U(6.W)
    val vfdiv              = 32.U(6.W)
    val vfrdiv             = 33.U(6.W)
    val vfmin              = 4.U(6.W)
    val vfmax              = 6.U(6.W)
    val vmfeq              = 24.U(6.W)
    val vmfne              = 58.U(6.W)
    val vmflt              = 27.U(6.W)
    val vmfle              = 25.U(6.W)
    val vmfgt              = 29.U(6.W)
    val vmfge              = 31.U(6.W)
    val vfmv               = 23.U(6.W)
    val vfmacc             = 44.U(6.W)
    val vfnmacc            = 45.U(6.W)
    val vfmsac             = 46.U(6.W)
    val vfnmsac            = 47.U(6.W)
    val vfmadd             = 40.U(6.W) 
    val vfnmadd            = 41.U(6.W)
    val vfmsub             = 42.U(6.W)
    val vfnmsub            = 43.U(6.W)
    // VFUNARY1             
    val vfsqrt             = Cat("b010011".U(6.W), "b00000".U(5.W))       //vfsqrt.v    //concatenation func6 + vs1
    val vfclass            = Cat("b010011".U(6.W), "b10000".U(5.W))
    val vfsgnj             = 8.U(6.W)
    val vfsgnjn            = 9.U(6.W)
    val vfsgnjx            = 10.U(6.W)
    val vfmerge            = 23.U(6.W)
}