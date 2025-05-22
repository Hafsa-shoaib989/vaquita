package vaquita.components.VecFpu
import chisel3._

case class VecFPParameters (
  val bias          : Int = 127,
  val expWidth      : Int = 8,
  val sigWidth      : Int = 24,
  val signWidth     : Int = 1,
  val canon_nan     : UInt = "h7FC00000".U
)

