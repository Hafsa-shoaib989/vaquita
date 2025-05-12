package vaquita.components.VecFpu
import chisel3._

case class VecFPParameters (
  val bias          : Int = 127,
  val expWidth      : Int = 8,
  val sigWidth      : Int = 23,
  val signWidth     : Int = 1
)

