;buildInfoPackage: chisel3, version: 3.4.3, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit V_RegFile : 
  module V_RegFile : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip vs1 : UInt<5>, flip vs2 : UInt<5>, flip V_reg_write : UInt<1>, flip vd : UInt<5>, flip V_w_data : SInt<128>, vdata1 : SInt<128>, vdata2 : SInt<128>}
    
    wire _register_WIRE : SInt<128>[32] @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[0] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[1] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[2] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[3] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[4] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[5] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[6] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[7] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[8] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[9] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[10] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[11] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[12] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[13] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[14] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[15] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[16] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[17] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[18] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[19] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[20] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[21] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[22] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[23] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[24] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[25] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[26] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[27] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[28] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[29] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[30] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    _register_WIRE[31] <= asSInt(UInt<128>("h00")) @[VECTOR_REGISTER.scala 25:31]
    reg register : SInt<128>[32], clock with : (reset => (reset, _register_WIRE)) @[VECTOR_REGISTER.scala 25:23]
    io.vdata1 <= register[io.vs1] @[VECTOR_REGISTER.scala 27:11]
    io.vdata2 <= register[io.vs2] @[VECTOR_REGISTER.scala 28:11]
    when io.V_reg_write : @[VECTOR_REGISTER.scala 30:25]
      register[io.vd] <= io.V_w_data @[VECTOR_REGISTER.scala 31:24]
      skip @[VECTOR_REGISTER.scala 30:25]
    
