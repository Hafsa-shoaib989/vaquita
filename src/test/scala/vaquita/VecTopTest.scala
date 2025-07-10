package vaquita
import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import vaquita.configparameter.VaquitaConfig

class VecTopTest extends AnyFreeSpec with ChiselScalatestTester {
  "vec top test" in {
    implicit val config = new VaquitaConfig (32,32,32,1,true)
    test(new VaquitaTop) { dut =>
      // dut.io.instr.poke(0.U)
      // dut.io.rs1_data.poke(5.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(0.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      
      dut.io.instr.poke("x0112f2d7".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(4.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      dut.io.instr.poke("x0222b257".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      dut.io.instr.poke("x02613457".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      dut.io.instr.poke("x02440557".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      
      dut.io.instr.poke("x4aa11657".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      dut.io.instr.poke("x4a411757".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      // dut.io.instr.poke("xffffffff".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)

      // dut.io.instr.poke("xffffffff".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)


      dut.io.instr.poke("x02c71857".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)

      dut.io.instr.poke("x03055957".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(50)
    }
  }
}