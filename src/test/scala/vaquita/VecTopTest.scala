package vaquita
import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import vaquita.configparameter.VaquitaConfig

class VecTopTest extends AnyFreeSpec with ChiselScalatestTester {
  "vec top test" in {
    // implicit val config = new VaquitaConfig (32,32,32,1,true)
    // // FOR INTEGRATING WITH NRV ..
    implicit val config = new VaquitaConfig (256,32,32,8,true)

    test(new VaquitaTop) { dut =>
      // my normal test cases 
      dut.io.instr.poke("x0102f2d7".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(1.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 1 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x0200b057".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 2 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x022db257".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 2 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x026f3457".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 3 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x02440557".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(0.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 4 completed, instr: ${dut.io.instr.peek()}")
      
      dut.io.instr.poke("x4aa11657".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 5 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x4aa19757".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 6 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x4a411857".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 7 completed, instr: ${dut.io.instr.peek()}")



      // compare
      dut.io.instr.poke("x62c71957".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 11 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x73071a57".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 12 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x6ec81b57".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 13 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x66eb1c57".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 14 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x63855d57".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 15 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x73a55e57".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 16 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x6fc55f57".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 17 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x67e55157".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 18 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x76255257".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 19 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x7e455357".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 20 completed, instr: ${dut.io.instr.peek()}")

      dut.clock.step(50)
      println(s"Cycle 21 completed, instr: ${dut.io.instr.peek()}")
    }
  }
}