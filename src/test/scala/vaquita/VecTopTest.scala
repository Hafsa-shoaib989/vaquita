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
      dut.io.instr.poke("x0112f2d7".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(4.U)
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

     //min / max
      dut.io.instr.poke("x12e81957".U)
      dut.io.rs1_data.poke(0.S)
      dut.io.hazard_rs1_data_in.poke(0.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 11 completed, instr: ${dut.io.instr.peek()}")

      dut.io.instr.poke("x13255a57".U)
      dut.io.rs1_data.poke(0x40400000.S)
      dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // dut.io.vl_rs1_out.expect(5.U)    
      dut.io.dmemReq.ready.poke(true.B)
      dut.io.dmemRsp.valid.poke(true.B)
      dut.clock.step(1)
      println(s"Cycle 12 completed, instr: ${dut.io.instr.peek()}")






      // // divv
      // dut.io.instr.poke("x82e81957".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(32)
      // println(s"Cycle 11 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x83255a57".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(32)
      // println(s"Cycle 12 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x87455b57".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 13 completed, instr: ${dut.io.instr.peek()}")







      // dut.io.instr.poke("x03055957".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 8 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x03255a57".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 9 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x03491b57".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 10 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x022414d7".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 13 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x02955557".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 14 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x029515d7".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 15 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("xffffffff".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 8 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("xffffffff".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 9 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("xffffffff".U)
      // dut.io.rs1_data.poke(0.S)
      // dut.io.hazard_rs1_data_in.poke(0.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 10 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x03055957".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 11 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x03055957".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 12 completed, instr: ${dut.io.instr.peek()}")

      // dut.io.instr.poke("x03255a57".U)
      // dut.io.rs1_data.poke(0x40400000.S)
      // dut.io.hazard_rs1_data_in.poke(0x40400000.U)
      // // dut.io.vl_rs1_out.expect(5.U)    
      // dut.io.dmemReq.ready.poke(true.B)
      // dut.io.dmemRsp.valid.poke(true.B)
      // dut.clock.step(1)
      // println(s"Cycle 13 completed, instr: ${dut.io.instr.peek()}")

      dut.clock.step(10)
      println(s"Cycle 13 completed, instr: ${dut.io.instr.peek()}")
    }
  }
}