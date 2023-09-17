package freechips.rocketchip.rocc.dmaguard

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocc.qarma._

object DmaGuardSignInst {
  val opcode = BitPat("b?????????????????????????1101011")
}

class DmaGuardSign(opcodes: OpcodeSet)(implicit p: Parameters) 
    extends LazyRoCC(opcodes) 
    with HasCoreParameters { 
  override lazy val module = new DmaGuardSignMultiCycleImp(this)
}

class DmaGuardSignMultiCycleImp(outer: DmaGuardSign)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import chisel3.util.experimental.BoringUtils
  val csr_dma_guard_keyl = WireInit(0.U(xLen.W))
  val csr_dma_guard_keyh = WireInit(0.U(xLen.W))

  BoringUtils.addSink(csr_dma_guard_keyh, "csr_dma_guard_keyh")
  BoringUtils.addSink(csr_dma_guard_keyl, "csr_dma_guard_keyl")

  val pec_engine = Module(new QarmaMultiCycle(7))
  pec_engine.input.bits.actual_round  := 7.U(3.W)

  val rd = RegInit(0.U(5.W))
  val busy = RegInit(false.B)
  val resp = RegInit(false.B)
  val result = RegInit(0.U(xLen.W))
  val text  = RegInit(0.U(xLen.W))
  val tweak = RegInit(0.U(xLen.W))
  val keyh = RegInit(0.U(xLen.W))
  val keyl = RegInit(0.U(xLen.W))
  val encrypt = RegInit(false.B)
  val valid = RegInit(false.B)

  pec_engine.input.bits.keyh  := keyh
  pec_engine.input.bits.keyl  := keyl
  pec_engine.input.bits.text  := text
  pec_engine.input.bits.tweak := tweak
  pec_engine.input.bits.encrypt := encrypt
  pec_engine.input.valid   := valid

  pec_engine.output.ready  := Mux(pec_engine.output.valid, true.B, false.B)

  when(io.cmd.fire()){
    valid := true.B
    busy := true.B
    rd := io.cmd.bits.inst.rd
    val keySelect = Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)

    keyl := csr_dma_guard_keyl
    keyh := csr_dma_guard_keyh

    text  := io.cmd.bits.rs1
    tweak := io.cmd.bits.rs2
    encrypt := ~io.cmd.bits.inst.funct(0)

    printf("[cmd fire] text %x tweak %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  }

  when (pec_engine.output.valid) {
    result := pec_engine.output.bits.result
    resp := true.B

    printf("[pec valid] res %x\n", pec_engine.output.bits.result)
  }

  when (valid) {
    valid := false.B    
  }

  when(io.resp.fire()){
    resp := false.B
    busy := false.B      
    valid := false.B
    result := 0.U

    printf("[resp fire]\n")
  } 

  io.resp.bits.rd   := rd
  io.resp.bits.data := (result << 48) | text

  io.cmd.ready  := !busy
  io.busy       := busy
  io.resp.valid := resp

  // Disable unused interfaces
  io.interrupt      := false.B
  io.mem.req.valid  := false.B
}