package freechips.rocketchip.rocc.qarma

import chisel3._
import chisel3.util._

trait QarmaParams {
  val debug = false
  val ppldbg = false
  // TODO support super-scalar in both scheduler and SRAM
  val superscalar = false
  val ds = false

  val n = 64
  val m = n / 16
  val sbox_number = 2

  val check_box = Array(
    "hc003b93999b33765".U,
    "h270a787275c48d10".U,
    "h5c06a7501b63b2fd".U
  )
  val alpha = "hC0AC29B7C97C50DD".U
  val c = VecInit(
    "h0000000000000000".U, "h13198A2E03707344".U, "hA4093822299F31D0".U, "h082EFA98EC4E6C89".U,
    "h452821E638D01377".U, "hBE5466CF34E90C6C".U, "h3F84D5B5B5470917".U, "h9216D5D98979FB1B".U
  )
  val sbox = Array(
    VecInit(0.U(4.W), 14.U(4.W), 2.U(4.W), 10.U(4.W), 9.U(4.W), 15.U(4.W), 8.U(4.W), 11.U(4.W), 6.U(4.W), 4.U(4.W), 3.U(4.W), 7.U(4.W), 13.U(4.W), 12.U(4.W), 1.U(4.W), 5.U(4.W)),
    VecInit(10.U(4.W), 13.U(4.W), 14.U(4.W), 6.U(4.W), 15.U(4.W), 7.U(4.W), 3.U(4.W), 5.U(4.W), 9.U(4.W), 8.U(4.W), 0.U(4.W), 12.U(4.W), 11.U(4.W), 1.U(4.W), 2.U(4.W), 4.U(4.W)),
    VecInit(11.U(4.W), 6.U(4.W), 8.U(4.W), 15.U(4.W), 12.U(4.W), 0.U(4.W), 9.U(4.W), 14.U(4.W), 3.U(4.W), 7.U(4.W), 4.U(4.W), 5.U(4.W), 13.U(4.W), 2.U(4.W), 1.U(4.W), 10.U(4.W))
  )
  val sbox_inv = Array(
    VecInit(0.U, 14.U, 2.U, 10.U, 9.U, 15.U, 8.U, 11.U, 6.U, 4.U, 3.U, 7.U, 13.U, 12.U, 1.U, 5.U),
    VecInit(10.U, 13.U, 14.U, 6.U, 15.U, 7.U, 3.U, 5.U, 9.U, 8.U, 0.U, 12.U, 11.U, 1.U, 2.U, 4.U),
    VecInit(5.U, 14.U, 13.U, 8.U, 10.U, 11.U, 1.U, 9.U, 2.U, 6.U, 15.U, 0.U, 4.U, 12.U, 7.U, 3.U)
  )
  val t = Array(0, 11, 6, 13, 10, 1, 12, 7, 5, 14, 3, 8, 15, 4, 9, 2)
  val t_inv = Array(0, 5, 15, 10, 13, 8, 2, 7, 11, 14, 4, 1, 6, 3, 9, 12)
  val h = Array(6, 5, 14, 15, 0, 1, 2, 3, 7, 12, 13, 4, 8, 9, 10, 11)
  val h_inv = Array(4, 5, 6, 7, 11, 1, 0, 8, 12, 13, 14, 15, 9, 10, 2, 3)
  val M = Array(
    0, 1, 2, 1,
    1, 0, 1, 2,
    2, 1, 0, 1,
    1, 2, 1, 0
  )

  def lfsr_inv_operation(operand: UInt): UInt = {
    Cat(operand(2, 0), operand(0) ^ operand(3))
  }

  def log(round: Int, num1: UInt, num2: UInt): Unit = {
    if (debug) {
      printf("cp %d\tis=%x tk=%x\n", round.asUInt, num1, num2)
    }
  }

  def o_operation(operand: UInt): UInt = {
    Cat(operand(0), operand(operand.getWidth - 1, 1)) ^ (operand >> (n - 1).asUInt).asUInt
  }

  def lfsr_operation(operand: UInt): UInt = {
    Cat(operand(0) ^ operand(1), operand(3, 1))
  }

  val code_map_width = 2
  val code_map = Map("end" -> 0.U(code_map_width.W), "forward" -> 1.U(code_map_width.W),
    "reflect" -> 2.U(code_map_width.W), "backward" -> 3.U(code_map_width.W))
}

class MixColumnOperatorIO extends Bundle {
  val in = Input(UInt(64.W))
  val out = Output(UInt(64.W))
}

class OperatorIO extends Bundle {
  val is = Input(UInt(64.W))
  val tk = Input(UInt(64.W))
  val round_zero = Input(Bool())
  val out = Output(UInt(64.W))
}

class TweakIO extends Bundle {
  val old_tk = Input(UInt(64.W))
  val new_tk = Output(UInt(64.W))
}

class DataBundle(max_round: Int, step_len: Int) extends Bundle with QarmaParams {
  val code = UInt((code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
  val step_end = UInt((log2Ceil(step_len) * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
  val is = UInt(64.W)
  val tk = UInt(64.W)
  val k0 = UInt(64.W)
  val k1 = UInt(64.W)
  val w0 = UInt(64.W)
  val w1 = UInt(64.W)
}

class MetaBundle(max_round: Int) extends Bundle with QarmaParams {
  val valid = Bool()
  val done = Bool()
  val pointer = UInt(log2Ceil(code_map_width * (max_round * 2 + 2)).W)
}

class MixColumnOperator extends Module with QarmaParams {
  val io = IO(new MixColumnOperatorIO)

  val result_vec = Wire(Vec(16, UInt(4.W)))
  val temp_vec = Wire(Vec(16, Vec(4, UInt(4.W))))
  for (x <- 0 until 4; y <- 0 until 4) {
    for (j <- 0 until 4) {
      val a = io.in.asTypeOf(Vec(16, UInt(4.W)))(15 - (4 * j + y)).asUInt
      val b = M(4 * x + j)
      when(b.asUInt =/= 0.U) {
        temp_vec(15 - (4 * x + y))(j) := Cat(a(3 - b, 0), a(3, 3 - b)) >> 1.U
      }.otherwise {
        temp_vec(15 - (4 * x + y))(j) := 0.U
      }
    }
    result_vec(15 - (4 * x + y)) := temp_vec(15 - (4 * x + y)).reduce((a, b) => (a ^ b).asUInt)
  }
  io.out := result_vec.asTypeOf(UInt(64.W))
}

class ForwardOperator extends Module with QarmaParams {
  val io = IO(new OperatorIO)

  val new_is = io.is ^ io.tk
  val mix_column_is = Wire(UInt(64.W))
  val perm_vec = Wire(Vec(16, UInt(4.W)))
  val result_vec = Wire(Vec(16, UInt(4.W)))
  val sbox_prev_is = Mux(io.round_zero, new_is, mix_column_is)
  val mix_column_operator = Module(new MixColumnOperator)

  for (i <- 0 until 16) {
    perm_vec(15 - i) := new_is.asTypeOf(Vec(16, UInt(4.W)))(15 - t(i))
  }

  mix_column_operator.io.in := perm_vec.asTypeOf(UInt(64.W))
  mix_column_is := mix_column_operator.io.out

  for (i <- 0 until 16) {
    result_vec(15 - i) := sbox(sbox_number)(sbox_prev_is.asTypeOf(Vec(16, UInt(4.W)))(15 - i))
  }

  io.out := result_vec.asTypeOf(UInt(64.W))
}

class BackwardOperator extends Module with QarmaParams {
  val io = IO(new OperatorIO)

  val inv_sbox_is = Wire(Vec(16, UInt(4.W)))
  val mix_column_is = Wire(UInt(64.W))
  val result_vec = Wire(Vec(16, UInt(4.W)))
  val perm_vec = Wire(Vec(16, UInt(4.W)))
  val mix_column_operator = Module(new MixColumnOperator)

  for (i <- 0 until 16) {
    inv_sbox_is(15 - i) := sbox_inv(sbox_number)(io.is.asTypeOf(Vec(16, UInt(4.W)))(15 - i))
  }

  mix_column_operator.io.in := inv_sbox_is.asTypeOf(UInt(64.W))
  mix_column_is := mix_column_operator.io.out

  for (i <- 0 until 16) {
    perm_vec(15 - i) := mix_column_is.asTypeOf(Vec(16, UInt(4.W)))(15 - t_inv(i))
  }

  result_vec := Mux(io.round_zero, inv_sbox_is, perm_vec)
  io.out := result_vec.asTypeOf(UInt(64.W)) ^ io.tk
}

class ForwardTweakUpdateOperator extends Module with QarmaParams {
  val io = IO(new TweakIO)

  val result_vec = Wire(Vec(16, UInt(4.W)))
  val temp_vec = Wire(Vec(16, UInt(4.W)))
  for (i <- 0 until 16) {
    temp_vec(15 - i) := io.old_tk.asTypeOf(Vec(16, UInt(4.W)))(15 - h(i))
  }

  for (i <- 0 until 16) {
    if (Set(0, 1, 3, 4, 8, 11, 13).contains(i)) {
      result_vec(15 - i) := lfsr_operation(temp_vec(15 - i))
    } else {
      result_vec(15 - i) := temp_vec(15 - i)
    }
  }

  io.new_tk := result_vec.asTypeOf(UInt(64.W))
}

class BackwardTweakUpdateOperator extends Module with QarmaParams {
  val io = IO(new TweakIO)

  val mtk = Wire(Vec(16, UInt(4.W)))
  val result_vec = Wire(Vec(16, UInt(4.W)))
  for (i <- 0 until 16) {
    val tk_base_index = (15 - i) * 4
    val step_result = if (List(0, 1, 3, 4, 8, 11, 13).indexOf(i) != -1)
      lfsr_inv_operation(io.old_tk(tk_base_index + 3, tk_base_index)) else io.old_tk(tk_base_index + 3, tk_base_index)
    mtk(15 - i) := step_result
  }
  for (i <- 0 until 16) {
    result_vec(15 - i) := mtk(15 - h_inv(i))
  }
  io.new_tk := result_vec.asTypeOf(UInt(64.W))
}

class PseudoReflectOperator extends Module with QarmaParams {
  val io = IO(new Bundle {
    val is = Input(UInt(64.W))
    val key = Input(UInt(64.W))
    val out = Output(UInt(64.W))
  })

  val perm_vec = Wire(Vec(16, UInt(4.W)))
  val mix_column_is = Wire(UInt(64.W))
  val mix_column_operator = Module(new MixColumnOperator)
  val result_vec = Wire(Vec(16, UInt(4.W)))
  val tweakey_is = Wire(Vec(16, UInt(4.W)))

  for (i <- 0 until 16) {
    perm_vec(15 - i) := io.is.asTypeOf(Vec(16, UInt(4.W)))(15 - t(i))
  }

  mix_column_operator.io.in := perm_vec.asTypeOf(UInt(64.W))
  mix_column_is := mix_column_operator.io.out

  for (i <- 0 until 16) {
    val key_base = 4 * (15 - i)
    tweakey_is(15 - i) := (mix_column_is.asTypeOf(Vec(16, UInt(4.W)))(15 - i) ^
      io.key(key_base + 3, key_base)).asUInt
  }

  for (i <- 0 until 16) {
    result_vec(15 - i) := tweakey_is(15 - t_inv(i))
  }

  io.out := result_vec.asTypeOf(UInt(64.W))
}

class ExecutionContext(max_round: Int = 7, depth: Int = 0, port: Int = 0, step_len: Int)
  extends MultiIOModule with QarmaParams {

  if (port != 1 && port != 2 && port != 0) {
    println("Variable read_port in ExecutionContext should be in [1, 2].")
    sys.exit(-1)
  }

  val slot_depth = if (depth == 0) {
    if (superscalar) 2 else 1
  } else depth
  val read_write_port = if (port == 0) slot_depth else port
  // forward * mr + backward * mr + reflect + end
  val code_width = code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)
  val valid_width = 1
  val done_width = 1
  val data_width = 64 * 6
  val pointer_width = log2Ceil(code_width)
  // code width + is tk w0 w1 k0 k1
  val data_slot_width = new DataBundle(max_round, step_len).getWidth
  // valid + done + pointer
  val meta_slot_width = new MetaBundle(max_round).getWidth

  val input = IO(new Bundle {
    val new_data = Input(Vec(slot_depth, new DataBundle(max_round, step_len)))
    val new_meta = Input(Vec(slot_depth, new MetaBundle(max_round)))
    val update = Input(Vec(slot_depth, Bool()))
  })
  val output = IO(new Bundle {
    val old_data = Output(Vec(slot_depth, new DataBundle(max_round, step_len)))
    val old_meta = Output(Vec(slot_depth, new MetaBundle(max_round)))
  })

  // Here READ_PORT === WRITE_PORT to simplify and accelerate
  val data = RegInit(VecInit(Seq.fill(slot_depth)(0.U(data_slot_width.W))))
  val meta = RegInit(VecInit(Seq.fill(slot_depth)(0.U(meta_slot_width.W))))

  for (i <- 0 until slot_depth) {
    when(input.update(i)) {
      meta(i) := input.new_meta(i).asUInt
      data(i) := input.new_data(i).asUInt
    }
  }

  for (i <- 0 until slot_depth) {
    output.old_meta(i) := meta(i).asTypeOf(new MetaBundle(max_round))
    output.old_data(i) := data(i).asTypeOf(new DataBundle(max_round, step_len))
  }
}

trait QarmaParamsIO extends MultiIOModule with QarmaParams {
  val input = IO(Flipped(Decoupled(new Bundle {
    val encrypt = Bool()
    val keyh = UInt(64.W)
    val keyl = UInt(64.W)
    val tweak = UInt(64.W)
    val text = UInt(64.W)
    val actual_round = UInt(3.W)
  })))
  val output = IO(Decoupled(new Bundle {
    val result = UInt(64.W)
  }))
}

class QarmaSingleCysle(max_round: Int = 7) extends QarmaParamsIO {

  // Step 1 ---- Generate Key
  val mix_column = Module(new MixColumnOperator)
  mix_column.io.in := input.bits.keyl
  val w0 = Mux(input.bits.encrypt, input.bits.keyh, o_operation(input.bits.keyh))
  val k0 = Mux(input.bits.encrypt, input.bits.keyl, input.bits.keyl ^ alpha)
  val w1 = Mux(input.bits.encrypt, o_operation(input.bits.keyh), input.bits.keyh)
  val k1 = Mux(input.bits.encrypt, input.bits.keyl, mix_column.io.out)

  // Step 2 ---- Define Hardware
  val is_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
  val tk_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
  val forward_operator_vec = Array.fill(max_round + 1)(Module(new ForwardOperator).io)
  val forward_tweak_update_operator_vec = Array.fill(max_round)(Module(new ForwardTweakUpdateOperator).io)
  val reflector = Module(new PseudoReflectOperator)
  val backward_operator_vec = Array.fill(max_round + 1)(Module(new BackwardOperator).io)
  val backward_tweak_update_operator_vec = Array.fill(max_round)(Module(new BackwardTweakUpdateOperator).io)
  var wire_index = 0
  var module_index = 0

  // Step 3 ---- Forward
  is_vec(wire_index) := input.bits.text ^ w0
  log(1, is_vec(wire_index), tk_vec(wire_index))
  tk_vec(wire_index) := input.bits.tweak
  for (i <- 0 until max_round) {
    forward_operator_vec(module_index).is := is_vec(wire_index)
    forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ k0 ^ c(i.asUInt)
    forward_operator_vec(module_index).round_zero := i.asUInt === 0.U
    forward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
    wire_index = wire_index + 1
    is_vec(wire_index) := Mux(i.asUInt < input.bits.actual_round,
      forward_operator_vec(module_index).out, is_vec(wire_index - 1))
    tk_vec(wire_index) := Mux(i.asUInt < input.bits.actual_round,
      forward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
    module_index = module_index + 1
    log(2 + i, is_vec(wire_index), tk_vec(wire_index))
  }

  // Step 4 ---- Reflect
  forward_operator_vec(module_index).is := is_vec(wire_index)
  forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ w1
  forward_operator_vec(module_index).round_zero := false.B
  wire_index = wire_index + 1
  is_vec(wire_index) := forward_operator_vec(module_index).out
  tk_vec(wire_index) := tk_vec(wire_index - 1)
  log(max_round + 2, is_vec(wire_index), tk_vec(wire_index))
  module_index = max_round
  reflector.io.is := is_vec(wire_index)
  reflector.io.key := k1
  wire_index = wire_index + 1
  is_vec(wire_index) := reflector.io.out
  tk_vec(wire_index) := tk_vec(wire_index - 1)
  log(max_round + 3, is_vec(wire_index), tk_vec(wire_index))
  backward_operator_vec(module_index).is := is_vec(wire_index)
  backward_operator_vec(module_index).tk := tk_vec(wire_index) ^ w0
  backward_operator_vec(module_index).round_zero := false.B
  wire_index = wire_index + 1
  is_vec(wire_index) := backward_operator_vec(module_index).out
  tk_vec(wire_index) := tk_vec(wire_index - 1)
  log(max_round + 4, is_vec(wire_index), tk_vec(wire_index))
  module_index = 0

  // Step 5 ---- Backward
  for (i <- 0 until max_round) {
    val j = max_round - 1 - i
    backward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
    backward_operator_vec(module_index).is := is_vec(wire_index)
    wire_index = wire_index + 1
    backward_operator_vec(module_index).tk := k0 ^ tk_vec(wire_index) ^ c(j.asUInt) ^ alpha.asUInt
    backward_operator_vec(module_index).round_zero := i.asUInt + 1.U === max_round.asUInt
    tk_vec(wire_index) := Mux(j.asUInt < input.bits.actual_round,
      backward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
    is_vec(wire_index) := Mux(j.asUInt < input.bits.actual_round,
      backward_operator_vec(module_index).out, is_vec(wire_index - 1))
    module_index = module_index + 1
    log(max_round + 5 + i, is_vec(wire_index), tk_vec(wire_index))
  }

  output.bits.result := is_vec(wire_index) ^ w1
  output.valid := true.B
  input.ready := true.B
}

class QarmaMultiCycle(max_round: Int = 7) extends QarmaParamsIO {
  // Step 1 ---- Generate Key
  val mix_column = Module(new MixColumnOperator)
  mix_column.io.in := input.bits.keyl
  val w0 = Mux(input.bits.encrypt, input.bits.keyh, o_operation(input.bits.keyh))
  val k0 = Mux(input.bits.encrypt, input.bits.keyl, input.bits.keyl ^ alpha)
  val w1 = Mux(input.bits.encrypt, o_operation(input.bits.keyh), input.bits.keyh)
  val k1 = Mux(input.bits.encrypt, input.bits.keyl, mix_column.io.out)

  // Step 2 ---- Define Hardware
  val is_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
  val tk_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
  val forward_operator_vec = Array.fill(max_round + 1)(Module(new ForwardOperator).io)
  val forward_tweak_update_operator_vec = Array.fill(max_round)(Module(new ForwardTweakUpdateOperator).io)
  val reflector = Module(new PseudoReflectOperator)
  val backward_operator_vec = Array.fill(max_round + 1)(Module(new BackwardOperator).io)
  val backward_tweak_update_operator_vec = Array.fill(max_round)(Module(new BackwardTweakUpdateOperator).io)
  var wire_index = 0
  var module_index = 0
  val temp_index = new Array[Int](3)
  val busy_table = RegInit(VecInit(Seq.fill(4)(false.B)))
  val stall_table = Wire(Vec(4, Bool()))
  val round_table = RegInit(VecInit(Seq.fill(4)(0.U(3.W))))
  val internal_regs = RegInit(VecInit(Seq.fill(4)(0.U((64 * 6).W))))

  // Step 3 ---- Forward Internal-Regs is/tk/w0/k0/w1/k1
  is_vec(wire_index) := internal_regs(0)(64 * 6 - 1, 64 * 5)
  log(1, is_vec(wire_index), tk_vec(wire_index))
  tk_vec(wire_index) := internal_regs(0)(64 * 5 - 1, 64 * 4)
  for (i <- 0 until max_round) {
    forward_operator_vec(module_index).is := is_vec(wire_index)
    forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ internal_regs(0)(64 * 3 - 1, 64 * 2) ^ c(i.asUInt)
    forward_operator_vec(module_index).round_zero := i.asUInt === 0.U
    forward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
    wire_index = wire_index + 1
    is_vec(wire_index) := Mux(i.asUInt < input.bits.actual_round,
      forward_operator_vec(module_index).out, is_vec(wire_index - 1))
    tk_vec(wire_index) := Mux(i.asUInt < input.bits.actual_round,
      forward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
    module_index = module_index + 1
    log(2 + i, is_vec(wire_index), tk_vec(wire_index))
  }

  // Step 4 ---- Reflect
  temp_index(0) = wire_index
  forward_operator_vec(module_index).is := internal_regs(1)(64 * 6 - 1, 64 * 5)
  forward_operator_vec(module_index).tk := internal_regs(1)(64 * 5 - 1, 64 * 4) ^ internal_regs(1)(64 * 2 - 1, 64 * 1)
  forward_operator_vec(module_index).round_zero := false.B
  wire_index = wire_index + 1
  is_vec(wire_index) := forward_operator_vec(module_index).out
  tk_vec(wire_index) := internal_regs(1)(64 * 5 - 1, 64 * 4)
  log(max_round + 2, is_vec(wire_index), tk_vec(wire_index))
  module_index = max_round
  reflector.io.is := is_vec(wire_index)
  reflector.io.key := internal_regs(1)(64 * 1 - 1, 64 * 0)
  wire_index = wire_index + 1
  is_vec(wire_index) := reflector.io.out
  tk_vec(wire_index) := tk_vec(wire_index - 1)
  log(max_round + 3, is_vec(wire_index), tk_vec(wire_index))
  backward_operator_vec(module_index).is := is_vec(wire_index)
  backward_operator_vec(module_index).tk := tk_vec(wire_index) ^ internal_regs(1)(64 * 4 - 1, 64 * 3)
  backward_operator_vec(module_index).round_zero := false.B
  wire_index = wire_index + 1
  is_vec(wire_index) := backward_operator_vec(module_index).out
  tk_vec(wire_index) := tk_vec(wire_index - 1)
  log(max_round + 4, is_vec(wire_index), tk_vec(wire_index))
  module_index = 0

  // Step 5 ---- Backward
  temp_index(1) = wire_index
  for (i <- 0 until max_round) {
    val j = max_round - 1 - i
    backward_tweak_update_operator_vec(module_index).old_tk := Mux(j.asUInt + 1.U === input.bits.actual_round,
      internal_regs(2)(64 * 5 - 1, 64 * 4), tk_vec(wire_index))
    backward_operator_vec(module_index).is := Mux(j.asUInt + 1.U === input.bits.actual_round,
      internal_regs(2)(64 * 6 - 1, 64 * 5), is_vec(wire_index))
    wire_index = wire_index + 1
    backward_operator_vec(module_index).tk := internal_regs(2)(64 * 3 - 1, 64 * 2) ^ tk_vec(wire_index) ^ c(j.asUInt) ^ alpha.asUInt
    backward_operator_vec(module_index).round_zero := i.asUInt + 1.U === max_round.asUInt
    tk_vec(wire_index) := Mux(j.asUInt < input.bits.actual_round,
      backward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
    is_vec(wire_index) := Mux(j.asUInt < input.bits.actual_round,
      backward_operator_vec(module_index).out, is_vec(wire_index - 1))
    module_index = module_index + 1
    log(max_round + 5 + i, is_vec(wire_index), tk_vec(wire_index))
  }
  temp_index(2) = wire_index

  // Step 6 ---- Busy Table
  for (j <- 0 until 4) {
    val i = 3 - j
    if (i == 3) {
      stall_table(i) := Mux(busy_table(i), !output.ready, false.B)
    } else {
      stall_table(i) := Mux(busy_table(i), stall_table(i + 1), false.B)
    }
    if (i == 0) {
      when(!stall_table(0)) {
        busy_table(0) := input.valid
        round_table(0) := input.bits.actual_round
        internal_regs(0) := Cat(input.bits.text ^ w0, input.bits.tweak,
          w0, k0, w1, k1)
      }
    } else {
      when(!stall_table(i)) {
        busy_table(i) := busy_table(i - 1)
        round_table(i) := round_table(i - 1)
        internal_regs(i) := Cat(is_vec(temp_index(i - 1)), tk_vec(temp_index(i - 1)),
          internal_regs(i - 1)(64 * 4 - 1, 0))
      }
    }
  }

  when (output.valid) {
    printf("[PEC] stall_table %x\t\t\t%x\t\t\t%x\t\t\t%x\n", stall_table(0), stall_table(1), stall_table(2), stall_table(3))
    printf("[PEC] busy_table %x\t\t\t%x\t\t\t%x\t\t\t%x\n", busy_table(0), busy_table(1), busy_table(2), busy_table(3))
    printf("[PEC] internal_regs %x\t%x\t%x\t%x\n", internal_regs(0)(64 * 6 - 1, 64 * 5),
      internal_regs(1)(64 * 6 - 1, 64 * 5), internal_regs(2)(64 * 6 - 1, 64 * 5), internal_regs(3)(64 * 6 - 1, 64 * 5))
  }

  output.bits.result := internal_regs(3)(64 * 6 - 1, 64 * 5) ^ internal_regs(3)(64 * 2 - 1, 64 * 1)
  output.valid := busy_table(3)
  input.ready := !stall_table(0)
}