use ::{BroadcastMode, Instruction, MaskReg, MergeMode, Mnemonic, OperandSize, Reg, RoundingMode};
use ::RegType::*;
use ::instruction_def::*;
use ::Operand::*;
use ::Reg::*;
use ::RegScale::*;

fn kunpckbw_1() {
    run_test(&Instruction { mnemonic: Mnemonic::KUNPCKBW, operand1: Some(Direct(K7)), operand2: Some(Direct(K6)), operand3: Some(Direct(K1)), operand4: None, lock: false, rounding_mode: None, merge_mode: None, sae: false, mask: None, broadcast: None }, &[197, 205, 75, 249], OperandSize::Dword)
}

fn kunpckbw_2() {
    run_test(&Instruction { mnemonic: Mnemonic::KUNPCKBW, operand1: Some(Direct(K6)), operand2: Some(Direct(K6)), operand3: Some(Direct(K2)), operand4: None, lock: false, rounding_mode: None, merge_mode: None, sae: false, mask: None, broadcast: None }, &[197, 205, 75, 242], OperandSize::Qword)
}

