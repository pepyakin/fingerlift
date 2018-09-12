use super::abi::{AbiGen, AbiLocation, AbiType};
use super::{Asm, IntCC, Label, Reg, RegSet};

use byteorder::{ByteOrder, LE};
use std::collections::HashMap;
use x86asm;

// TODO: Common patching mechanism suitable.

mod regs {
    use super::Reg;

    pub const RAX: Reg = Reg { idx: 0 };
    pub const RDI: Reg = Reg { idx: 1 };
    pub const RSI: Reg = Reg { idx: 2 };
    pub const RDX: Reg = Reg { idx: 3 };
    pub const RCX: Reg = Reg { idx: 4 };
    pub const RBX: Reg = Reg { idx: 5 };
    pub const R8: Reg = Reg { idx: 6 };
    pub const R9: Reg = Reg { idx: 7 };

    pub const ALL: &[Reg] = &[RAX, RCX, RDX, RBX, RDI, RSI, R8, R9];
}

impl Reg {
    fn into_quad(&self) -> x86asm::Reg {
        match *self {
            regs::RAX => x86asm::Reg::RAX,
            regs::RDI => x86asm::Reg::RDI,
            regs::RSI => x86asm::Reg::RSI,
            regs::RDX => x86asm::Reg::RDX,
            regs::RCX => x86asm::Reg::RCX,
            regs::RBX => x86asm::Reg::RBX,
            regs::R8 => x86asm::Reg::R8,
            regs::R9 => x86asm::Reg::R9,
            _ => panic!(),
        }
    }

    fn into_word(&self) -> Option<x86asm::Reg> {
        let r = match *self {
            regs::RAX => x86asm::Reg::EAX,
            regs::RDI => x86asm::Reg::EDI,
            regs::RSI => x86asm::Reg::ESI,
            regs::RDX => x86asm::Reg::EDX,
            regs::RCX => x86asm::Reg::ECX,
            regs::RBX => x86asm::Reg::EBX,
            _ => return None,
        };
        Some(r)
    }

    fn into_byte_low(&self) -> Option<x86asm::Reg> {
        let r = match *self {
            regs::RAX => x86asm::Reg::AL,
            regs::RDX => x86asm::Reg::DL,
            regs::RCX => x86asm::Reg::CL,
            regs::RBX => x86asm::Reg::BL,
            _ => return None,
        };
        Some(r)
    }
}

impl From<Reg> for x86asm::Reg {
    fn from(reg: Reg) -> x86asm::Reg {
        match reg {
            regs::RAX => x86asm::Reg::RAX,
            regs::RDI => x86asm::Reg::RDI,
            regs::RSI => x86asm::Reg::RSI,
            regs::RDX => x86asm::Reg::RDX,
            regs::RCX => x86asm::Reg::RCX,
            regs::RBX => x86asm::Reg::RBX,
            regs::R8 => x86asm::Reg::R8,
            regs::R9 => x86asm::Reg::R9,
            _ => panic!(),
        }
    }
}

pub struct X64AbiGen;

impl AbiGen for X64AbiGen {
    const RETURN_REG: Reg = regs::RAX;

    fn abi_args(args: &[AbiType]) -> Vec<AbiLocation> {
        const GPR_ARG_REGS: &'static [Reg] = &[
            regs::RDI,
            regs::RSI,
            regs::RDX,
            regs::RCX,
            regs::R8,
            regs::R9,
        ];

        let mut abi_locs = vec![];
        let mut offset = 0;
        for (i, arg_ty) in args.iter().enumerate() {
            match *arg_ty {
                AbiType::I32 | AbiType::I64 => (),
                _ => unimplemented!(),
            }

            let loc = if i < GPR_ARG_REGS.len() {
                AbiLocation::Reg(GPR_ARG_REGS[i])
            } else {
                let loc = AbiLocation::Stack(offset);
                offset += 1;
                loc
            };
            abi_locs.push(loc);
        }

        abi_locs
    }
}

pub struct X64Asm {
    buf: Vec<u8>,
    next_label_id: u32,
    label_offsets: HashMap<Label, Option<usize>>,
    label_uses: HashMap<Label, Vec<usize>>,

    /// Patch point set to `Some` on opening the stack frame.
    /// sub esp, [stack_size]
    ///
    /// Patched in `epilogue`.
    prologue_stack_size_offset: Option<usize>,
}

impl X64Asm {
    pub fn new() -> X64Asm {
        X64Asm {
            buf: Vec::new(),
            next_label_id: 0,
            label_offsets: HashMap::new(),
            label_uses: HashMap::new(),
            prologue_stack_size_offset: None,
        }
    }

    fn emit_instr(&mut self, instr: &x86asm::Instruction) {
        let mut writer = x86asm::InstructionWriter::new(&mut self.buf, x86asm::Mode::Long);
        writer.write(&instr).unwrap();
    }

    #[inline]
    fn emit<D: AsRef<[u8]>>(&mut self, data: D) {
        self.buf.extend_from_slice(data.as_ref())
    }

    #[inline]
    fn emit_u32(&mut self, value: u32) {
        let mut buf = [0; 4];
        LE::write_u32(&mut buf, value);
        self.emit(buf);
    }

    #[inline]
    fn current_offset(&self) -> usize {
        self.buf.len()
    }

    fn jmp_cc(&mut self, target: Label, cc: IntCC) {
        match cc {
            // je rel32
            IntCC::Equal => self.emit([0x0f, 0x84]),
            // jne rel32
            IntCC::NotEqual => self.emit([0x0f, 0x85]),
        }

        let cur = self.current_offset();
        self.label_uses.entry(target).or_default().push(cur);
        self.emit_u32(0);
    }

    fn slot_bp_offset(&self, slot: i32) -> i32 {
        if slot < 0 {
            unimplemented!()
        } else {
            let offset = -((slot + 1) * 8);
            offset
        }
    }
}

impl Asm for X64Asm {
    type AbiGen = X64AbiGen;

    fn available_regs() -> RegSet {
        let mut set = RegSet::empty();
        for reg in regs::ALL {
            set.insert(*reg);
        }
        set
    }

    fn trap(&mut self) {
        // ud2
        self.emit([0x0f, 0x0b]);
    }

    fn ret(&mut self) {
        // retq
        self.emit([0xc3]);
    }

    fn jmp(&mut self, target: Label) {
        // jmp
        self.emit([0xe9]);

        let cur = self.current_offset();
        self.label_uses.entry(target).or_default().push(cur);
        self.emit_u32(0);
    }

    fn jmp_i32_conditional(&mut self, target: Label, r1: Reg, r2: Reg, cc: IntCC) {
        let (r1, r2) = (r1.into_quad(), r2.into_quad());

        // let (r1, r2) = match (r1.into_word(), r2.into_word()) {
        //     (Some(r1), Some(r2)) => (r1, r2),
        //     (_, _) => unimplemented!(),
        // };

        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::CMP,
            x86asm::Operand::Direct(r1),
            x86asm::Operand::Direct(r2),
        ));

        self.jmp_cc(target, cc);
    }

    fn mov(&mut self, dst: Reg, src: Reg) {
        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::Direct(dst.into_quad()),
            x86asm::Operand::Direct(src.into_quad()),
        ));
    }

    /// Load a 32-bit constant into `dst` register.
    fn load_imm32(&mut self, dst: Reg, imm: u32) {
        // let dst = match dst.into_word() {
        //     Some(dst) => dst,
        //     None => unimplemented!(),
        // };

        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::Direct(dst.into_quad()),
            x86asm::Operand::Literal32(imm),
        ));
    }

    fn spill(&mut self, src: Reg, slot: i32) {
        let offset = self.slot_bp_offset(slot);

        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::IndirectDisplaced(x86asm::Reg::RBP, offset as u64, None, None),
            x86asm::Operand::Direct(src.into_quad()),
        ))
    }

    fn fill(&mut self, dst: Reg, slot: i32) {
        let offset = self.slot_bp_offset(slot);

        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::Direct(dst.into_quad()),
            x86asm::Operand::IndirectDisplaced(x86asm::Reg::RBP, offset as u64, None, None),
        ))
    }

    fn add_i32(&mut self, r1d: Reg, r2: Reg) {
        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::ADD,
            x86asm::Operand::Direct(r1d.into_quad()),
            x86asm::Operand::Direct(r2.into_quad()),
        ));
    }

    fn relop_i32(&mut self, r1d: Reg, r2: Reg, cc: IntCC) {
        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::CMP,
            x86asm::Operand::Direct(r1d.into_quad()),
            x86asm::Operand::Direct(r2.into_quad()),
        ));

        if let Some(r1d_byte) = r1d.into_byte_low() {
            // if `r1d` is one of one-byte regs such as RAX (-> al/ah),
            // then take advantage of setCC instruction.

            let r1d_word = r1d
                .into_word()
                .expect("1 byte regs always can be converted to 1 word regs");

            let instr = match cc {
                IntCC::Equal => x86asm::Mnemonic::SETE,
                IntCC::NotEqual => x86asm::Mnemonic::SETNE,
            };

            self.emit_instr(&x86asm::Instruction::new1(
                instr,
                x86asm::Operand::Direct(r1d_byte),
            ));
            self.emit_instr(&x86asm::Instruction::new2(
                x86asm::Mnemonic::MOVZX,
                x86asm::Operand::Direct(r1d_word),
                x86asm::Operand::Direct(r1d_byte),
            ));
        } else {
            let if_false = self.create_label();
            let end_label = self.create_label();

            self.load_imm32(r1d, 1);
            self.jmp_cc(end_label, cc);

            self.bind(if_false);
            self.load_imm32(r1d, 0);

            self.bind(end_label);
        }
    }

    fn create_label(&mut self) -> Label {
        let label = Label(self.next_label_id);
        self.next_label_id += 1;
        let previous = self.label_offsets.insert(label, None);
        assert!(previous.is_none(), "label is already created");
        label
    }

    fn bind(&mut self, label: Label) {
        let cur = self.current_offset();
        let previous = self.label_offsets.insert(label, Some(cur));
        assert_eq!(
            previous,
            Some(None),
            "label should be created and can be bound only once"
        );
    }

    fn prologue(&mut self) {
        self.emit_instr(&x86asm::Instruction::new1(
            x86asm::Mnemonic::PUSH,
            x86asm::Operand::Direct(x86asm::Reg::RBP),
        ));
        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::Direct(x86asm::Reg::RBP),
            x86asm::Operand::Direct(x86asm::Reg::RSP),
        ));

        let cur = self.current_offset() + 3;
        assert!(
            self.prologue_stack_size_offset.is_none(),
            "stack offset is already set"
        );
        self.prologue_stack_size_offset = Some(cur);
        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::SUB,
            x86asm::Operand::Direct(x86asm::Reg::RSP),
            x86asm::Operand::Literal32(0),
        ));
    }

    fn epilogue(&mut self, highest_slot: i32) {
        assert!(highest_slot >= 0);
        let stack_size = highest_slot * 8;

        {
            let stack_frame_size_offset = self
                .prologue_stack_size_offset
                .take()
                .expect("epilogue call without prologue?");
            let use_site = &mut self.buf[stack_frame_size_offset..stack_frame_size_offset + 4];
            LE::write_i32(use_site, stack_size);
        }

        self.emit_instr(&x86asm::Instruction::new2(
            x86asm::Mnemonic::MOV,
            x86asm::Operand::Direct(x86asm::Reg::RSP),
            x86asm::Operand::Direct(x86asm::Reg::RBP),
        ));
        self.emit_instr(&x86asm::Instruction::new1(
            x86asm::Mnemonic::POP,
            x86asm::Operand::Direct(x86asm::Reg::RBP),
        ));
    }

    fn finalize(mut self) -> Vec<u8> {
        println!(
            "finalization: \n  offsets: {:?}\n  uses: {:?}",
            self.label_offsets, self.label_uses
        );

        // Patch used labels.
        for (label, label_uses) in self.label_uses {
            let label_offset = self
                .label_offsets
                .get(&label)
                .expect("unknown label")
                .expect("unresolved label");

            for label_used_at in label_uses {
                let use_site = &mut self.buf[label_used_at..label_used_at + 4];
                let diff: i32 = label_offset as i32 - label_used_at as i32 - 4;
                LE::write_i32(use_site, diff);
            }
        }

        self.buf
    }
}
