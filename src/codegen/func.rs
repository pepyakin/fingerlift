use codegen::asm::abi::{self, AbiGen};
use codegen::asm::{Asm, IntCC, Label, Reg, RegSet};
use parity_wasm::elements::{FuncBody, FunctionType, Instruction, ValueType};

pub trait ModuleEnv {
    /// Returns the type of the specified global.
    fn global_ty(&self, idx: usize) -> ValueType;
}

/// Type of a control frame.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ControlFrameKind {
    /// Usual block frame.
    ///
    /// Can be used for an implicit function block.
    Block { end_label: Label },
    /// Loop frame (branching to the beginning of block).
    Loop { header: Label },
    /// True-subblock of if expression.
    IfTrue {
        /// If jump happens inside the if-true block then control will
        /// land on this label.
        end_label: Label,

        /// If the condition of the `if` statement is unsatisfied, control
        /// will land on this label. This label might point to `else` block if it
        /// exists. Otherwise it equal to `end_label`.
        if_not: Label,
    },
    /// False-subblock of if expression.
    IfFalse { end_label: Label },
}

impl ControlFrameKind {
    /// Returns a label which should be used as a branch destination.
    fn br_destination(&self) -> Label {
        match *self {
            ControlFrameKind::Block { end_label } => end_label,
            ControlFrameKind::Loop { header } => header,
            ControlFrameKind::IfTrue { end_label, .. } => end_label,
            ControlFrameKind::IfFalse { end_label } => end_label,
        }
    }

    /// Returns `true` if this block of a loop kind.
    fn is_loop(&self) -> bool {
        match *self {
            ControlFrameKind::Loop { .. } => true,
            _ => false,
        }
    }
}

/// A wasm control frame
struct Control {
    kind: ControlFrameKind,
}

struct RegAlloc {
    avail: RegSet,
    scratch_reg: Reg,
}

impl RegAlloc {
    fn new(mut avail: RegSet) -> RegAlloc {
        let scratch_reg = avail.last().expect("can't be empty");
        avail.remove(scratch_reg);

        RegAlloc { avail, scratch_reg }
    }

    fn has_gpr(&self) -> bool {
        !self.avail.is_empty()
    }

    fn is_available(&self, reg: Reg) -> bool {
        self.avail.has(reg)
    }

    fn alloc_any_gpr(&mut self) -> Reg {
        assert!(self.has_gpr());
        let first = self.avail.first().expect("can't be empty");
        self.avail.remove(first);
        first
    }

    fn alloc_gpr(&mut self, reg: Reg) {
        assert!(self.is_available(reg));
        self.avail.remove(reg);
    }

    fn free_gpr(&mut self, reg: Reg) {
        assert!(!self.is_available(reg));
        self.avail.insert(reg);
    }

    fn scratch_reg(&self) -> Reg {
        self.scratch_reg
    }
}

#[derive(Copy, Clone)]
enum Literal {
    I32(u32),
    I64(u64),
    F32(u32),
    F64(u64),
}

#[derive(Clone)]
enum Location {
    Register(Reg),
    Stack(i32),
    Literal(Literal),
}

/// Flush all values (including ephemeral) onto the stack.
fn flush_stack<A: Asm>(
    stack: &mut Vec<Location>,
    asm: &mut A,
    stack_layout: &mut StackLayout,
    reg_alloc: &mut RegAlloc,
) {
    for (i, item) in stack.iter_mut().enumerate() {
        let new_item = match *item {
            Location::Register(reg) => {
                let stack_slot = stack_layout.stack_slot(i as u32);
                asm.spill(reg, stack_slot);
                reg_alloc.free_gpr(reg);
                Location::Stack(stack_slot)
            }
            Location::Literal(lit) => {
                let stack_slot = stack_layout.stack_slot(i as u32);
                match lit {
                    Literal::I32(v) => {
                        let scratch_reg = reg_alloc.scratch_reg();
                        asm.load_imm32(scratch_reg, v);
                        asm.spill(scratch_reg, stack_slot);
                    }
                    _ => unimplemented!(),
                }
                Location::Stack(stack_slot)
            }
            // Already in stack.
            Location::Stack(_) => continue,
        };

        *item = new_item;
    }
}

fn alloc_i32<A: Asm>(
    stack: &mut Vec<Location>,
    asm: &mut A,
    stack_layout: &mut StackLayout,
    reg_alloc: &mut RegAlloc,
) -> Reg {
    if reg_alloc.has_gpr() {
        return reg_alloc.alloc_any_gpr();
    }

    flush_stack(stack, asm, stack_layout, reg_alloc);

    assert!(reg_alloc.has_gpr());
    reg_alloc.alloc_any_gpr()
}

fn pop_i32_into<A: Asm>(item: &Location, asm: &mut A, dst: Reg) {
    match *item {
        Location::Literal(literal) => match literal {
            Literal::I32(v) => {
                asm.load_imm32(dst, v as u32);
            }
            _ => panic!(),
        },
        Location::Register(src) => {
            asm.mov(dst, src);
        }
        Location::Stack(offset) => {
            asm.fill(dst, offset);
        }
    }
}

fn pop_i32<A: Asm>(
    stack: &mut Vec<Location>,
    asm: &mut A,
    stack_layout: &mut StackLayout,
    reg_alloc: &mut RegAlloc,
) -> Reg {
    let item = stack.pop().unwrap();

    match item {
        Location::Register(reg) => reg,
        other => {
            let reg = alloc_i32(stack, asm, stack_layout, reg_alloc);
            pop_i32_into(&other, asm, reg);
            reg
        }
    }
}

// TODO: Come up with calling convention
// TODO: Come up with stack layout

fn ty_to_abi_ty(ty: ValueType) -> ::codegen::asm::abi::AbiType {
    match ty {
        ValueType::I32 => ::codegen::asm::abi::AbiType::I32,
        ValueType::I64 => ::codegen::asm::abi::AbiType::I64,
        _ => unimplemented!(),
    }
}

struct StackLayout {
    local_homes: Vec<i32>,
    stack_offset: i32,
    highest_slot: i32,
}

impl StackLayout {
    fn new(abi_arg_locs: &[abi::AbiLocation], func_body: &FuncBody) -> StackLayout {
        let mut next_slot = 0;

        let mut local_homes = Vec::new();
        for abi_arg_loc in abi_arg_locs {
            match abi_arg_loc {
                abi::AbiLocation::Reg(ref _reg) => {
                    local_homes.push(next_slot);
                    next_slot += 1;
                }
                abi::AbiLocation::Stack(slot) => {
                    local_homes.push(-slot);
                }
            }
        }

        println!("locals = {:?}", func_body.locals());

        // Handle non-arg locals
        for local_group in func_body.locals() {
            for _ in 0..local_group.count() {
                local_homes.push(next_slot);
                next_slot += 1;
            }
        }

        StackLayout {
            local_homes,
            stack_offset: next_slot,
            highest_slot: next_slot,
        }
    }

    /// Returns the slot for the local under the specified index.
    fn local_slot(&self, index: usize) -> i32 {
        // TODO: make `index` u32
        self.local_homes[index]
    }

    fn stack_slot(&mut self, height: u32) -> i32 {
        let slot = self.stack_offset + height as i32;
        if self.highest_slot < slot {
            self.highest_slot = slot;
        }
        slot
    }
}

pub fn compile_func<E: ModuleEnv, A: Asm>(
    _env: &E,
    asm: &mut A,
    sig: &FunctionType,
    func_body: &FuncBody,
) {
    let mut control_frames = Vec::new();

    // Push a frame for the implicit function block.
    let epilogue_label = asm.create_label();
    control_frames.push(Control {
        kind: ControlFrameKind::Block {
            end_label: epilogue_label,
        },
    });
    // TODO: Set arity for the block according to the `sig` of the function.

    let mut stack = Vec::new();
    let mut reg_alloc = RegAlloc::new(A::available_regs());

    // Get locations of arguments according to the ABI for this function.
    let abi_param_tys = sig
        .params()
        .iter()
        .cloned()
        .map(ty_to_abi_ty)
        .collect::<Vec<_>>();
    let abi_arg_locs = A::AbiGen::abi_args(&abi_param_tys);

    // Layout the stack.
    // - vmctx
    // - spill slots for arguments that came in registers
    // - locals
    let mut stack_layout = StackLayout::new(&abi_arg_locs, func_body);

    asm.prologue();

    // Spill all incoming args in registers immediately.
    for (i, abi_arg_loc) in abi_arg_locs.iter().enumerate() {
        match abi_arg_loc {
            abi::AbiLocation::Reg(ref reg) => {
                let local_slot = stack_layout.local_slot(i);
                asm.spill(*reg, local_slot);
            }
            _ => continue,
        }
    }

    for non_arg_local_idx in sig.params().len() + 1..sig.params().len() + func_body.locals().len() {
        let scratch_reg = reg_alloc.scratch_reg();
        asm.load_imm32(scratch_reg, 0);

        let local_slot = stack_layout.local_slot(non_arg_local_idx);
        asm.spill(scratch_reg, local_slot);
    }

    // TODO: Abstract dead code handling?
    // TODO: Block value passing

    for insn in func_body.code().elements() {
        println!("{:?}", insn);
        match *insn {
            Instruction::Unreachable => {
                asm.trap();
            }
            Instruction::Block(ty) => {
                assert_eq!(
                    ty,
                    ::parity_wasm::elements::BlockType::NoResult,
                    "not implemented yet"
                );
                let end_label = asm.create_label();

                control_frames.push(Control {
                    kind: ControlFrameKind::Block { end_label },
                });

                // Flush stack to ease control flow merging on branching.
                flush_stack(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
            }
            Instruction::Br(depth) => {
                let idx = control_frames.len() - 1 - depth as usize;
                let control_frame = control_frames.get(idx).expect("wrong depth");
                // TODO: value
                asm.jmp(control_frame.kind.br_destination());
            }
            Instruction::BrIf(depth) => {
                // TODO: Should be peek.
                let item = stack.pop().unwrap();
                stack.push(item.clone());
                stack.push(item);

                let r = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);

                let idx = control_frames.len() - 1 - depth as usize;
                let control_frame = control_frames.get(idx).expect("wrong depth");

                let scratch_reg = reg_alloc.scratch_reg();
                asm.load_imm32(scratch_reg, 0);

                // TODO: value

                asm.jmp_i32_conditional(
                    control_frame.kind.br_destination(),
                    r,
                    scratch_reg,
                    IntCC::NotEqual,
                );
                reg_alloc.free_gpr(r);
            }
            Instruction::Return => {
                if let Some(_ret) = sig.return_type() {
                    // TODO: Handle different types.
                    let r = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
                    if A::AbiGen::RETURN_REG != r {
                        asm.mov(A::AbiGen::RETURN_REG, r);
                    }
                    reg_alloc.free_gpr(r);
                }
                asm.jmp(epilogue_label);
            }
            Instruction::End => {
                let control_frame = control_frames.pop().expect("control stack is never empty");

                // If this is not the loop, bind label here.
                if !control_frame.kind.is_loop() {
                    asm.bind(control_frame.kind.br_destination());
                }
            }
            Instruction::GetLocal(idx) => {
                let slot = stack_layout.local_slot(idx as usize);
                let r = reg_alloc.alloc_any_gpr();
                asm.fill(r, slot);
                stack.push(Location::Register(r));
            }
            Instruction::SetLocal(idx) => {
                let r = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
                asm.spill(r, stack_layout.local_slot(idx as usize));
                reg_alloc.free_gpr(r);
            }
            Instruction::I32Const(c) => {
                stack.push(Location::Literal(Literal::I32(c as u32)));
            }
            Instruction::I32Add => {
                // TODO: Handle constant case
                let r2 = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
                let r1d = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);

                asm.add_i32(r1d, r2);

                reg_alloc.free_gpr(r2);
                stack.push(Location::Register(r1d));
            }
            Instruction::I32Eq => {
                let r2 = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
                let r1d = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);

                asm.relop_i32(r1d, r2, IntCC::Equal);

                reg_alloc.free_gpr(r2);
                stack.push(Location::Register(r1d));
            }
            Instruction::I32Ne => {
                let r2 = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);
                let r1d = pop_i32(&mut stack, asm, &mut stack_layout, &mut reg_alloc);

                asm.relop_i32(r1d, r2, IntCC::NotEqual);

                reg_alloc.free_gpr(r2);
                stack.push(Location::Register(r1d));
            }
            Instruction::Drop => {
                if let Location::Register(reg) = stack.pop().unwrap() {
                    reg_alloc.free_gpr(reg);
                }
            }
            _ => unimplemented!("{:?} is not implemented", insn),
        }
    }

    asm.epilogue(stack_layout.highest_slot);
    asm.ret();
}
