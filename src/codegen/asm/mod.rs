pub mod x64;
pub mod abi;

/// Platform independent register.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Reg {
    pub idx: u32,
}

impl Reg {
    pub fn from_index(idx: u32) -> Reg {
        Reg {
            idx,
        }
    }
}

/// Set of platform independent registers.
#[derive(Clone, Debug)]
pub struct RegSet(u32);

impl RegSet {
    pub fn empty() -> RegSet {
        RegSet(0)
    }
    
    /// Create a signleton set from the given register.
    pub fn from_single(reg: Reg) -> RegSet {
        let set = 1 << reg.idx;
        RegSet(set)
    }

    /// Returns `true` if this set has the specified register.
    pub fn has(&self, reg: Reg) -> bool {
        (self.0 & RegSet::from_single(reg).0) != 0
    }

    /// Add given register into this set.
    pub fn insert(&mut self, reg: Reg) {
        self.0 |= RegSet::from_single(reg).0;
    }

    pub fn remove(&mut self, reg: Reg) {
        self.0 &= !RegSet::from_single(reg).0;
    }

    /// Remove all registers contained in `other` from self.
    pub fn remove_all(&mut self, other: &RegSet) {
        self.0 = self.0 & !other.0;
    }

    /// Returns `true` if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn first(&self) -> Option<Reg> {
        if self.is_empty() {
            None
        } else {
            let index = self.0.trailing_zeros();
            Some(Reg::from_index(index))
        }
    }

    pub fn last(&self) -> Option<Reg> {
        if self.is_empty() {
            None
        } else {
            let index = 32 - self.0.leading_zeros() - 1; 
            Some(Reg::from_index(index))
        }
    }
}

/// A label
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label(u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntCC {
    Equal,
    NotEqual,
}

/// Platform independent interface for generating assembly code.
pub trait Asm {
    type AbiGen: abi::AbiGen;

    fn available_regs() -> RegSet;

    /// Emit trap instruction.
    fn trap(&mut self);

    /// Return to the parent frame.
    fn ret(&mut self);

    /// Jump to the specified label.
    ///
    /// The label can be unbound for the moment.
    fn jmp(&mut self, target: Label);

    /// Jump to the speicified label if the relation `cc` holds
    /// between `r1` and `r2`.
    fn jmp_i32_conditional(&mut self, target: Label, r1: Reg, r2: Reg, cc: IntCC);

    fn mov(&mut self, dst: Reg, src: Reg);

    /// Load a 32-bit constant into `dst` register.
    fn load_imm32(&mut self, dst: Reg, imm: u32);

    // TODO: Rename this to stack_save, stack_load
    // TODO: Follow intel 

    fn spill(&mut self, src: Reg, slot: i32);

    fn fill(&mut self, dst: Reg, slot: i32);

    /// Sum `r1d` and `r2` and put result into `r1d`.
    fn add_i32(&mut self, r1d: Reg, r2: Reg);

    // Compare `r1d` with `r2`, and if relation in `cc` holds, then
    // put `1` in `r1d`. Otherwise, put `0` in `r1d`.
    fn relop_i32(&mut self, r1d: Reg, r2: Reg, cc: IntCC);

    /// Create a new label that can be bound later.
    fn create_label(&mut self) -> Label;

    /// Bind the given label to the current position.
    fn bind(&mut self, label: Label);

    fn prologue(&mut self);
    fn epilogue(&mut self, highest_slot: i32);

    fn finalize(self) -> Vec<u8>;
}
