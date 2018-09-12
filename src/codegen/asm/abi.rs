use super::Reg;

pub enum AbiType {
    I32,
    I64,
    F32,
    F64,
}

pub enum AbiLocation {
    Reg(Reg),
    Stack(i32),
}

pub trait AbiGen {
    // TODO: This really should be a function
    const RETURN_REG: Reg;
    fn abi_args(args: &[AbiType]) -> Vec<AbiLocation>;
}
