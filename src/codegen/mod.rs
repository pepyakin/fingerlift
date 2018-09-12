use parity_wasm::elements::{Module, ValueType, FunctionType, Type};

mod asm;
mod func;

use self::asm::Asm;

// TODO: relocations
pub struct CompiledModule {
    pub funcs: Vec<Vec<u8>>,
    pub signatures: Vec<FunctionType>,
}

impl CompiledModule {
    pub fn disasm(&self) -> String {
        use capstone::prelude::*;
        use std::fmt::Write;

        let mut capstone = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .build()
            .unwrap();

        let mut out = String::new();

        for (i, func_code) in self.funcs.iter().enumerate() {
            writeln!(out, "func[{}]:", i);

            let insns = capstone.disasm_all(func_code, 0x0).unwrap();
            for i in insns.iter() {
                writeln!(out, "{}", i);
            }
        }
        
        out
    }
}

struct PwasmModuleEnv<'a> {
    module: &'a Module,
}

impl<'a> func::ModuleEnv for PwasmModuleEnv<'a> {
    fn global_ty(&self, _idx: usize) -> ValueType {
        unimplemented!()
    }
}

pub fn compile_module(module: &Module) -> CompiledModule {
    // TODO: Remove unwraps. Use entries()/types()
    let function_section = module
        .function_section()
        .unwrap();
    let code_section = module
        .code_section()
        .unwrap();
    let type_section = module
        .type_section()
        .unwrap();

    let mut compiled_funcs = Vec::new();
    let mut signatures = Vec::new();
    for (index, function) in function_section.entries().iter().enumerate() {
        let func_body = code_section.bodies().get(index as usize).unwrap();
        let env = PwasmModuleEnv { module: &module };
        let mut asm = self::asm::x64::X64Asm::new();

        let Type::Function(ref sig) = &type_section.types()[function.type_ref() as usize];
        signatures.push(sig.clone());

        func::compile_func(&env, &mut asm, sig, func_body);

        let compiled_func = asm.finalize();
        compiled_funcs.push(compiled_func);
    }

    CompiledModule {
        funcs: compiled_funcs,
        signatures,
    }
}
