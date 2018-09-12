#![allow(dead_code)]

extern crate byteorder;
extern crate capstone;
extern crate libc;
extern crate parity_wasm;
extern crate x86asm;

#[macro_use]
extern crate lazy_static;

extern crate wabt;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;

mod codegen;
mod exec;

use std::env;
use std::fs;
use std::io::Read;

use codegen::compile_module;
use exec::{link_module, LinkedModule};
use parity_wasm::deserialize_buffer;
use parity_wasm::elements::Module;

fn compile_and_link_wat(wat: &str) -> LinkedModule {
    let wasm = wabt::wat2wasm(wat).unwrap();
    let module: Module = deserialize_buffer(&wasm).unwrap();
    let compiled = compile_module(&module);
    println!("disasm: {}", compiled.disasm());
    link_module(compiled)
}

fn main() {
    let filename = env::args().nth(1).unwrap();
    let mut file = fs::File::open(filename).unwrap();

    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    compile_and_link_wat(&contents);
}

#[cfg(test)]
mod tests {
    use super::compile_and_link_wat;

    #[test]
    fn empty_executes_fine() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    return
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Ok(_));
    }

    #[test]
    fn unreachable_traps() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    unreachable
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Err(_));
    }

    #[test]
    fn br_works() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    (block
                        br 0
                        unreachable
                    )
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Ok(_));
    }

    #[test]
    fn br_if_jumps_if_true() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    (block
                        i32.const 2
                        br_if 0
                        unreachable
                    )
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Ok(_));
    }

    #[test]
    fn br_if_doesnt_jump_if_false() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    (block
                        i32.const 0
                        br_if 0
                        unreachable
                    )
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Err(_));
    }

    #[test]
    fn add() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call")
                    (local i32 i32)

                    (set_local 0
                        (i32.const 5)
                    )
                    (set_local 1
                        (i32.const 3)
                    )

                    block
                        (i32.eq
                            (i32.add
                                (get_local 0)
                                (get_local 1)
                            )
                            (i32.const 8)
                        )

                        br_if 0
                        unreachable
                    end
                )
            )
        "#,
        );

        assert_matches!(m.invoke_func(0), Ok(_));
    }

    #[test]
    fn args() {
        let m = compile_and_link_wat(
            r#"
            (module
                (func (export "call") (param i32 i32) (result i32)
                    (i32.add
                        (get_local 0)
                        (get_local 1)
                    )
                    return
                )
            )
        "#,
        );

        assert_matches!(m.invoke::<(u32), _>(0, (5u32, 3u32)), Ok(8u32));
        assert_matches!(m.invoke::<(u32), _>(0, (0xFFFFFFFFu32, 1u32)), Ok(0u32));
        assert_matches!(m.invoke::<(u32), _>(0, (0xFFFFFFFFu32, 2u32)), Ok(1u32));
    }
}
