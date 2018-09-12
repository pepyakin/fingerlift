(module
    (func $simple_add (param i32 i32)
        (local i32)

        get_local 0
        get_local 1
        i32.add
        set_local 2    
   )

   (func $block (param i32 i32)
        (local i32)

        get_local 0
        get_local 1
        block
            get_local 0
            br_if 0
        end
        i32.add
        set_local 2
   )

    (func $simple_eq (param i32 i32)
        (local i32)

        (set_local 2
            (i32.eq
                (get_local 0)
                (get_local 1)
            )
        )
    )

    (func (export "call")
        block
            (i32.eq
                (i32.add
                    (i32.const 5)
                    (i32.const 3)
                )
                (i32.const 8)
            )

            br_if 0
            unreachable
        end
    )
)
