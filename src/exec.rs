use codegen::CompiledModule;
use libc::{self, c_int, c_void, SIGILL};
use parity_wasm::elements::{FunctionType, ValueType};
use std::marker::PhantomData;
use std::sync::Mutex;
use std::{mem, ptr};

// Global setjmp buffer + signal handler is not thread-safe.
// TODO: Lift this restriction.
lazy_static! {
    static ref LOCK: Mutex<()> = Mutex::new(());
}

// TODO: why 27?
static mut SETJMP_BUF: [c_int; 27] = [0; 27];

extern "C" {
    fn setjmp(env: *mut c_void) -> c_int;
    fn longjmp(env: *mut c_void, val: c_int);
}

extern "C" fn catch_sigill(signum: c_int, info: *mut libc::siginfo_t, ptr: *mut libc::c_void) {
    // TODO: Prints are not signal safe.
    println!("ptr = {:?}", ptr);
    println!("info = {:?}", info);

    unsafe {
        println!("addr = {:?}", (*info).si_addr);

        // Return control back to the calling code.
        longjmp((&mut SETJMP_BUF).as_mut_ptr() as *mut c_void, signum);
    }
}

/// This function is unsafe to call since it uses setjmp/longjmp for catching signals that
/// produced in `f`.
unsafe fn exec<R, F: FnOnce() -> R>(f: F) -> Result<R, String> {
    let _lock = LOCK.lock();

    let mut new: libc::sigaction = mem::zeroed();
    new.sa_sigaction = catch_sigill as usize;
    libc::sigemptyset(&mut new.sa_mask as *mut _);
    new.sa_flags = libc::SA_NODEFER;

    let mut prev: libc::sigaction = mem::zeroed();

    if libc::sigaction(SIGILL, &new, &mut prev) != 0 {
        panic!("failed to set signal!")
    }

    let setjmp_result = setjmp((&mut SETJMP_BUF[..]).as_mut_ptr() as *mut c_void);
    if setjmp_result == 0 {
        return Ok(f());
    } else {
        return Err(format!("Catched signal: {}", setjmp_result));
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct UntypedFnPtr(usize);

struct FuncInstance {
    func_ptr: UntypedFnPtr,
    signature: FunctionType,
}

// TODO: free allocated memory on drop
// TODO: extract instantiation.
pub struct LinkedModule {
    funcs: Vec<FuncInstance>,
    executable_mem: *mut u8,
}

pub trait WasmReturnType {
    // None means unit/void
    const WASM_TYPE: Option<ValueType>;
}

impl WasmReturnType for () {
    const WASM_TYPE: Option<ValueType> = None;
}

impl WasmReturnType for u32 {
    const WASM_TYPE: Option<ValueType> = Some(ValueType::I32);
}

pub trait WasmParamType {
    const WASM_TYPE: ValueType;
}

impl WasmParamType for u32 {
    const WASM_TYPE: ValueType = ValueType::I32;
}

pub trait WasmParams {
    const WASM_TYPES: &'static [ValueType];
}

impl WasmParams for () {
    const WASM_TYPES: &'static [ValueType] = &[];
}

impl<A: WasmParamType> WasmParams for (A,) {
    const WASM_TYPES: &'static [ValueType] = &[A::WASM_TYPE];
}

impl<A: WasmParamType, B: WasmParamType> WasmParams for (A, B) {
    const WASM_TYPES: &'static [ValueType] = &[A::WASM_TYPE, B::WASM_TYPE];
}

#[inline]
fn signature_matches<R: WasmReturnType, P: WasmParams>(sig: &FunctionType) -> bool {
    sig.params() == P::WASM_TYPES && sig.return_type() == R::WASM_TYPE
}

pub struct FnWrapper<R: WasmReturnType, P: WasmParams>(PhantomData<(R, P)>);

pub unsafe trait FuncWrapper<R: WasmReturnType + Sized, P: WasmParams + Sized> {
    unsafe fn call(fn_ptr: UntypedFnPtr, args: P) -> R;
}

unsafe impl<R: WasmReturnType> FuncWrapper<R, ()> for FnWrapper<R, ()> {
    unsafe fn call(fn_ptr: UntypedFnPtr, args: ()) -> R {
        let () = args;
        let f = mem::transmute::<usize, extern "C" fn() -> R>(fn_ptr.0);
        f()
    }
}

unsafe impl<R: WasmReturnType, A: WasmParamType> FuncWrapper<R, (A,)> for FnWrapper<R, (A,)> {
    unsafe fn call(fn_ptr: UntypedFnPtr, args: (A,)) -> R {
        let (a1,) = args;
        let f = mem::transmute::<usize, extern "C" fn(A) -> R>(fn_ptr.0);
        f(a1)
    }
}

unsafe impl<R: WasmReturnType, A: WasmParamType, B: WasmParamType> FuncWrapper<R, (A, B)>
    for FnWrapper<R, (A, B)>
{
    unsafe fn call(fn_ptr: UntypedFnPtr, args: (A, B)) -> R {
        let (a1, a2) = args;
        let f = mem::transmute::<usize, extern "C" fn(A, B) -> R>(fn_ptr.0);
        f(a1, a2)
    }
}

impl LinkedModule {
    pub fn invoke_func(&self, idx: usize) -> Result<(), String> {
        self.invoke::<(), ()>(idx, ())
    }

    pub fn invoke<R: WasmReturnType + Sized, P: WasmParams + Sized>(
        &self,
        idx: usize,
        args: P,
    ) -> Result<R, String>
    where
        FnWrapper<R, P>: FuncWrapper<R, P>,
    {
        let func = self.funcs.get(idx).expect("invalid index");

        {
            if !signature_matches::<R, P>(&func.signature) {
                return Err("Signature mismatch!".to_string());
            }
        }

        unsafe {
            exec(|| <FnWrapper<R, P> as FuncWrapper<R, P>>::call(func.func_ptr, args))
        }
    }
}

pub fn link_module(compiled: CompiledModule) -> LinkedModule {
    const PAGE_SIZE: usize = 4096;

    let total_size = compiled.funcs.iter().map(|f| f.len()).sum();

    unsafe {
        // TODO: Don't allocate memory if there is no functions.

        let mut contents: *mut libc::c_void = mem::uninitialized();
        if libc::posix_memalign(&mut contents, PAGE_SIZE, total_size) != 0 {
            panic!("failed to allocate memory for jitted code");
        }
        libc::mprotect(
            contents,
            total_size,
            libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
        );
        let contents = contents as *mut u8;

        ptr::write_bytes(contents, 0xc3, total_size);

        let func_bodies = compiled.funcs;
        let func_sigs = compiled.signatures;

        let mut offset = 0;
        let mut funcs = Vec::new();
        for (func_code, func_sig) in func_bodies.into_iter().zip(func_sigs) {
            let func_start = contents.offset(offset as isize);
            println!("copying {} bytes to {:?} (+{})", func_code.len(), func_start, offset);
            ptr::copy_nonoverlapping(func_code.as_ptr(), func_start as *mut u8, func_code.len());

            funcs.push(FuncInstance {
                func_ptr: UntypedFnPtr(func_start as usize),
                signature: func_sig,
            });
            offset += func_code.len();
        }

        LinkedModule {
            funcs,
            executable_mem: contents,
        }
    }
}
