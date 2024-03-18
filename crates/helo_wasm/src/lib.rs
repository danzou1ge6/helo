#![feature(fmt_internals)]

use std::borrow::Borrow;
use std::collections::VecDeque;
use std::io::Write;
use std::panic;
use std::sync::Arc;

use helo_driver::*;
use helo_parse::ast;
use helo_parse::source_tree::{SourceFile, SourceTree};
use helo_runtime::byte_code::ToBytes;
use helo_runtime::vm::{self, VmIo};
use helo_runtime::{builtins, errors, executable};
use wasm_bindgen;
use wasm_bindgen::prelude::*;

use miette::{EyreContext, MietteHandler};

extern crate wee_alloc;

// Use `wee_alloc` as the global allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn register_panic_hook() {
    panic::set_hook(Box::new(console_error_panic_hook::hook))
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct JsOutput {
    write: js_sys::Function,
    flush: js_sys::Function,
}

#[wasm_bindgen]
impl JsOutput {
    pub fn new(write: js_sys::Function, flush: js_sys::Function) -> Self {
        Self { write, flush }
    }
}

impl std::fmt::Write for JsOutput {
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        let _ = self.write(&c.to_bytes());
        Ok(())
    }
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let _ = self.write(s.as_bytes());
        Ok(())
    }
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        let _ = self.write(args.to_string().as_bytes());
        Ok(())
    }
}

impl std::io::Write for JsOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let js_val = JsValue::from(String::from_utf8_lossy(buf).to_string());
        let this = JsValue::null();

        let _ = self.write.call1(&this, &js_val);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        let _ = self.flush.call0(&JsValue::null());
        Ok(())
    }
}

pub struct JsInput {
    buf: VecDeque<u8>,
    eof: bool,
}

impl JsInput {
    pub fn ready(&self) -> bool {
        !self.buf.is_empty()
    }
}

impl JsInput {
    pub fn new(input: &[u8]) -> Self {
        Self {
            buf: VecDeque::from(input.to_vec()),
            eof: false,
        }
    }
}

impl std::io::Read for JsInput {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.eof {
            return Ok(0);
        }

        let len = buf.len().min(self.buf.len());
        for (_, r) in (0..len).zip(buf.iter_mut()) {
            *r = self.buf.pop_front().unwrap();
        }
        Ok(len)
    }
}

impl std::io::BufRead for JsInput {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        self.buf.make_contiguous();
        let (b, _) = self.buf.as_slices();
        Ok(b)
    }
    fn consume(&mut self, amt: usize) {
        for _ in 0..amt {
            self.buf.pop_front();
        }
    }
}

#[wasm_bindgen]
pub struct JsIo {
    input: JsInput,
    output: JsOutput,
}

impl VmIo for JsIo {
    type Input = JsInput;
    type Output = JsOutput;
    fn input(&mut self) -> &mut Self::Input {
        &mut self.input
    }
    fn input_ready(&self) -> bool {
        self.input.ready()
    }
    fn output(&mut self) -> &mut Self::Output {
        &mut self.output
    }
}

#[wasm_bindgen]
impl JsIo {
    pub fn new(write: js_sys::Function, flush: js_sys::Function, input: String) -> Self {
        Self {
            input: JsInput::new(input.as_bytes()),
            output: JsOutput::new(write, flush),
        }
    }
    pub fn write(&mut self, data: &str) {
        self.input.buf.extend(data.as_bytes().iter());
    }
    pub fn write_eof(&mut self) {
        self.input.eof = true;
    }
}

#[wasm_bindgen]
pub struct Compiler {
    print_stages: Vec<Stage>,
    term_width: usize,
    produce_exe: bool,
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct Src {
    tree: SourceTree,
}

#[wasm_bindgen]
impl Src {
    pub fn new() -> Self {
        Self {
            tree: SourceTree::new_empty(),
        }
    }
    pub fn insert(&mut self, parent: Vec<String>, file_name: String, src: String) -> bool {
        let mut path_string = String::new();
        for item in parent.iter() {
            path_string.push('/');
            path_string.push_str(item);
        }
        path_string.push('/');
        path_string.push_str(&file_name);
        path_string.push_str(".helo");

        let file = SourceFile {
            src: Arc::new(src),
            file_name: file_name.to_string(),
            file_path: Arc::new(path_string),
        };

        let parent = parent.iter().map(|x| &x[..]).collect();
        self.tree
            .insert(ast::Path(parent).into_iter(), file)
            .is_ok()
    }
    pub fn set_root_file_name(&mut self, name: String) {
        self.tree.set_root_file_name(name)
    }
}

#[wasm_bindgen]
pub struct Executable(executable::Executable);

fn write_miette_report(report: miette::Report, output: &mut JsOutput) {
    let handler = MietteHandler::new();
    let mut fmt = std::fmt::Formatter::new(output);
    let _ = handler.debug(report.borrow(), &mut fmt);
}

#[wasm_bindgen]
impl Compiler {
    pub fn compile(&self, mut output: JsOutput, src: Src) -> Option<Executable> {
        let controller = SingleOutController {
            produce_exe: self.produce_exe,
            print_stages: self.print_stages.clone(),
            term_width: self.term_width,
            out: output.clone(),
            print_all: false,
        };
        match compile(&src.tree, controller) {
            Ok(r) => r.map(|x| Executable(x)),
            Err(e) => {
                write_miette_report(e, &mut output);
                None
            }
        }
    }
    pub fn new(print_stages: Vec<Stage>, produce_exe: bool, term_width: usize) -> Self {
        Self {
            print_stages,
            term_width,
            produce_exe,
        }
    }
}

#[wasm_bindgen]
pub struct Vm {
    st: vm::VmState,
    builtin_table: builtins::BuiltinTable<JsIo>,
    exe: executable::Executable,
}

enum ExecutionResultPayload {
    Returned(String),
    Hung(Vm),
    Error(String),
    Panic(Panic),
}

#[wasm_bindgen]
pub struct Panic(errors::Panic);

#[wasm_bindgen]
impl Panic {
    pub fn print_to(&self, mut output: JsOutput, src: Src) {
        if let Some(file) = src.tree.search(&self.0.file) {
            let report = miette::miette!(
                labels = vec![miette::LabeledSpan::at(self.0.span, &self.0.msg)],
                "Program Panicked"
            )
            .with_source_code(file.src);
            write_miette_report(report, &mut output);
        } else {
            let _ = write!(&mut output, "{}", self.0);
        }
    }
}

#[wasm_bindgen]
pub enum ExecutionResultType {
    Returned,
    Hung,
    Exception,
    Panic,
}

#[wasm_bindgen]
pub struct ExecutionResult {
    payload: ExecutionResultPayload,
}

#[wasm_bindgen]
impl ExecutionResult {
    pub fn type_(&self) -> ExecutionResultType {
        use ExecutionResultPayload::*;
        match &self.payload {
            Returned(..) => ExecutionResultType::Returned,
            Hung(..) => ExecutionResultType::Hung,
            Error(..) => ExecutionResultType::Exception,
            Panic(..) => ExecutionResultType::Panic,
        }
    }
    pub fn unwrap_str(self) -> String {
        use ExecutionResultPayload::*;
        match self.payload {
            Returned(s) | Error(s) => s,
            _ => panic!("`unwrap_str` on other value"),
        }
    }
    pub fn unwrap_vm(self) -> Vm {
        use ExecutionResultPayload::*;
        match self.payload {
            Hung(v) => v,
            _ => panic!("`unwrap_str` on other value"),
        }
    }
    pub fn unwrap_panic(self) -> Panic {
        match self.payload {
            ExecutionResultPayload::Panic(p) => p,
            _ => panic!("`unwrap_panic` on other value"),
        }
    }
}

#[wasm_bindgen]
impl Vm {
    pub fn new(exe: Executable) -> Self {
        let st = vm::VmState::new(&exe.0);
        Self {
            exe: exe.0,
            st,
            builtin_table: builtins::BuiltinTable::new_async(),
        }
    }
    pub fn run(self, mut io: JsIo) -> ExecutionResult {
        let Vm {
            st,
            builtin_table,
            exe,
        } = self;
        let gc_policy = vm::IncreasingGcPolicy::new(2, 1024 * 1024 * 50);
        let r = st.run(&exe, &mut io, &builtin_table, gc_policy);

        match r {
            Ok((pack, mut lock)) => {
                let (mut pool, val) = pack.unpack(&lock);
                let val = format!("{:?}", val);
                pool.clear(&mut lock);
                ExecutionResult {
                    payload: ExecutionResultPayload::Returned(val),
                }
            }
            Err(errors::Exception::Hang(st)) => ExecutionResult {
                payload: ExecutionResultPayload::Hung(Vm {
                    st,
                    builtin_table,
                    exe,
                }),
            },
            Err(errors::RunTimeError_::Panic(panic, _)) => ExecutionResult {
                payload: ExecutionResultPayload::Panic(Panic(panic)),
            },
            Err(other) => ExecutionResult {
                payload: ExecutionResultPayload::Error(other.to_string()),
            },
        }
    }
}
