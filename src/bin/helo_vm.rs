use helo_runtime::{builtins, errors, executable, vm};

use std::env;
use std::fs;
use std::io::Read;
use std::path;

use executable::Executable;

pub fn load_executable(file: &path::PathBuf) -> std::io::Result<Executable> {
    let mut handle = fs::File::open(file)?;
    Executable::read_from(&mut handle)
}

pub fn run<G>(vm: vm::VmState, exe: &Executable, g: G)
where
    G: vm::GcPolicy,
{
    let builtin_table = builtins::BuiltinTable::new_sync();
    let mut io = vm::SyncIo::new_std();
    let r = vm.run(exe, &mut io, &builtin_table, g);

    match r {
        Ok((pack, mut lock)) => {
            let (mut pool, val) = pack.unpack(&lock);
            println!("Return value = {:?}", val);
            pool.clear(&mut lock);
        }
        Err(e) => match e {
            errors::Exception::Panic { file, span, msg , ..} => {
                print_panic(file, span, msg);
            }
            _ => eprintln!("{}", e),
        },
    }
}

fn print_panic(file: String, span: (usize, usize), msg: String) {
    if let Ok(mut file) = fs::File::open(&file) {
        let mut src = String::new();
        if let Ok(_) = file.read_to_string(&mut src) {
            let report = miette::miette!(
                labels = vec![miette::LabeledSpan::at(span, msg)],
                "Program Panicked"
            )
            .with_source_code(src);
            eprintln!("{:?}", report);
        }
    } else {
        eprintln!(
            "In file {}, characters {} - {}: {}",
            file, span.0, span.1, msg
        );
    }
}
pub fn main() {
    let args: Vec<_> = env::args().collect();
    let file_name = args
        .get(1)
        .unwrap_or_else(|| panic!("Usage: helo_vm <file_name>"))
        .clone();
    match load_executable(&path::PathBuf::from(file_name)) {
        Ok(exe) => {
            let gc_policy = vm::IncreasingGcPolicy::new(2, 1024 * 1024 * 50);
            let vm_state = vm::VmState::new(&exe);
            run(vm_state, &exe, gc_policy);
        }
        Err(e) => eprintln!("{e}"),
    }
}
