use std::fs;
use std::io::stdout;

use helo_driver::*;
use helo_parse::source_tree;
use helo_runtime::{builtins, errors as vm_errors, executable::Executable, vm};

use clap::Parser;
use miette::IntoDiagnostic;
use terminal_size::{terminal_size, Width};

#[derive(Parser)]
#[command(name = "heloc", about = "Compiler for Helo Lang")]
struct Cli {
    #[arg(short, long, default_value = "out.heloc")]
    /// Path to compiled byte code
    output: String,
    #[arg(short, long)]
    /// Specify which intermediate result to print
    prints: Vec<Stage>,
    #[arg(long)]
    /// Print all intermediate result
    print_all: bool,
    #[arg(short, long)]
    /// Do not save emitted byte code to file
    no_save: bool,
    #[arg(short, long)]
    /// Run immediately after compilation
    run: bool,
    /// Path to source file
    input: String,
}

pub fn run_with_source_tree<'e, G>(
    vm: vm::VmState,
    exe: &Executable,
    g: G,
    src_tree: &source_tree::SourceTree,
) where
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
            vm_errors::Exception::Panic(panic, _) => {
                print_panic_with_source_tree(panic, src_tree);
            }
            _ => eprintln!("{}", e),
        },
    }
}

fn print_panic_with_source_tree(panic: vm_errors::Panic, src_tree: &source_tree::SourceTree) {
    if let Some(file) = src_tree.search(&panic.file) {
        let report = miette::miette!(
            labels = vec![miette::LabeledSpan::at(panic.span, panic.msg)],
            "Program Panicked"
        )
        .with_source_code(file.src);
        eprintln!("{:?}", report);
    } else {
        eprintln!("{panic}");
    }
}

pub fn main() -> miette::Result<()> {
    let cli = Cli::parse();
    let file_path = std::path::PathBuf::from(cli.input);

    let term_width = if let Some((Width(w), _)) = terminal_size() {
        w
    } else {
        80
    };

    let config = SingleOutController {
        produce_exe: !cli.no_save || cli.run,
        print_stages: cli.prints,
        print_all: cli.print_all,
        term_width: term_width as usize,
        out: stdout(),
    };

    let src_tree = helo_parse::source_tree::SourceTree::new(file_path).into_diagnostic()?;

    if let Some(exe) = compile(&src_tree, config)? {
        if !cli.no_save {
            let mut f = fs::File::create(cli.output).into_diagnostic()?;
            exe.write_to(&mut f).into_diagnostic()?;
        }

        if cli.run {
            let gc_policy = vm::IncreasingGcPolicy::new(2, 1024 * 1024 * 50);
            let vm_state = vm::VmState::new(&exe);
            run_with_source_tree(vm_state, &exe, gc_policy, &src_tree);
        }
    }

    Ok(())
}
