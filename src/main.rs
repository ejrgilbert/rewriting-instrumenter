use anyhow::bail;
use rewriting_monitor::monitor::{add_monitor, Monitor};
use std::{env, fs, path::Path};
use wirm::Module;

fn main() -> Result<(), anyhow::Error> {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() != 2 {
        bail!("Usage: ./rewrite <monitor> <filename>");
    }

    let path = Path::new(&args[1]);
    if !path.exists() {
        bail!("File does not exist");
    }

    let monitor: Monitor = match &args[0][..] {
        "branches" => Monitor::Branch,
        "hotness" => Monitor::Hotness,
        "icount" => Monitor::ICount,
        "imix" => Monitor::IMix,
        "cache-sim" => Monitor::Cache,
        "mem-access" => Monitor::MemAccess,
        "loop-tracer" => Monitor::LoopTracer,
        "basic-blocks" => Monitor::BasicBlocks,
        "call-graph" => Monitor::CallGraph,
        "instr-coverage" => Monitor::CoverageInstr,
        name => bail!("Invalid monitor {}", name),
    };

    let wasm = fs::read(path).unwrap();
    let module = match Module::parse(&wasm, false) {
        Ok(module) => module,
        _ => bail!("Unable to parse module {:?}", path),
    };

    if let Err(e) = add_monitor(module, monitor, path) {
        panic!("Something went wrong: {}", e)
    } else {
        Ok(())
    }
}
