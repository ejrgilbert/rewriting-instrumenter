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
        "imix" => Monitor::IMix,
        "cache-sim" => Monitor::Cache,
        "branches" => Monitor::Branch,
        "hotness" => Monitor::Hotness,
        "mem-access" => Monitor::MemAccess,
        "loop-tracer" => Monitor::LoopTracer,
        "call-graph" => Monitor::CallGraph,
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
