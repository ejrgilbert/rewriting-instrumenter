use std::{env, fs, path::Path};
use anyhow::bail;
use orca_wasm::Module;
use rewrite::monitor::{add_monitor, Monitor};

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
        "cache" => Monitor::Cache,
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