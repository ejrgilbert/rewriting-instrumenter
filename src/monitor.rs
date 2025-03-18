mod imix;
mod cache;

use std::io::Error;
use std::path::{Path, PathBuf};
use orca_wasm::Module;

pub enum Monitor {
    IMix,
    Cache
}

impl Monitor {
    fn name(&self) -> &str {
        match self {
            Monitor::IMix => "imix",
            Monitor::Cache => "cache"
        }
    }
}

const MEMREGION: &str = "instrument";
const MEMUNIT: usize = 64;

/// Adds monitor instrumentation bytecode to an existing
/// WASM module.
pub fn add_monitor(module: Module, monitor: Monitor, path: &Path) -> Result<(), Error> {
    let instrumented_module = match monitor {
        Monitor::IMix => imix::instrument(module),
        Monitor::Cache => cache::instrument(module),
    };

    write_module(instrumented_module, &monitor.name(), path)
}

/// Writes the WASM module to the given path adding
/// monitor name to the file name.
fn write_module(mut module: Module, monitor_name: &str, path: &Path) -> Result<(), Error> {
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let extension = path.extension().unwrap().to_str().unwrap();
    let new_file_stem = format!("{}-{}", file_stem, monitor_name);
    let new_file_name = PathBuf::from(format!("{}.{}", new_file_stem, extension));

    module.emit_wasm(new_file_name.to_str().unwrap())
}