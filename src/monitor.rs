mod imix;
mod cache;
mod branch;

use std::collections::HashMap;
use std::io::Error;
use std::path::{Path, PathBuf};
use orca_wasm::ir::id::GlobalID;
use orca_wasm::ir::types::{InitExpr, Value};
use orca_wasm::{DataType, Instructions, Module};
use orca_wasm::module_builder::AddLocal;

pub enum Monitor {
    IMix,
    Cache,
    Branch
}

impl Monitor {
    fn name(&self) -> &str {
        match self {
            Monitor::IMix => "imix",
            Monitor::Cache => "cache",
            Monitor::Branch => "branch"
        }
    }
}

/// Adds monitor instrumentation bytecode to an existing
/// WASM module.
pub fn add_monitor(module: Module, monitor: Monitor, path: &Path) -> Result<(), Error> {
    let instrumented_module = match monitor {
        Monitor::IMix => imix::instrument(module),
        Monitor::Cache => cache::instrument(module),
        Monitor::Branch => branch::instrument(module),
    };

    write_module(instrumented_module, monitor.name(), path)
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


pub fn add_global(wasm: &mut Module) -> GlobalID {
    wasm.add_global(
        InitExpr::new(vec![Instructions::Value(Value::I32(0))]),
        DataType::I32,
        true,
        false,
    )
}

#[derive(Default)]
pub struct LocalsTracker {
    available: HashMap<DataType, Vec<u32>>,
    in_use: HashMap<DataType, Vec<u32>>,
}
impl LocalsTracker {
    pub fn use_local<T: AddLocal>(&mut self, ty: DataType, injector: &mut T) -> u32 {
        let id = if let Some(list) = self.available.get_mut(&ty) {
            if let Some(id) = list.pop() {
                id
            } else {
                *injector.add_local(ty)
            }
        } else {
            *injector.add_local(ty)
        };

        self.add_in_use(ty, id);
        id
    }
    fn add_in_use(&mut self, ty: DataType, id: u32) {
        self.in_use
            .entry(ty)
            .and_modify(|list| {
                // insert at the beginning so that lower IDs are at the top
                // (for `extend` to keep them there)
                list.insert(0, id);
            })
            .or_insert(vec![id]);
    }
    pub fn add(&mut self, ty: DataType, id: u32) {
        self.available
            .entry(ty)
            .and_modify(|list| {
                // insert at the beginning so that lower IDs are at the top
                // (for `pop`)
                list.insert(0, id);
            })
            .or_insert(vec![id]);
    }
    pub fn reset_probe(&mut self) {
        self.available.extend(self.in_use.to_owned());
        self.in_use.clear();
    }
    pub fn reset_function(&mut self) {
        self.available.clear();
        self.in_use.clear();
    }
}