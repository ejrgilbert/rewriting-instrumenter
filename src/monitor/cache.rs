use std::collections::HashMap;
use orca_wasm::ir::id::{FunctionID, GlobalID, LocalID};
use orca_wasm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use orca_wasm::iterator::module_iterator::ModuleIterator;
use orca_wasm::{DataType, Module, Opcode};
use orca_wasm::DataType::{F32, F64, I32, I64};
use orca_wasm::module_builder::AddLocal;
use orca_wasm::opcode::MacroOpcode;
use wasmparser::{MemArg, Operator};
use crate::monitor::add_global;

pub fn instrument(mut wasm: Module) -> Module {
    let globals = Globals::new(&mut wasm);
    let mut locals = LocalsTracker::default();
    let check_access = import_lib(&mut wasm);
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &mut locals, &globals, check_access);

    wasm
}

fn inject_instrumentation(wasm: &mut ModuleIterator, locals: &mut LocalsTracker, globals: &Globals, check_access: FunctionID) {
    loop {
        if let Some(op) = wasm.curr_op() {
            if let Some((val_dt, offset, num_bytes)) = match op {
                Operator::I32Load8S { memarg: MemArg {offset, .. }, .. } |
                Operator::I32Load8U { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load8S { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load8U { memarg: MemArg {offset, .. }, .. } => {
                    Some((None, *offset, 1))
                }
                Operator::I32Load16S { memarg: MemArg {offset, .. }, .. } |
                Operator::I32Load16U { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load16S { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load16U { memarg: MemArg {offset, .. }, .. } => {
                    Some((None, *offset, 2))
                },
                Operator::I32Load { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load32U { memarg: MemArg {offset, .. }, .. } |
                Operator::I64Load32S { memarg: MemArg {offset, .. }, .. } |
                Operator::F32Load { memarg: MemArg {offset, .. }, .. } => {
                    Some((None, *offset, 4))
                }
                Operator::I64Load { memarg: MemArg {offset, .. }, .. } |
                Operator::F64Load { memarg: MemArg {offset, .. }, .. } => {
                    Some((None, *offset, 8))
                }
                Operator::I32Store8 { memarg: MemArg {offset, .. }, .. } => Some((Some(I32), *offset, 1)),
                Operator::I64Store8 { memarg: MemArg {offset, .. }, .. } => Some((Some(I64), *offset, 1)),
                Operator::I32Store16 { memarg: MemArg {offset, .. }, .. } => Some((Some(I32), *offset, 2)),
                Operator::I64Store16 { memarg: MemArg {offset, .. }, .. } => Some((Some(I64), *offset, 2)),
                Operator::I32Store { memarg: MemArg {offset, .. }, .. } => Some((Some(I32), *offset, 4)),
                Operator::F32Store { memarg: MemArg {offset, .. }, .. } => Some((Some(F32), *offset, 4)),
                Operator::I64Store32 { memarg: MemArg {offset, .. }, .. } => Some((Some(I64), *offset, 4)),
                Operator::I64Store { memarg: MemArg {offset, .. }, .. } => Some((Some(I64), *offset, 8)),
                Operator::F64Store { memarg: MemArg {offset, .. }, .. } => Some((Some(F64), *offset, 8)),
                _ => None,
            } {
                perform_cache_lookup(val_dt, offset, num_bytes, wasm, locals, globals, check_access);
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn import_lib(wasm: &mut Module) -> FunctionID {
    let lib_name = "cache";
    let lib_func = "check_access";

    let ty_id = wasm.types.add_func_type(&[I32, I32], &[I32]);
    let (fid, imp_id) = wasm.add_import_func(lib_name.to_string(), lib_func.to_string(), ty_id);
    wasm.imports.set_name(lib_func.to_string(), imp_id);

    fid
}

fn perform_cache_lookup(val_dt: Option<DataType>, static_offset: u64, num_bytes: u32, wasm: &mut ModuleIterator, locals: &mut LocalsTracker, globals: &Globals, check_access: FunctionID) {
    wasm.before();

    let orig_stack_vals = bundle_args(val_dt, static_offset, num_bytes, wasm, locals);

    call_cache(wasm, check_access);
    decode_result(wasm, locals, globals);

    fix_stack(wasm, &orig_stack_vals);
}

/// Returns values in the order that they should be replaced on the stack!
fn bundle_args(val_dt: Option<DataType>, static_offset: u64, data_size: u32, wasm: &mut ModuleIterator, locals: &mut LocalsTracker) -> Vec<LocalID> {
    let mut orig_stack_vals = vec![];

    let addr = if let Some(dt) = val_dt {
        let addr = LocalID(locals.use_local(I32, wasm));
        let val = LocalID(locals.use_local(dt, wasm));

        wasm.local_set(val).local_set(addr);

        orig_stack_vals = vec![addr, val];
        addr
    } else {
        let addr = LocalID(locals.use_local(I32, wasm));

        wasm.local_set(addr);

        orig_stack_vals = vec![addr];
        addr
    };

    // the effective address
    wasm.local_get(addr);
    if static_offset > 0 {
        wasm.u32_const(static_offset as u32)
            .i32_add();
    }

    // the size of the data being accessed
    wasm.u32_const(data_size);

    orig_stack_vals
}

fn call_cache(wasm: &mut ModuleIterator, check_access: FunctionID) {
    wasm.call(check_access);
}

fn decode_result(wasm: &mut ModuleIterator, locals: &mut LocalsTracker, globals: &Globals) {
    let result = LocalID(locals.use_local(I32, wasm));
    let hits = LocalID(locals.use_local(I32, wasm));
    let misses = LocalID(locals.use_local(I32, wasm));

    wasm.local_tee(result);

    // decode number of hits
    wasm
        .u32_const(0xFFFF0000)
        .i32_and()
        .i32_const(16)
        .i32_shr_signed()
        .local_set(hits);

    // decode number of misses
    wasm
        .local_get(result)
        .u32_const(0x0000FFFF)
        .i32_and()
        .local_set(misses);

    // increment hits global
    wasm.global_get(globals.hit)
        .local_get(hits)
        .i32_add()
        .global_set(globals.hit);

    // increment misses global
    wasm.global_get(globals.miss)
        .local_get(misses)
        .i32_add()
        .global_set(globals.miss);

}
fn fix_stack(wasm: &mut ModuleIterator, orig_stack_vals: &Vec<LocalID>) {
    for local in orig_stack_vals {
        wasm.local_get(*local);
    }
}

struct Globals {
    hit: GlobalID,
    miss: GlobalID,
}
impl Globals {
    pub fn new(wasm: &mut Module) -> Self {
        Self {
            hit: add_global(wasm),
            miss: add_global(wasm),
        }
    }
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
    pub fn reset_probe<'a, T: Opcode<'a>>(&mut self, _injector: &mut T) {
        // Reset the local variables to their default value, keeps value guarantee consistent
        // between probe body entries!
        self.available.extend(self.in_use.to_owned());
        self.in_use.clear();
    }
    pub fn reset_function(&mut self) {
        self.available.clear();
        self.in_use.clear();
    }
}
