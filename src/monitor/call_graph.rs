use crate::monitor::{add_global, call_flush_on_exit, import_lib_func, LocalsTracker};
use wasmparser::Operator;
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, GlobalID};
use wirm::ir::types::BlockType;
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::opcode::{Instrumenter, MacroOpcode};
use wirm::DataType::I32;
use wirm::{Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    let mut locals = LocalsTracker::default();
    let globals = Globals::new(&mut wasm);
    let maps_lib = import_maps_lib(&mut wasm);

    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);
    inject_instrumentation(&mut mod_it, &mut locals, &globals, &maps_lib);
    flush(&globals, &maps_lib, &mut wasm);
    init_map_on_start(&globals, &maps_lib, &mut wasm);

    wasm
}

fn inject_instrumentation(
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    globals: &Globals,
    maps: &Maps,
) {
    let mut first_func: bool = true;
    let (mut curr_fid, ..) = if let Location::Module {
        func_idx,
        instr_idx,
        ..
    } = wasm.curr_loc().0
    {
        (func_idx, instr_idx)
    } else {
        panic!("we don't support non-module locations (components don't work atm).")
    };
    loop {
        if let Location::Module { func_idx, .. } = wasm.curr_loc().0 {
            if first_func || curr_fid != func_idx {
                // This is the first time we're visiting this new function
                // reset state
                locals.reset_function();
                curr_fid = func_idx;
                first_func = false;

                // inject a function entry probe
                func_entry_probe(*curr_fid, globals, maps, wasm);
            }
        } else {
            panic!("we don't support non-module locations (components don't work atm).")
        };
        if let Some(op) = wasm.curr_op() {
            match op {
                Operator::Call { function_index } | Operator::ReturnCall { function_index } => {
                    call_probe(*curr_fid, *function_index, globals, maps, wasm)
                }
                Operator::CallIndirect { .. }
                | Operator::ReturnCallIndirect { .. }
                | Operator::CallRef { .. }
                | Operator::ReturnCallRef { .. } => call_indirect_probe(*curr_fid, globals, wasm),
                _ => {}
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn func_entry_probe(fid: u32, globals: &Globals, maps: &Maps, wasm: &mut ModuleIterator) {
    wasm.func_entry();

    wasm
        // check if we're trying to collect a call target
        .global_get(globals.tracking_target)
        .if_stmt(BlockType::Empty)
        // the map ID
        .global_get(globals.call_graph)
        // (caller, fid)
        .global_get(globals.caller)
        .u32_const(fid)
        // get the current value
        .global_get(globals.call_graph)
        // (caller, fid)
        .global_get(globals.caller)
        .u32_const(fid)
        .call(maps.get)
        // increment the current value by 1
        .u32_const(1)
        .i32_add()
        .call(maps.insert)
        // unset the tracking bool
        .i32_const(0)
        .global_set(globals.tracking_target)
        .end();

    wasm.finish_instr();
}

fn call_probe(fid: u32, target: u32, globals: &Globals, maps: &Maps, wasm: &mut ModuleIterator) {
    wasm.before();
    wasm
        // the map ID
        .global_get(globals.call_graph)
        // (caller, target)
        .u32_const(fid)
        .u32_const(target)
        // get the current value
        .global_get(globals.call_graph)
        // (caller, fid)
        .u32_const(fid)
        .u32_const(target)
        .call(maps.get)
        // increment the current value by 1
        .u32_const(1)
        .i32_add()
        .call(maps.insert);
}

fn call_indirect_probe(fid: u32, globals: &Globals, wasm: &mut ModuleIterator) {
    wasm.before();
    wasm
        // set the tracking bool
        .i32_const(1)
        .global_set(globals.tracking_target)
        .u32_const(fid)
        .global_set(globals.caller);
}

fn flush(globals: &Globals, maps: &Maps, wasm: &mut Module) {
    let flush_fn = emit_flush_fn(globals, maps, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn emit_flush_fn(globals: &Globals, maps: &Maps, wasm: &mut Module) -> FunctionID {
    let mut flush = FunctionBuilder::new(&[], &[]);

    // flush the map by calling the library
    flush.global_get(globals.call_graph).call(maps.flush);

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

struct Globals {
    call_graph: GlobalID,
    tracking_target: GlobalID,
    caller: GlobalID,
}
impl Globals {
    pub fn new(wasm: &mut Module) -> Self {
        Self {
            call_graph: add_global(wasm),
            tracking_target: add_global(wasm),
            caller: add_global(wasm),
        }
    }
}

fn init_map_on_start(globals: &Globals, maps: &Maps, wasm: &mut Module) {
    // now call `instr_init` in the module's start function
    if let Some(start_fid) = wasm.start {
        if let Some(mut start_func) = wasm.functions.get_fn_modifier(start_fid) {
            start_func.func_entry();
            start_func.global_get(globals.call_graph).call(maps.create);
            start_func.finish_instr();
        } else {
            unreachable!("Should have found the function in the module.")
        }
    } else {
        // create the start function and call the `instr_init` function
        let mut start_func = FunctionBuilder::new(&[], &[]);
        start_func.global_get(globals.call_graph).call(maps.create);
        let start_fid = start_func.finish_module(wasm);
        wasm.start = Some(start_fid);
    }
}

struct Maps {
    create: FunctionID,
    insert: FunctionID,
    get: FunctionID,
    flush: FunctionID,
}
fn import_maps_lib(wasm: &mut Module) -> Maps {
    let lib = "whamm_core";
    Maps {
        create: import_lib_func(lib, "create_tuple_i32_with_id", &[I32], &[], wasm),
        insert: import_lib_func(
            lib,
            "insert_i32i32tuple_i32",
            &[I32, I32, I32, I32],
            &[],
            wasm,
        ),
        get: import_lib_func(lib, "get_i32i32tuple_i32", &[I32, I32, I32], &[I32], wasm),
        flush: import_lib_func(lib, "print_map_as_csv", &[I32], &[], wasm),
    }
}
