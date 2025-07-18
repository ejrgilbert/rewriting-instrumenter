use crate::monitor::{
    add_global, bundle_load_store_args, call_flush_on_exit, fix_stack, import_lib_func,
    LocalsTracker,
};
use wasmparser::{MemArg, Operator};
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, GlobalID, LocalID};
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::opcode::Instrumenter;
use wirm::DataType::{F32, F64, I32, I64};
use wirm::{DataType, Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    let globals = Globals::new(&mut wasm);
    let mut locals = LocalsTracker::default();
    let maps_lib = import_maps_lib(&mut wasm);
    init_map_on_start(&globals, &maps_lib, &mut wasm);

    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);
    inject_instrumentation(&mut mod_it, &mut locals, &globals, &maps_lib);
    flush(&globals, &maps_lib, &mut wasm);

    wasm
}

fn inject_instrumentation(
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    globals: &Globals,
    maps: &Maps,
) {
    let mut first_func: bool = true;
    let mut curr_fid = if let Location::Module { func_idx, .. } = wasm.curr_loc().0 {
        func_idx
    } else {
        panic!("we don't support non-module locations (components don't work atm).")
    };
    loop {
        if let Location::Module { func_idx, .. } = wasm.curr_loc().0 {
            if first_func || curr_fid != func_idx {
                locals.reset_function();
                curr_fid = func_idx;
                first_func = false;
            }
        } else {
            panic!("we don't support non-module locations (components don't work atm).")
        };
        if let Some(op) = wasm.curr_op() {
            if let Some((val_dt, offset, is_write)) = match op {
                Operator::I32Load8S {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I32Load8U {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load8S {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load8U {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I32Load16S {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I32Load16U {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load16S {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load16U {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I32Load {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load32U {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load32S {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::F32Load {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::I64Load {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::F64Load {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((None, *offset, false)),
                Operator::I32Store8 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, true)),
                Operator::I64Store8 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, true)),
                Operator::I32Store16 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, true)),
                Operator::I64Store16 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, true)),
                Operator::I32Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, true)),
                Operator::F32Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(F32), *offset, true)),
                Operator::I64Store32 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, true)),
                Operator::I64Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, true)),
                Operator::F64Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(F64), *offset, true)),
                _ => None,
            } {
                save_access(val_dt, offset, wasm, locals, globals, is_write, maps);
                locals.reset_probe();
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn save_access(
    val_dt: Option<DataType>,
    static_offset: u64,
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    globals: &Globals,
    is_write: bool,
    maps: &Maps,
) {
    wasm.before();

    let orig_stack_vals = bundle_load_store_args(val_dt, static_offset, wasm, locals);

    let effective_addr = LocalID(locals.use_local(I32, wasm));
    wasm.local_set(effective_addr)
        // id and key for $insert_i32booltuple_i32
        .global_get(globals.accesses)
        .local_get(effective_addr)
        .i32_const(is_write as i32)
        // get the original value
        .global_get(globals.accesses)
        .local_get(effective_addr)
        .i32_const(is_write as i32)
        .call(maps.get)
        // increment the original value
        .i32_const(1)
        .i32_add()
        .call(maps.insert);

    fix_stack(wasm, &orig_stack_vals);
}

fn flush(globals: &Globals, maps: &Maps, wasm: &mut Module) {
    let flush_fn = emit_flush_fn(globals, maps, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn emit_flush_fn(globals: &Globals, maps: &Maps, wasm: &mut Module) -> FunctionID {
    let mut flush = FunctionBuilder::new(&[], &[]);

    // flush the map by calling the library
    flush.global_get(globals.accesses).call(maps.flush);

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

fn init_map_on_start(globals: &Globals, maps: &Maps, wasm: &mut Module) {
    // now call `instr_init` in the module's start function
    if let Some(start_fid) = wasm.start {
        if let Some(mut start_func) = wasm.functions.get_fn_modifier(start_fid) {
            start_func.func_entry();
            start_func.global_get(globals.accesses).call(maps.create);
            start_func.finish_instr();
        } else {
            unreachable!("Should have found the function in the module.")
        }
    } else {
        // create the start function and call the `instr_init` function
        let mut start_func = FunctionBuilder::new(&[], &[]);
        start_func.global_get(globals.accesses).call(maps.create);
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
            "insert_i32booltuple_i32",
            &[I32, I32, I32, I32],
            &[],
            wasm,
        ),
        get: import_lib_func(lib, "get_i32booltuple_i32", &[I32, I32, I32], &[I32], wasm),
        flush: import_lib_func(lib, "print_map_as_csv", &[I32], &[], wasm),
    }
}

struct Globals {
    accesses: GlobalID,
}
impl Globals {
    pub fn new(wasm: &mut Module) -> Self {
        Self {
            accesses: add_global(wasm),
        }
    }
}
