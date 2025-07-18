use crate::monitor::{
    add_global, add_util_funcs, bundle_load_store_args, call_flush_on_exit, fix_stack,
    import_lib_func, LocalsTracker, MemTracker,
};
use std::collections::HashMap;
use wasmparser::{MemArg, Operator};
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, GlobalID, LocalID};
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::opcode::MacroOpcode;
use wirm::DataType::{F32, F64, I32, I64};
use wirm::{DataType, Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    let globals = Globals::new(&mut wasm);
    let mut locals = LocalsTracker::default();
    let mut memory = MemTracker::new(
        vec![
            "\nhit: ".to_string(),
            "\nmiss: ".to_string(),
            "\n".to_string(),
        ],
        &mut wasm,
    );
    let check_access = import_lib(&mut wasm);
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &mut locals, &globals, check_access);
    flush(&globals, &mut memory, &mut wasm);
    // make sure the memory is large enough!
    memory.memory_grow(&mut wasm);

    wasm
}

fn flush(globals: &Globals, memory: &mut MemTracker, wasm: &mut Module) {
    let utils = add_util_funcs(memory, wasm);
    let flush_fn = emit_flush_fn(globals, memory, &utils, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn emit_flush_fn(
    globals: &Globals,
    memory: &mut MemTracker,
    utils: &HashMap<String, FunctionID>,
    wasm: &mut Module,
) -> FunctionID {
    let mut flush = FunctionBuilder::new(&[], &[]);

    let Some(puts) = utils.get("puts") else {
        panic!("could not find puts hostfunc")
    };
    let puts = *puts;
    let Some(puti) = utils.get("puti32") else {
        panic!("could not find puti hostfunc")
    };
    let puti = *puti;

    let (hit_addr, hit_len) = memory.get_str("\nhit: ");
    let (miss_addr, miss_len) = memory.get_str("\nmiss: ");
    let (newline_addr, newline_len) = memory.get_str("\n");

    // print "\nhit: "
    flush
        .u32_const(hit_addr)
        .u32_const(hit_len as u32)
        .call(puts);

    flush.global_get(globals.hit).call(puti);

    // print "\nmiss: "
    flush
        .u32_const(miss_addr)
        .u32_const(miss_len as u32)
        .call(puts);

    flush.global_get(globals.miss).call(puti);

    // print "\n: "
    flush
        .u32_const(newline_addr)
        .u32_const(newline_len as u32)
        .call(puts);

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

fn inject_instrumentation(
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    globals: &Globals,
    check_access: FunctionID,
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
            if let Some((val_dt, offset, num_bytes)) = match op {
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
                } => Some((None, *offset, 1)),
                Operator::I32Load16S {
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
                } => Some((None, *offset, 2)),
                Operator::I32Load {
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
                } => Some((None, *offset, 4)),
                Operator::I64Load {
                    memarg: MemArg { offset, .. },
                    ..
                }
                | Operator::F64Load {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((None, *offset, 8)),
                Operator::I32Store8 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, 1)),
                Operator::I64Store8 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, 1)),
                Operator::I32Store16 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, 2)),
                Operator::I64Store16 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, 2)),
                Operator::I32Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I32), *offset, 4)),
                Operator::F32Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(F32), *offset, 4)),
                Operator::I64Store32 {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, 4)),
                Operator::I64Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(I64), *offset, 8)),
                Operator::F64Store {
                    memarg: MemArg { offset, .. },
                    ..
                } => Some((Some(F64), *offset, 8)),
                _ => None,
            } {
                perform_cache_lookup(
                    val_dt,
                    offset,
                    num_bytes,
                    wasm,
                    locals,
                    globals,
                    check_access,
                );
                locals.reset_probe();
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn import_lib(wasm: &mut Module) -> FunctionID {
    import_lib_func("cache", "check_access", &[I32, I32], &[I32], wasm)
}

fn perform_cache_lookup(
    val_dt: Option<DataType>,
    static_offset: u64,
    num_bytes: u32,
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    globals: &Globals,
    check_access: FunctionID,
) {
    wasm.before();

    let orig_stack_vals = bundle_load_store_args(val_dt, static_offset, wasm, locals);

    // the size of the data being accessed
    wasm.u32_const(num_bytes);

    call_cache(wasm, check_access);
    decode_result(wasm, locals, globals);

    fix_stack(wasm, &orig_stack_vals);
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
    wasm.u32_const(0xFFFF0000)
        .i32_and()
        .i32_const(16)
        .i32_shr_signed()
        .local_set(hits);

    // decode number of misses
    wasm.local_get(result)
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
