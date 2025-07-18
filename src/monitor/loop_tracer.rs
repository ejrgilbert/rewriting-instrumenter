use crate::monitor::{call_flush_on_exit, import_lib_func, LocalsTracker, MemTracker};
use wasmparser::{MemArg, Operator};
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, LocalID};
use wirm::ir::types::BlockType;
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::opcode::{Instrumenter, MacroOpcode};
use wirm::DataType::I32;
use wirm::{Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    // setup mem tracker
    let mut memory = MemTracker::new(vec![], &mut wasm);
    let mut locals = LocalsTracker::default();
    let tracer = import_tracer_lib(&mut wasm);

    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);
    let mut init_func = FunctionBuilder::new(&[], &[]);
    inject_instrumentation(
        &mut mod_it,
        &mut init_func,
        &mut memory,
        &mut locals,
        &tracer,
    );
    configure_init_func(init_func, &mut wasm);
    flush(&tracer, &mut wasm);

    wasm
}

fn inject_instrumentation(
    wasm: &mut ModuleIterator,
    init_func: &mut FunctionBuilder,
    memory: &mut MemTracker,
    locals: &mut LocalsTracker,
    tracer: &Tracer,
) {
    let mut first_func: bool = true;
    let (mut curr_fid, curr_pc) = if let Location::Module {
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
                wasm.func_entry();
                anchor_probe(*curr_fid, curr_pc as u32, memory, tracer, init_func, wasm);
                wasm.finish_instr();
            }
        } else {
            panic!("we don't support non-module locations (components don't work atm).")
        };
        if let Some(op) = wasm.curr_op() {
            match op {
                Operator::Loop { .. } => {
                    anchor_probe(*curr_fid, curr_pc as u32, memory, tracer, init_func, wasm)
                }
                Operator::BrIf { .. } | Operator::If { .. } => if_probe(locals, tracer, wasm),
                Operator::BrTable { targets, .. } => {
                    br_table_probe(targets.len(), locals, tracer, wasm)
                }
                _ => {}
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn anchor_probe(
    fid: u32,
    pc: u32,
    memory: &mut MemTracker,
    tracer: &Tracer,
    init_func: &mut FunctionBuilder,
    wasm: &mut ModuleIterator,
) {
    wasm.before();

    let (alloc_id, memarg) = alloc_anchor_id(fid, pc, memory, tracer, init_func);

    // now the actual probe logic
    wasm
        // load the alloc_id
        .u32_const(alloc_id)
        .i32_load(memarg)
        // call the tracer with the id
        .call(tracer.on_anchor);
}

fn alloc_anchor_id(
    fid: u32,
    pc: u32,
    memory: &mut MemTracker,
    tracer: &Tracer,
    init_func: &mut FunctionBuilder,
) -> (u32, MemArg) {
    let val_loc = memory.alloc_i32_var();

    let mem = MemArg {
        align: 0,
        max_align: 0,
        offset: 0, // this alloc var type has no header!
        memory: memory.mem_id,
    };

    // logic to initialize the anchor_id for this probe.
    init_func
        // where to store
        .u32_const(val_loc)
        // get the anchor_id
        .u32_const(fid)
        .u32_const(pc)
        .call(tracer.init_anchor)
        .i32_store(mem);

    (val_loc, mem)
}

fn if_probe(locals: &mut LocalsTracker, tracer: &Tracer, wasm: &mut ModuleIterator) {
    wasm.before();
    let arg0 = bundle_args(wasm, locals);

    wasm
        // call the tracer with arg0
        .local_get(arg0)
        .call(tracer.on_if);

    fix_stack(wasm, &vec![arg0]);
    locals.reset_probe();
}

fn br_table_probe(
    num_targets: u32,
    locals: &mut LocalsTracker,
    tracer: &Tracer,
    wasm: &mut ModuleIterator,
) {
    wasm.before();
    let arg0 = bundle_args(wasm, locals);

    wasm
        // let target = arg0 <= (num_targets - 1) ? arg0 : num_targets
        .local_get(arg0)
        .u32_const(num_targets)
        .i32_const(1)
        .i32_sub()
        .i32_lte_unsigned()
        .if_stmt(BlockType::Type(I32))
        .local_get(arg0)
        .else_stmt()
        .u32_const(num_targets)
        .end()
        // Call the tracer with this br_table's target
        .call(tracer.on_br_table);

    fix_stack(wasm, &vec![arg0]);
    locals.reset_probe();
}

/// Returns values in the order that they should be replaced on the stack!
fn bundle_args(wasm: &mut ModuleIterator, locals: &mut LocalsTracker) -> LocalID {
    let arg0 = LocalID(locals.use_local(I32, wasm));
    wasm.local_set(arg0);
    arg0
}

fn fix_stack(wasm: &mut ModuleIterator, orig_stack_vals: &Vec<LocalID>) {
    for local in orig_stack_vals {
        wasm.local_get(*local);
    }
}

fn flush(tracer: &Tracer, wasm: &mut Module) {
    let flush_fn = emit_flush_fn(tracer, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn emit_flush_fn(tracer: &Tracer, wasm: &mut Module) -> FunctionID {
    let mut flush = FunctionBuilder::new(&[], &[]);

    // flush the map by calling the library
    flush.call(tracer.flush);

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}
pub fn configure_init_func<'a>(init_func: FunctionBuilder<'a>, wasm: &mut Module<'a>) {
    let state_init_id = if init_func.body.num_instructions > 0 {
        // Call the probe init state function in the instr_init body
        let state_init_id = init_func.finish_module(wasm);
        wasm.set_fn_name(state_init_id, "init_probe_state".to_string());
        Some(state_init_id)
    } else {
        None
    };
    call_instr_init_at_start(state_init_id, wasm);
}

fn call_instr_init_at_start(state_init_id: Option<FunctionID>, module: &mut Module) {
    if let Some(state_init_id) = state_init_id {
        // now call `instr_init` in the module's start function
        if let Some(start_fid) = module.start {
            if let Some(mut start_func) = module.functions.get_fn_modifier(start_fid) {
                start_func.func_entry();
                start_func.call(state_init_id);
                start_func.finish_instr();
            } else {
                unreachable!("Should have found the function in the module.")
            }
        } else {
            // create the start function and call the `instr_init` function
            let mut start_func = FunctionBuilder::new(&[], &[]);
            start_func.call(state_init_id);

            let start_fid = start_func.finish_module(module);
            module.start = Some(start_fid);
        }
    }
}

struct Tracer {
    init_anchor: FunctionID,
    on_anchor: FunctionID,
    on_if: FunctionID,
    on_br_table: FunctionID,
    flush: FunctionID,
}
fn import_tracer_lib(wasm: &mut Module) -> Tracer {
    let lib = "tracer";
    Tracer {
        init_anchor: import_lib_func(lib, "init_anchor", &[I32, I32], &[I32], wasm),
        on_anchor: import_lib_func(lib, "on_anchor", &[I32], &[], wasm),
        on_if: import_lib_func(lib, "on_if", &[I32], &[], wasm),
        on_br_table: import_lib_func(lib, "on_br_table", &[I32], &[], wasm),
        flush: import_lib_func(lib, "flush_csv", &[], &[], wasm),
    }
}
