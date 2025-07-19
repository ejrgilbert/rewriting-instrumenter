use crate::monitor::{add_global, add_util_funcs, call_flush_on_exit, MemTracker};
use std::collections::HashMap;
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, GlobalID};
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::opcode::MacroOpcode;
use wirm::{Module, Opcode};

struct Globals {
    count: GlobalID,
}
impl Globals {
    pub fn new(wasm: &mut Module) -> Self {
        Self {
            count: add_global(wasm),
        }
    }
}

pub fn instrument(mut wasm: Module) -> Module {
    let globals = Globals::new(&mut wasm);

    let mut memory = MemTracker::new(
        vec![
            "\ncount: ".to_string(),
            "\n".to_string(),
        ],
        &mut wasm,
    );
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &globals);
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

    let (count_addr, count_len) = memory.get_str("\ncount: ");
    let (newline_addr, newline_len) = memory.get_str("\n");

    // print "\ncount: "
    flush
        .u32_const(count_addr)
        .u32_const(count_len as u32)
        .call(puts);

    flush.global_get(globals.count).call(puti);


    // print "\n: "
    flush
        .u32_const(newline_addr)
        .u32_const(newline_len as u32)
        .call(puts);

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

fn inject_instrumentation(wasm: &mut ModuleIterator, globals: &Globals) {
    loop {
        count_probe(wasm, globals);

        if wasm.next().is_none() {
            break;
        };
    }
}

// ==== HELPERS ====

fn count_probe(wasm: &mut ModuleIterator, globals: &Globals) {
    wasm.before();

    wasm.global_get(globals.count)
        .i32_const(1)
        .i32_add()
        .global_set(globals.count);
}
