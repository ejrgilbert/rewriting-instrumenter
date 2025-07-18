use crate::monitor::{add_util_funcs, call_flush_on_exit, FuncLocHeader, MemTracker};
use std::collections::HashMap;
use wasmparser::MemArg;
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::FunctionID;
use wirm::ir::types::BlockType;
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::module_builder::AddLocal;
use wirm::opcode::MacroOpcode;
use wirm::DataType::I32;
use wirm::{Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    // setup mem tracker and add in necessary strings for flushing
    let mut memory = MemTracker::new(
        vec![
            "fid=".to_string(),
            ", pc=".to_string(),
            ", [".to_string(),
            "]\n".to_string(),
            "true".to_string(),
            "false".to_string(),
            "\n".to_string(),
        ],
        &mut wasm,
    );
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &mut memory);
    memory.setup_module(&mut wasm);
    flush(&mut memory, &mut wasm);
    // make sure the memory is large enough!
    memory.memory_grow(&mut wasm);

    wasm
}

fn flush(memory: &mut MemTracker, wasm: &mut Module) {
    let utils = add_util_funcs(memory, wasm);
    let flush_fn = emit_flush_fn(memory, &utils, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn emit_flush_fn(
    memory: &mut MemTracker,
    utils: &HashMap<String, FunctionID>,
    wasm: &mut Module,
) -> FunctionID {
    let mut flush = FunctionBuilder::new(&[], &[]);

    // create locals
    let entry = flush.add_local(I32);

    let mem = MemArg {
        align: 0,
        max_align: 0,
        offset: 0,
        memory: memory.mem_id,
    };

    let Some(puts) = utils.get("puts") else {
        panic!("could not find puts hostfunc")
    };
    let puts = *puts;
    let Some(puti) = utils.get("puti32") else {
        panic!("could not find puti hostfunc")
    };
    let puti = *puti;

    let (newline_addr, newline_len) = memory.get_str("\n");
    let (fid_addr, fid_len) = memory.get_str("fid=");
    let (pc_addr, pc_len) = memory.get_str(", pc=");
    let (start_addr, start_len) = memory.get_str(", [");
    let (end_addr, end_len) = memory.get_str("]\n");
    let (true_addr, true_len) = memory.get_str("true");
    let (false_addr, false_len) = memory.get_str("false");

    // print "\n"
    flush
        .u32_const(newline_addr)
        .u32_const(newline_len as u32)
        .call(puts);

    #[rustfmt::skip]
    flush
        .u32_const(memory.var_start_offset)
        .local_set(entry)
        .block(BlockType::Empty) // end_loop
            .loop_stmt(BlockType::Empty) // loop_entry
                // check at the end of memory
                .local_get(entry)
                .u32_const(memory.curr_mem_offset)
                .i32_eq()
                .br_if(1) // end_loop

                // print "fid="
                .u32_const(fid_addr)
                .u32_const(fid_len as u32)
                .call(puts)

                .local_get(entry)
                .i32_load(mem) // fid
                .call(puti)
                .local_get(entry)
                .i32_const(4)
                .i32_add()
                .local_set(entry)

                // print ", pc="
                .u32_const(pc_addr)
                .u32_const(pc_len as u32)
                .call(puts)
                .local_get(entry)
                .i32_load(mem) // pc
                .call(puti)
                .local_get(entry)
                .i32_const(4)
                .i32_add()
                .local_set(entry)

                // print ", ["
                .u32_const(start_addr)
                .u32_const(start_len as u32)
                .call(puts)

                .local_get(entry)
                .i32_load8_u(mem) // load the value
                .if_stmt(BlockType::Empty)
                    // if true, print 'true'
                    .u32_const(true_addr)
                    .u32_const(true_len as u32)
                    .call(puts)
                .else_stmt()
                    // if false, print 'false'
                    .u32_const(false_addr)
                    .u32_const(false_len as u32)
                    .call(puts)
                .end()

                .local_get(entry)
                .i32_const(1)
                .i32_add()
                .local_set(entry)

                // print "]\n"
                .u32_const(end_addr)
                .u32_const(end_len as u32)
                .call(puts)
                .br(0) // loop_entry
            .end()
        .end();

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

fn inject_instrumentation(wasm: &mut ModuleIterator, memory: &mut MemTracker) {
    loop {
        let (
            Location::Module {
                func_idx,
                instr_idx,
            },
            end,
        ) = wasm.curr_loc()
        else {
            panic!("non-module locations are not supported")
        };
        if !end {
            coverage_probe(*func_idx, instr_idx as u32, wasm, memory);
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn coverage_probe(fid: u32, pc: u32, wasm: &mut ModuleIterator, memory: &mut MemTracker) {
    wasm.before();

    let mem = MemArg {
        align: 0,
        max_align: 0,
        offset: FuncLocHeader::num_bytes() as u64,
        memory: memory.mem_id,
    };

    let alloc_at = alloc_bool(fid, pc, memory);

    // Set the in-memory value to `true`
    wasm.u32_const(alloc_at).i32_const(1).i32_store8(mem);
}

fn alloc_bool(fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_i8_var(fid, pc)
}
