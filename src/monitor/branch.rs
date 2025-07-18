use crate::monitor::branch::Case::{NoMatch, SimpleBranch, Table};
use crate::monitor::{
    add_util_funcs, call_flush_on_exit, LocalsTracker, MemTracker, MultiCountHeader,
};
use std::collections::HashMap;
use wasmparser::{MemArg, Operator};
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, LocalID};
use wirm::ir::types::BlockType;
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::module_builder::AddLocal;
use wirm::opcode::MacroOpcode;
use wirm::DataType::I32;
use wirm::{Location, Module, Opcode};

pub fn instrument(mut wasm: Module) -> Module {
    let mut locals = LocalsTracker::default();
    // setup mem tracker and add in necessary strings for flushing
    let mut memory = MemTracker::new(
        vec![
            "fid=".to_string(),
            ", pc=".to_string(),
            ", [".to_string(),
            "]\n".to_string(),
            "\n".to_string(),
            ",".to_string(),
        ],
        &mut wasm,
    );
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &mut locals, &mut memory);
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
    let n = flush.add_local(I32);

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
    let (comma_addr, comma_len) = memory.get_str(",");
    let (fid_addr, fid_len) = memory.get_str("fid=");
    let (pc_addr, pc_len) = memory.get_str(", pc=");
    let (start_addr, start_len) = memory.get_str(", [");
    let (end_addr, end_len) = memory.get_str("]\n");

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
                .i32_load(mem) // load the print option
                .local_set(n)
                .local_get(entry)
                .i32_const(4)
                .i32_add()
                .local_set(entry)

                .loop_stmt(BlockType::Empty) // loop_options
                    .local_get(entry)
                    .i32_load(mem) // load the value
                    .call(puti)

                    .local_get(entry)
                    .i32_const(8)
                    .i32_add()
                    .local_set(entry)

                    .local_get(n)
                    .i32_const(-1)
                    .i32_add()

                    .local_tee(n)
                    .i32_eqz()
                    .if_stmt(BlockType::Empty)
                        // print "]\n"
                        .u32_const(end_addr)
                        .u32_const(end_len as u32)
                        .call(puts)
                        .br(2) // loop_entry
                    .else_stmt()
                        // print ","
                        .u32_const(comma_addr)
                        .u32_const(comma_len as u32)
                        .call(puts)
                        .br(1) // loop_options
                    .end()
                .end()
            .end()
        .end();

    let flush_id = flush.finish_module(wasm);
    wasm.set_fn_name(flush_id, "flush_on_exit".to_string());

    flush_id
}

fn inject_instrumentation(
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    memory: &mut MemTracker,
) {
    let mut first_func: bool = true;
    let mut curr_fid = if let Location::Module { func_idx, .. } = wasm.curr_loc().0 {
        func_idx
    } else {
        panic!("we don't support non-module locations (components don't work atm).")
    };
    loop {
        if let Some(op) = wasm.curr_op() {
            let (
                Location::Module {
                    func_idx,
                    instr_idx,
                },
                _,
            ) = wasm.curr_loc()
            else {
                panic!("non-module locations are not supported")
            };
            if first_func || curr_fid != func_idx {
                locals.reset_function();
                curr_fid = func_idx;
                first_func = false;
            }
            let case = match op {
                Operator::If { .. } | Operator::BrIf { .. } => SimpleBranch,
                Operator::BrTable { targets } => Table {
                    num_targets: targets.len(),
                },
                _ => NoMatch,
            };

            match case {
                SimpleBranch => {
                    simple_branch_probe(*func_idx, instr_idx as u32, wasm, locals, memory);
                    locals.reset_probe();
                }
                Table { num_targets } => {
                    br_table_probe(
                        *func_idx,
                        instr_idx as u32,
                        num_targets,
                        wasm,
                        locals,
                        memory,
                    );
                    locals.reset_probe();
                }
                NoMatch => {}
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

enum Case {
    NoMatch,
    SimpleBranch,
    Table { num_targets: u32 },
}

fn simple_branch_probe(
    fid: u32,
    pc: u32,
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    memory: &mut MemTracker,
) {
    wasm.before();

    let mem = MemArg {
        align: 0,
        max_align: 0,
        offset: 0,
        memory: memory.mem_id,
    };

    let alloc_at = alloc_branch(fid, pc, memory);
    let arg0 = bundle_args(wasm, locals);
    let mem_offset = LocalID(locals.use_local(I32, wasm));

    // check if was taken
    // (local.set $offset (i32.add (local.get $entry) (select (i32.const 20) (i32.const 12) (local.get $arg0))))
    wasm
        // Load the memory offset
        .u32_const(alloc_at)
        // calculate location of the in-memory value we want to increment
        .u32_const((MultiCountHeader::num_bytes() + size_of::<u64>()) as u32)
        .u32_const(MultiCountHeader::num_bytes() as u32)
        .local_get(arg0)
        .select()
        .i32_add()
        // store the memory offset we're targeting
        .local_tee(mem_offset);

    // Increment the in-memory value now that we have the offset
    wasm.local_get(mem_offset)
        .i64_load(mem)
        .i64_const(1)
        .i64_add()
        .i64_store(mem);

    fix_stack(wasm, &vec![arg0]);
}

fn br_table_probe(
    fid: u32,
    pc: u32,
    num_targets: u32,
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
    memory: &mut MemTracker,
) {
    wasm.before();

    // targets.len is the number of targets (not including the default)
    let alloc_at = alloc_br_table(num_targets + 1, fid, pc, memory);
    let arg0 = bundle_args(wasm, locals);
    let max = LocalID(locals.use_local(I32, wasm));
    let tgt = LocalID(locals.use_local(I32, wasm));
    let addr = LocalID(locals.use_local(I32, wasm));

    // which branch was taken?
    // max = `n`; where `n` = num_targets - 1
    wasm.local_get(arg0)
        .local_tee(tgt)
        .u32_const(alloc_at)
        .i32_load(MemArg {
            align: 0,
            max_align: 0,
            offset: MultiCountHeader::loc_header_size() as u64,
            memory: memory.mem_id,
        })
        .i32_const(1)
        .i32_sub()
        .local_tee(max);

    #[rustfmt::skip]
    wasm
        .i32_gte_unsigned()
        .if_stmt(BlockType::Empty)
            // if arg0 >= n --> arg0 = n
            .local_get(max)
            .local_set(tgt)
        .end();

    // account for number of bits used to store each count (8 bits per u64)
    wasm.local_get(tgt).i32_const(3).i32_shl();

    wasm.u32_const(alloc_at)
        .i32_add()
        .local_tee(addr)
        .local_get(addr);

    wasm.i64_load(MemArg {
        align: 0,
        max_align: 0,
        offset: MultiCountHeader::num_bytes() as u64,
        memory: memory.mem_id,
    })
    .i64_const(1)
    .i64_add()
    .i64_store(MemArg {
        align: 0,
        max_align: 0,
        offset: MultiCountHeader::num_bytes() as u64,
        memory: memory.mem_id,
    });

    fix_stack(wasm, &vec![arg0]);
}

fn alloc_branch(fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_multicount_var(fid, pc, 2)
}

fn alloc_br_table(num_targets: u32, fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_multicount_var(fid, pc, num_targets)
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
