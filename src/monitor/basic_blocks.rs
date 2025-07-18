#![allow(dead_code)] // for BasicBlockState::get_instr_cnt
use crate::monitor::{
    add_util_funcs, call_flush_on_exit, is_prog_exit_call, FuncLocHeader, MemTracker,
};
use std::collections::HashMap;
use wasmparser::{MemArg, Operator};
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
                .i32_load(mem) // load the value
                .call(puti)

                .local_get(entry)
                .i32_const(8)
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
    let mut block_state = BasicBlockState::default();

    let mut first_func: bool = true;
    let mut curr_fid = if let Location::Module { func_idx, .. } = wasm.curr_loc().0 {
        func_idx
    } else {
        panic!("we don't support non-module locations (components don't work atm).")
    };
    loop {
        let (
            Location::Module {
                func_idx,
                instr_idx: pc,
            },
            at_func_end,
        ) = wasm.curr_loc()
        else {
            panic!("non-module locations are not supported")
        };
        if first_func || curr_fid != func_idx {
            // This is the first time we're visiting this new function
            block_state.reset();
            curr_fid = func_idx;
            first_func = false;
        }
        // https://github.com/titzer/wizard-engine/blob/master/src/util/BasicBlockIterator.v3
        if let Some(op) = wasm.curr_op() {
            match op {
                Operator::Loop {..} |
                // TODO (for End): track whether ends are branched to using a control stack.
                //       If this end has a branch to it, end the previous block, if there was one.
                Operator::End => {
                    if block_state.start != pc {
                        block_state.end_block_here();
                        if matches!(op, Operator::End) && !at_func_end {
                            // semantics of End requires that this be injected AFTER it to execute!
                            wasm.after();
                        } else {
                            wasm.before();
                        }
                        count_probe(*func_idx, pc as u32, wasm, memory);
                    }
                },

                // Bytecodes that end the current block after this instruction.
                Operator::If {..} |
                Operator::Else |
                Operator::Catch {..} |
                Operator::CatchAll |
                Operator::Throw {..} |
                Operator::Rethrow {..} |
                Operator::Return |
                Operator::Unreachable |
                Operator::Br {..} |
                Operator::BrTable {..} |
                Operator::BrIf {..} |
                Operator::BrOnCast {..} |
                Operator::BrOnCastFail {..} |
                Operator::BrOnNull {..} |
                Operator::BrOnNonNull {..} => {
                    // End the current block after this instruction.
                    block_state.continue_block();
                    block_state.end_block_here();

                    // exit | : before this instruction (to ensure it executes)
                    wasm.before();
                    count_probe(*func_idx, pc as u32, wasm, memory);
                },
                _ => {
                    block_state.continue_block();
                    if is_prog_exit_call(op, wasm.module) {
                        // This is an exiting WASI function call, block exit!
                        wasm.before();
                        count_probe(*func_idx, pc as u32, wasm, memory);
                    }
                }
            };
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

fn count_probe(fid: u32, pc: u32, wasm: &mut ModuleIterator, memory: &mut MemTracker) {
    let mem = MemArg {
        align: 0,
        max_align: 0,
        offset: FuncLocHeader::num_bytes() as u64,
        memory: memory.mem_id,
    };

    let alloc_at = alloc_count(fid, pc, memory);

    // Increment the in-memory value now that we have the offset
    wasm.u32_const(alloc_at)
        .u32_const(alloc_at)
        .i64_load(mem)
        .i64_const(1)
        .i64_add()
        .i64_store(mem);
}

fn alloc_count(fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_count_var(fid, pc)
}

// State to encode the start and end opcode index of a basic block.
// TODO: track whether ends are branched to using a control stack.
//       If this end has a branch to it, end the previous block, if there was one.
#[derive(Default)]
struct BasicBlockState {
    start: usize,
    end: usize,
}
impl BasicBlockState {
    fn reset(&mut self) {
        self.start = 0;
        self.end = 0;
    }
    fn continue_block(&mut self) {
        self.end += 1;
    }
    fn end_block_here(&mut self) {
        self.start = self.end;
    }
    fn get_instr_cnt(&self) -> usize {
        self.end - self.start
    }
}
