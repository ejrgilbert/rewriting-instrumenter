use std::collections::{HashMap, HashSet};
use orca_wasm::ir::id::{FunctionID, LocalID, MemoryID};
use orca_wasm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use orca_wasm::iterator::module_iterator::ModuleIterator;
use orca_wasm::{DataSegment, DataSegmentKind, Instructions, Location, Module, Opcode};
use orca_wasm::DataType::I32;
use orca_wasm::ir::function::FunctionBuilder;
use orca_wasm::ir::module::module_functions::{FuncKind, ImportedFunction, LocalFunction};
use orca_wasm::ir::types::{BlockType, InitExpr, Value as OrcaValue};
use orca_wasm::module_builder::AddLocal;
use orca_wasm::opcode::{Instrumenter, MacroOpcode};
use wasmparser::{MemArg, MemoryType, Operator};
use crate::monitor::branch::Case::{NoMatch, SimpleBranch, Table};
use crate::monitor::LocalsTracker;

pub const WASM_PAGE_SIZE: u32 = 65_536;

pub fn instrument(mut wasm: Module) -> Module {
    let mut locals = LocalsTracker::default();
    let mut memory = MemTracker::new(&mut wasm);
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &mut locals, &mut memory);
    memory.setup_module(&mut wasm);
    flush(&mut memory, &mut wasm);
    // make sure the memory is large enough!
    memory.memory_grow(&mut wasm);

    wasm
}

fn flush(memory: &mut MemTracker, wasm: &mut Module) {
    let host_funcs = add_imports(wasm);
    let flush_fn = emit_flush_fn(memory, &host_funcs, wasm);
    call_flush_on_exit(flush_fn, wasm)
}

fn add_imports(wasm: &mut Module) -> HashMap<String, FunctionID> {
    let host_funcs = [("wizeng", "puti", vec![I32], vec![]), ("wizeng", "puts", vec![I32, I32], vec![])];

    let mut imports = HashMap::new();
    for (module, name, params, results) in host_funcs.iter() {
        let ty_id = wasm.types.add_func_type(params, results);
        let (fid, imp_id) = wasm.add_import_func(module.to_string(), name.to_string(), ty_id);
        wasm.imports.set_name(name.to_string(), imp_id);

        imports.insert(name.to_string(), fid);
    }

    imports
}

fn emit_flush_fn(memory: &mut MemTracker, host_funcs: &HashMap<String, FunctionID>, wasm: &mut Module) -> FunctionID {
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

    let Some(puts) = host_funcs.get("puts") else {
        panic!("could not find puts hostfunc")
    };
    let puts = *puts;
    let Some(puti) = host_funcs.get("puti") else {
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

fn call_flush_on_exit(flush_fn: FunctionID, wasm: &mut Module) {
    // handles:
    // - start/main func::exit
    // - wasi exit calls!
    inject_flush_on_end(flush_fn, wasm);
    inject_flush_on_wasi_exit_calls(flush_fn, wasm);
}

fn inject_flush_on_end(flush_fn: FunctionID, wasm: &mut Module) {
    let fid = if let Some(main_fid) = wasm
        .exports
        .get_func_by_name("main".to_string())
    {
        main_fid
    } else if let Some(main_fid) = wasm
        .exports
        .get_func_by_name("_start".to_string())
    {
        main_fid
    } else if let Some(start_fid) = wasm.start {
        start_fid
    } else {
        unimplemented!("Your target Wasm has no main or start function...we do not support flushing state variables in this scenario.")
    };
    let mut main = wasm.functions.get_fn_modifier(fid).unwrap();

    main.func_exit();
    main.call(flush_fn);
    main.finish_instr();
}

fn inject_flush_on_wasi_exit_calls(flush_fn: FunctionID, wasm: &mut Module) {
    let mut iter = ModuleIterator::new(wasm, &vec![]);
    loop {
        if let Some(op) = iter.curr_op() {
            if is_prog_exit_call(op, iter.module) {
                // This is an exiting WASI function call, flush first!
                iter.before();
                iter.call(flush_fn);
                iter.finish_instr();
            }
        }

        if iter.next().is_none() {
            break;
        };
    }
}

fn is_prog_exit_call(opcode: &Operator, wasm: &Module) -> bool {
    match opcode {
        Operator::Call {function_index: fid} |
        Operator::ReturnCall {
            function_index: fid
        } => {
            let target = match wasm.functions.get_kind(FunctionID(*fid)) {
                FuncKind::Import(ImportedFunction { import_id, .. }) => {
                    let import = wasm.imports.get(*import_id);
                    let mod_name = import.module.to_string();
                    let func_name = import.name.to_string();
                    format!("{mod_name}:{func_name}")
                }
                FuncKind::Local(LocalFunction { func_id, .. }) => {
                    let mod_name = match &wasm.module_name {
                        Some(name) => name.clone(),
                        None => "".to_string(),
                    };
                    let func_name = wasm.functions.get_name(*func_id).clone().unwrap_or_default();
                    format!("{mod_name}:{func_name}")
                }
            };
            let exiting_call = HashSet::from(["wasi_snapshot_preview1:proc_exit".to_string()]);
            exiting_call.contains(&target)
        }
        _ => false,
    }
}

fn inject_instrumentation(wasm: &mut ModuleIterator, locals: &mut LocalsTracker, memory: &mut MemTracker) {
    let mut curr_fid = if let Location::Module {func_idx, ..} = wasm.curr_loc().0 {
        func_idx
    } else {
        panic!("we don't support non-module locations (components don't work atm).")
    };
    loop {
        if let Some(op) = wasm.curr_op() {
            let (Location::Module {
                func_idx, instr_idx
            }, _) = wasm.curr_loc() else {
                panic!("non-module locations are not supported")
            };
            if curr_fid != func_idx {
                locals.reset_function();
                curr_fid = func_idx;
            }
            let case = match op {
                Operator::If { .. } |
                Operator::BrIf { .. } => SimpleBranch,
                Operator::BrTable { targets } => Table {num_targets: targets.len()},
                Operator::Select => NoMatch,
                _ => NoMatch,
            };

            match case {
                SimpleBranch => {
                    simple_branch_probe(*func_idx, instr_idx as u32, wasm, locals, memory);
                    locals.reset_probe();
                }
                Table {num_targets} => {
                    br_table_probe(*func_idx, instr_idx as u32, num_targets, wasm, locals, memory);
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
    Table {num_targets: u32}
}

fn simple_branch_probe(fid: u32, pc: u32, wasm: &mut ModuleIterator, locals: &mut LocalsTracker, memory: &mut MemTracker) {
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
        .u32_const((AllocatedVar::full_header_size() + size_of::<u64>()) as u32)
        .u32_const(AllocatedVar::full_header_size() as u32)
        .local_get(arg0)
        .select()
        .i32_add()
        // store the memory offset we're targeting
        .local_tee(mem_offset);

    // Increment the in-memory value now that we have the offset
    wasm
        .local_get(mem_offset)
        .i64_load(mem)
        .i64_const(1)
        .i64_add()
        .i64_store(mem);

    fix_stack(wasm, &vec![arg0]);
}

fn br_table_probe(fid: u32, pc: u32, num_targets: u32, wasm: &mut ModuleIterator, locals: &mut LocalsTracker, memory: &mut MemTracker) {
    wasm.before();

    // targets.len is the number of targets (not including the default)
    let alloc_at = alloc_br_table(num_targets + 1, fid, pc, memory);
    let arg0 = bundle_args(wasm, locals);
    let max = LocalID(locals.use_local(I32, wasm));
    let tgt = LocalID(locals.use_local(I32, wasm));
    let addr = LocalID(locals.use_local(I32, wasm));

    // which branch was taken?
    // max = `n`; where `n` = num_targets - 1
    wasm
        .local_get(arg0)
        .local_tee(tgt)
        .u32_const(alloc_at)
        .i32_load(MemArg {
            align: 0,
            max_align: 0,
            offset: AllocatedVar::loc_header_size() as u64,
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
    wasm
        .local_get(tgt)
        .i32_const(3)
        .i32_shl();

    wasm
        .u32_const(alloc_at)
        .i32_add()
        .local_tee(addr)
        .local_get(addr);

    wasm
        .i64_load(MemArg {
            align: 0,
            max_align: 0,
            offset: AllocatedVar::full_header_size() as u64,
            memory: memory.mem_id,
        })
        .i64_const(1)
        .i64_add()
        .i64_store(MemArg {
            align: 0,
            max_align: 0,
            offset: AllocatedVar::full_header_size() as u64,
            memory: memory.mem_id,
        });

    fix_stack(wasm, &vec![arg0]);
}

fn alloc_branch(fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_var(fid, pc, 2)
}

fn alloc_br_table(num_targets: u32, fid: u32, pc: u32, memory: &mut MemTracker) -> u32 {
    memory.alloc_var(fid, pc, num_targets)
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

struct MemTracker {
    curr_mem_offset: u32,
    var_start_offset: u32,
    mem_id: u32,

    allocated_vars: Vec<AllocatedVar>,
    strings: HashMap<String, u32>
}
impl MemTracker {
    fn new(wasm: &mut Module) -> Self {
        let mem_id = *wasm.add_local_memory(MemoryType {
            memory64: false,
            shared: false,
            initial: 1,
            maximum: None,
            page_size_log2: None,
        });
        wasm.exports.add_export_mem("instrumentation_mem".to_string(), mem_id);

        // go ahead and set up the needed strings so that they live at
        // memory offset 0!
        let (strings, curr_mem_offset) = setup_strings(mem_id, wasm);

        Self {
            curr_mem_offset,
            var_start_offset: curr_mem_offset,
            mem_id,
            allocated_vars: Vec::default(),
            strings
        }
    }

    pub(crate) fn memory_grow(&mut self, wasm: &mut Module) {
        // If we've allocated any memory, bump the app's memory up to account for that
        if let Some(mem) = wasm.memories.get_mut(MemoryID(self.mem_id)) {
            let req_pages = ((self.curr_mem_offset / WASM_PAGE_SIZE) + 1) as u64;
            if mem.ty.initial < req_pages {
                mem.ty.initial = req_pages;
            }
        }
    }
    pub fn setup_module(&mut self, wasm: &mut Module) {
        self.alloc_vars_segments(wasm);
    }
    pub fn get_str(&self, str: &str) -> (u32, usize) {
        if let Some(offset) = self.strings.get(str) {
            (*offset, str.len())
        } else {
            // create a data segment for the new string

            panic!("couldn't find string: {str}")
        }
    }
    pub fn alloc_var(&mut self, fid: u32, pc: u32, n: u32) -> u32 {
        let mem_offset = self.curr_mem_offset;
        let allocated_var = AllocatedVar {
            fid,
            pc,
            n
        };

        let len = allocated_var.num_bytes() as u32;
        self.allocated_vars.push(allocated_var);
        self.curr_mem_offset += len;
        mem_offset
    }
    fn alloc_vars_segments(&mut self, wasm: &mut Module) {
        // write the allocated vars to memory
        let mut bytes = vec![];
        for var in self.allocated_vars.iter() {
            bytes.extend(&var.encode());
        }
        add_data_segment(bytes, self.mem_id, self.var_start_offset, wasm);
        // (size already accounted for in self.curr_mem_offset)
    }
}
fn setup_strings(mem_id: u32, wasm: &mut Module) -> (HashMap<String, u32>, u32) {
    // not using self's memory offset, so we can start back at zero for the actual data segment injections
    // since the allocated vars are in a vector, we can depend on the ordering being correct
    // this is to keep from having a ton of data segments created. We just make a single large one for allocated variables.
    let mut mem_offset = 0;
    let mut strings = HashMap::new();
    // add in necessary strings for flushing
    let strs = [
        "fid=",
        ", pc=",
        ", [",
        "]\n",
        "\n",
        ","
    ];
    for str in strs.iter() {
        let (addr, len) = add_data_segment(str.as_bytes().to_vec(), mem_id, mem_offset, wasm);
        mem_offset += len;
        strings.insert(str.to_string(), addr);
    }

    (strings, mem_offset)
}
fn add_data_segment(bytes: Vec<u8>, mem_id: u32, target_offset: u32, wasm: &mut Module) -> (u32, u32) {
    let len = bytes.len() as u32;
    let data = DataSegment {
        data: bytes,
        kind: DataSegmentKind::Active {
            memory_index: mem_id,
            offset_expr: InitExpr::new(vec![Instructions::Value(OrcaValue::I32(target_offset as i32))]),
        },
    };
    wasm.add_data(data);

    (target_offset, len)
}

struct AllocatedVar {
    fid: u32,
    pc: u32,
    // n is the number of entries in the table including the default target
    n: u32,
}
impl AllocatedVar {
    pub fn encode(&self) -> Vec<u8> {
        let mut res = self.fid.to_le_bytes().to_vec();
        res.extend(self.pc.to_le_bytes());
        res.extend(self.n.to_le_bytes());

        // zero padding for value slots based on N
        for _ in 0..self.n {
            res.extend(0_i64.to_le_bytes());
        }
        assert_eq!(self.num_bytes(), res.len());

        res
    }
    fn num_bytes(&self) -> usize {
        // 0      4      8      12        20
        // | fid  |  pc  |   n  |  0 taken |   ...  | n taken |
        Self::full_header_size() +
            (self.n as usize * size_of::<u64>())
    }
    fn loc_header_size() -> usize {
        size_of::<u32>() +
            size_of::<u32>()
    }
    fn full_header_size() -> usize {
        Self::loc_header_size() +
            size_of::<u32>()
    }
}