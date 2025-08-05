mod basic_blocks;
mod branch;
mod cache;
mod call_graph;
mod coverage_instr;
mod hotness;
mod imix;
mod icount;
mod loop_tracer;
mod mem_access;

use std::collections::{HashMap, HashSet};
use std::io::Error;
use std::path::{Path, PathBuf};
use wasmparser::{MemArg, MemoryType, Operator};
use wirm::ir::function::FunctionBuilder;
use wirm::ir::id::{FunctionID, GlobalID, LocalID, MemoryID};
use wirm::ir::module::module_functions::{FuncKind, ImportedFunction};
use wirm::ir::types::Value::I32;
use wirm::ir::types::{BlockType, InitExpr};
use wirm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use wirm::iterator::module_iterator::ModuleIterator;
use wirm::module_builder::AddLocal;
use wirm::opcode::{Instrumenter, MacroOpcode};
use wirm::{DataSegment, DataSegmentKind, DataType, InitInstr, Module, Opcode};

pub const WASM_PAGE_SIZE: u32 = 65_536;

pub enum Monitor {
    ICount,
    IMix,
    Cache,
    Branch,
    Hotness,
    MemAccess,
    LoopTracer,
    CallGraph,
    CoverageInstr,
    BasicBlocks,
}

impl Monitor {
    fn name(&self) -> &str {
        match self {
            Monitor::ICount => "icount",
            Monitor::IMix => "imix",
            Monitor::Cache => "cache-sim",
            Monitor::Branch => "branches",
            Monitor::Hotness => "hotness",
            Monitor::MemAccess => "mem-access",
            Monitor::LoopTracer => "loop-tracer",
            Monitor::CallGraph => "call-graph",
            Monitor::CoverageInstr => "instr-coverage",
            Monitor::BasicBlocks => "basic-blocks",
        }
    }
}

/// Adds monitor instrumentation bytecode to an existing
/// WASM module.
pub fn add_monitor(module: Module, monitor: Monitor, path: &Path) -> Result<(), Error> {
    let instrumented_module = match monitor {
        Monitor::ICount => icount::instrument(module),
        Monitor::IMix => imix::instrument(module),
        Monitor::Cache => cache::instrument(module),
        Monitor::Branch => branch::instrument(module),
        Monitor::Hotness => hotness::instrument(module),
        Monitor::MemAccess => mem_access::instrument(module),
        Monitor::LoopTracer => loop_tracer::instrument(module),
        Monitor::CallGraph => call_graph::instrument(module),
        Monitor::CoverageInstr => coverage_instr::instrument(module),
        Monitor::BasicBlocks => basic_blocks::instrument(module),
    };

    write_module(instrumented_module, monitor.name(), path)
}

/// Writes the WASM module to the given path adding
/// monitor name to the file name.
fn write_module(mut module: Module, monitor_name: &str, path: &Path) -> Result<(), Error> {
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let extension = path.extension().unwrap().to_str().unwrap();
    let new_file_stem = format!("{}-{}", file_stem, monitor_name);
    let new_file_name = PathBuf::from(format!("{}.{}", new_file_stem, extension));

    module.emit_wasm(new_file_name.to_str().unwrap())
}

pub fn add_global(wasm: &mut Module) -> GlobalID {
    wasm.add_global(
        InitExpr::new(vec![InitInstr::Value(I32(0))]),
        DataType::I32,
        true,
        false,
    )
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
    pub fn reset_probe(&mut self) {
        self.available.extend(self.in_use.to_owned());
        self.in_use.clear();
    }
    pub fn reset_function(&mut self) {
        self.available.clear();
        self.in_use.clear();
    }
}

struct MemTracker {
    curr_mem_offset: u32,
    var_start_offset: u32,
    mem_id: u32,

    allocated_vars: Vec<AllocatedVar>,
    strings: HashMap<String, u32>,
}
impl MemTracker {
    fn new(strs_to_emit: Vec<String>, wasm: &mut Module) -> Self {
        let mem_id = *wasm.add_local_memory(MemoryType {
            memory64: false,
            shared: false,
            initial: 1,
            maximum: None,
            page_size_log2: None,
        });
        wasm.exports
            .add_export_mem("instrumentation_mem".to_string(), mem_id);

        // go ahead and set up the needed strings so that they live at
        // memory offset 0!
        let (strings, curr_mem_offset) = setup_strings(strs_to_emit, mem_id, wasm);

        Self {
            curr_mem_offset,
            var_start_offset: curr_mem_offset,
            mem_id,
            allocated_vars: Vec::default(),
            strings,
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
    pub fn alloc_multicount_var(&mut self, fid: u32, pc: u32, n: u32) -> u32 {
        let mem_offset = self.curr_mem_offset;
        let allocated_var = AllocatedVar::MultiCount {
            header: MultiCountHeader { fid, pc, n },
        };

        let len = allocated_var.num_bytes() as u32;
        self.allocated_vars.push(allocated_var);
        self.curr_mem_offset += len;
        mem_offset
    }
    pub fn alloc_count_var(&mut self, fid: u32, pc: u32) -> u32 {
        let mem_offset = self.curr_mem_offset;
        let allocated_var = AllocatedVar::SingleCount {
            header: FuncLocHeader { fid, pc },
        };

        let len = allocated_var.num_bytes() as u32;
        self.allocated_vars.push(allocated_var);
        self.curr_mem_offset += len;
        mem_offset
    }
    pub fn alloc_i32_var(&mut self) -> u32 {
        let mem_offset = self.curr_mem_offset;
        let allocated_var = AllocatedVar::SingleI32;

        let len = allocated_var.num_bytes() as u32;
        self.allocated_vars.push(allocated_var);
        self.curr_mem_offset += len;
        mem_offset
    }
    pub fn alloc_i8_var(&mut self, fid: u32, pc: u32) -> u32 {
        let mem_offset = self.curr_mem_offset;
        let allocated_var = AllocatedVar::ReportI8 {
            header: FuncLocHeader { fid, pc },
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

struct FuncLocHeader {
    fid: u32,
    pc: u32,
}
impl FuncLocHeader {
    fn num_bytes() -> usize {
        // 0      4      8
        // | fid  |  pc  |
        size_of::<u32>() + size_of::<u32>()
    }
    pub fn encode(&self) -> Vec<u8> {
        let mut res = self.fid.to_le_bytes().to_vec();
        res.extend(self.pc.to_le_bytes());
        assert_eq!(Self::num_bytes(), res.len());

        res
    }
}

struct MultiCountHeader {
    fid: u32,
    pc: u32,
    // n is the number of entries in the table including the default target
    n: u32,
}
impl MultiCountHeader {
    fn num_bytes() -> usize {
        // 0      4      8      12        20
        // | fid  |  pc  |   n  |  0 taken |   ...  | n taken |
        Self::loc_header_size() + size_of::<u32>()
    }
    pub fn loc_header_size() -> usize {
        size_of::<u32>() + size_of::<u32>()
    }

    pub fn encode(&self) -> Vec<u8> {
        let mut res = self.fid.to_le_bytes().to_vec();
        res.extend(self.pc.to_le_bytes());
        res.extend(self.n.to_le_bytes());
        assert_eq!(Self::num_bytes(), res.len());

        res
    }
}

enum AllocatedVar {
    // 0      4
    // | i32  |
    SingleI32,
    // 0      4      8     9
    // | fid  |  pc  | val |
    ReportI8 { header: FuncLocHeader },
    // 0      4      8      16
    // | fid  |  pc  | count |
    SingleCount { header: FuncLocHeader },
    // 0      4      8      12        20
    // | fid  |  pc  |   n  |  0 taken |   ...  | n taken |
    MultiCount { header: MultiCountHeader },
}
impl AllocatedVar {
    pub fn encode(&self) -> Vec<u8> {
        match self {
            Self::SingleI32 => {
                let res = 0_i32.to_le_bytes().to_vec();
                assert_eq!(self.num_bytes(), res.len());

                res
            }
            Self::ReportI8 { header } => {
                let mut res = header.encode();
                // zero padding for single value slot
                res.extend(0_i8.to_le_bytes());
                assert_eq!(self.num_bytes(), res.len());

                res
            }
            Self::SingleCount { header } => {
                let mut res = header.encode();
                // zero padding for single value slot
                res.extend(0_i64.to_le_bytes());
                assert_eq!(self.num_bytes(), res.len());

                res
            }
            Self::MultiCount { header } => {
                let mut res = header.encode();
                // zero padding for value slots based on N
                for _ in 0..header.n {
                    res.extend(0_i64.to_le_bytes());
                }
                assert_eq!(self.num_bytes(), res.len());

                res
            }
        }
    }
    fn num_bytes(&self) -> usize {
        match self {
            AllocatedVar::SingleI32 => size_of::<i32>(),
            AllocatedVar::ReportI8 { .. } => self.full_header_size() + size_of::<u8>(),
            AllocatedVar::SingleCount { .. } => self.full_header_size() + size_of::<u64>(),
            AllocatedVar::MultiCount {
                header: MultiCountHeader { n, .. },
            } => self.full_header_size() + (*n as usize * size_of::<u64>()),
        }
    }
    pub fn full_header_size(&self) -> usize {
        match self {
            AllocatedVar::SingleI32 => 0,
            AllocatedVar::ReportI8 { .. } => FuncLocHeader::num_bytes(),
            AllocatedVar::SingleCount { .. } => FuncLocHeader::num_bytes(),
            AllocatedVar::MultiCount { .. } => MultiCountHeader::num_bytes(),
        }
    }
}
fn setup_strings(
    to_emit: Vec<String>,
    mem_id: u32,
    wasm: &mut Module,
) -> (HashMap<String, u32>, u32) {
    // not using self's memory offset, so we can start back at zero for the actual data segment injections
    // since the allocated vars are in a vector, we can depend on the ordering being correct
    // this is to keep from having a ton of data segments created. We just make a single large one for allocated variables.
    let mut mem_offset = 0;
    let mut strings = HashMap::new();
    for str in to_emit.iter() {
        let (addr, len) = add_data_segment(str.as_bytes().to_vec(), mem_id, mem_offset, wasm);
        mem_offset += len;
        strings.insert(str.to_string(), addr);
    }

    (strings, mem_offset)
}
fn add_data_segment(
    bytes: Vec<u8>,
    mem_id: u32,
    target_offset: u32,
    wasm: &mut Module,
) -> (u32, u32) {
    let len = bytes.len() as u32;
    let data = DataSegment {
        data: bytes,
        kind: DataSegmentKind::Active {
            memory_index: mem_id,
            offset_expr: InitExpr::new(vec![InitInstr::Value(I32(target_offset as i32))]),
        },
        tag: None,
    };
    wasm.add_data(data);

    (target_offset, len)
}

fn add_util_funcs(memory: &mut MemTracker, wasm: &mut Module) -> HashMap<String, FunctionID> {
    let host_funcs = [
        ("whamm_core", "puti32", vec![DataType::I32], vec![]),
        ("whamm_core", "putc", vec![DataType::I32], vec![]),
    ];

    let mut utils = HashMap::new();
    for (module, name, params, results) in host_funcs.iter() {
        let ty_id = wasm.types.add_func_type(params, results);
        let (fid, imp_id) = wasm.add_import_func(module.to_string(), name.to_string(), ty_id);
        wasm.imports.set_name(name.to_string(), imp_id);

        utils.insert(name.to_string(), fid);
    }

    let puts_fid = emit_puts(memory, &utils, wasm);
    utils.insert("puts".to_string(), puts_fid);

    utils
}

fn emit_puts(
    memory: &mut MemTracker,
    utils: &HashMap<String, FunctionID>,
    wasm: &mut Module,
) -> FunctionID {
    let start_addr = LocalID(0);
    let len = LocalID(1);
    let mut puts = FunctionBuilder::new(&[DataType::I32, DataType::I32], &[]);

    let i = puts.add_local(DataType::I32);
    let Some(putc) = utils.get("putc") else {
        panic!("Couldn't find function for 'putc'");
    };
    let putc = *putc;

    #[rustfmt::skip]
    puts.loop_stmt(BlockType::Empty)
        // Check if we've reached the end of the string
        .local_get(i)
        .local_get(len)
        .i32_lt_unsigned()
        .i32_eqz()
        .br_if(1)

        // get next char
        .local_get(start_addr)
        .local_get(i)
        .i32_add()
        // load a byte from memory
        .i32_load8_u(
            MemArg {
                align: 0,
                max_align: 0,
                offset: 0,
                memory: memory.mem_id
            }
        );

    puts.call(putc);

    // Increment i and continue loop
    puts.local_get(i)
        .i32_const(1)
        .i32_add()
        .local_set(i)
        .br(0) // (;3;)
        .end();

    let puts_fid = puts.finish_module(wasm);
    wasm.set_fn_name(puts_fid, "puts".to_string());

    puts_fid
}
fn call_flush_on_exit(flush_fn: FunctionID, wasm: &mut Module) {
    // handles:
    // - start/main func::exit
    // - wasi exit calls!
    inject_flush_on_end(flush_fn, wasm);
    inject_flush_on_wasi_exit_calls(flush_fn, wasm);
}

fn inject_flush_on_end(flush_fn: FunctionID, wasm: &mut Module) {
    let fid = if let Some(main_fid) = wasm.exports.get_func_by_name("main".to_string()) {
        main_fid
    } else if let Some(main_fid) = wasm.exports.get_func_by_name("_start".to_string()) {
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
        Operator::Call {
            function_index: fid,
        }
        | Operator::ReturnCall {
            function_index: fid,
        } => {
            let target = match wasm.functions.get_kind(FunctionID(*fid)) {
                FuncKind::Import(ImportedFunction { import_id, .. }) => {
                    let import = wasm.imports.get(*import_id);
                    let mod_name = import.module.to_string();
                    let func_name = import.name.to_string();
                    format!("{mod_name}:{func_name}")
                }
                FuncKind::Local(local_func) => {
                    let mod_name = match &wasm.module_name {
                        Some(name) => name.clone(),
                        None => "".to_string(),
                    };
                    let func_name = wasm
                        .functions
                        .get_name(local_func.func_id)
                        .clone()
                        .unwrap_or_default();
                    format!("{mod_name}:{func_name}")
                }
            };
            let exiting_call = HashSet::from(["wasi_snapshot_preview1:proc_exit".to_string()]);
            exiting_call.contains(&target)
        }
        _ => false,
    }
}
fn import_lib_func(
    lib_name: &str,
    lib_func: &str,
    params: &[DataType],
    results: &[DataType],
    wasm: &mut Module,
) -> FunctionID {
    let ty_id = wasm.types.add_func_type(params, results);
    let (fid, imp_id) = wasm.add_import_func(lib_name.to_string(), lib_func.to_string(), ty_id);
    wasm.imports.set_name(lib_func.to_string(), imp_id);

    fid
}

/// Returns values in the order that they should be replaced on the stack!
fn bundle_load_store_args(
    val_dt: Option<DataType>,
    static_offset: u64,
    wasm: &mut ModuleIterator,
    locals: &mut LocalsTracker,
) -> Vec<LocalID> {
    let orig_stack_vals;

    let addr = if let Some(dt) = val_dt {
        let addr = LocalID(locals.use_local(DataType::I32, wasm));
        let val = LocalID(locals.use_local(dt, wasm));

        wasm.local_set(val).local_set(addr);

        orig_stack_vals = vec![addr, val];
        addr
    } else {
        let addr = LocalID(locals.use_local(DataType::I32, wasm));

        wasm.local_set(addr);

        orig_stack_vals = vec![addr];
        addr
    };

    // the effective address
    wasm.local_get(addr);
    if static_offset > 0 {
        wasm.u32_const(static_offset as u32).i32_add();
    }

    orig_stack_vals
}
fn fix_stack(wasm: &mut ModuleIterator, orig_stack_vals: &Vec<LocalID>) {
    for local in orig_stack_vals {
        wasm.local_get(*local);
    }
}
