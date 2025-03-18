use orca_wasm::ir::types::{InitExpr, Value};
use orca_wasm::{DataType, Instructions, Module, Opcode};
use orca_wasm::ir::id::GlobalID;
use orca_wasm::iterator::iterator_trait::{IteratingInstrumenter, Iterator};
use orca_wasm::iterator::module_iterator::ModuleIterator;
use wasmparser::Operator;

struct Globals {
    _const: GlobalID,
    misc: GlobalID,
    control: GlobalID,
    local: GlobalID,
    global: GlobalID,
    table: GlobalID,
    load: GlobalID,
    store: GlobalID,
    mem: GlobalID,
    arith: GlobalID,
    compare: GlobalID,
    convert: GlobalID,
    exn: GlobalID,
    gc: GlobalID,
    atomic: GlobalID,
}
impl Globals {
    pub fn new(wasm: &mut Module) -> Self {
        Self {
            _const: add_global(wasm),
            misc: add_global(wasm),
            control: add_global(wasm),
            local: add_global(wasm),
            global: add_global(wasm),
            table: add_global(wasm),
            load: add_global(wasm),
            store: add_global(wasm),
            mem: add_global(wasm),
            arith: add_global(wasm),
            compare: add_global(wasm),
            convert: add_global(wasm),
            exn: add_global(wasm),
            gc: add_global(wasm),
            atomic: add_global(wasm),
        }
    }
}

/// Adds instruction mix instrumentation logic to a module.
///     1. Add one global per instruction class
///     2. Instrument each opcode and increment the appropriate global.
///     3. TODO: Figure out how to print the output at the end.
pub fn instrument(mut wasm: Module) -> Module {
    let globals = Globals::new(&mut wasm);
    let mut mod_it = ModuleIterator::new(&mut wasm, &vec![]);

    inject_instrumentation(&mut mod_it, &globals);

    wasm
}

fn inject_instrumentation(wasm: &mut ModuleIterator, globals: &Globals) {
    loop {
        if let Some(op) = wasm.curr_op() {
            match op {
                Operator::Unreachable |
                Operator::Nop |
                Operator::Drop => count_misc(wasm, globals),
                Operator::Block { .. } |
                Operator::Loop { .. } |
                Operator::If { .. } |
                Operator::Else |
                Operator::End |
                Operator::Br { .. } |
                Operator::BrIf { .. } |
                Operator::BrTable { .. } |
                Operator::Return |
                Operator::Call { .. } |
                Operator::CallIndirect { .. } |
                Operator::Select => count_control(wasm, globals),
                Operator::LocalGet { .. } |
                Operator::LocalSet { .. } |
                Operator::LocalTee { .. } => count_local(wasm, globals),
                Operator::GlobalGet { .. } |
                Operator::GlobalSet { .. } => count_global(wasm, globals),
                Operator::I32Load { .. } |
                Operator::I64Load { .. } |
                Operator::F32Load { .. } |
                Operator::F64Load { .. } |
                Operator::I32Load8S { .. } |
                Operator::I32Load8U { .. } |
                Operator::I32Load16S { .. } |
                Operator::I32Load16U { .. } |
                Operator::I64Load8S { .. } |
                Operator::I64Load8U { .. } |
                Operator::I64Load16S { .. } |
                Operator::I64Load16U { .. } |
                Operator::I64Load32S { .. } |
                Operator::I64Load32U { .. } => count_load(wasm, globals),
                Operator::I32Store { .. } |
                Operator::I64Store { .. } |
                Operator::F32Store { .. } |
                Operator::F64Store { .. } |
                Operator::I32Store8 { .. } |
                Operator::I32Store16 { .. } |
                Operator::I64Store8 { .. } |
                Operator::I64Store16 { .. } |
                Operator::I64Store32 { .. } => count_store(wasm, globals),
                Operator::MemorySize { .. } |
                Operator::MemoryGrow { .. } => count_mem(wasm, globals),
                Operator::I32Const { .. } |
                Operator::I64Const { .. } |
                Operator::F32Const { .. } |
                Operator::F64Const { .. } => count_const(wasm, globals),
                Operator::I32Eqz |
                Operator::I32Eq |
                Operator::I32Ne |
                Operator::I32LtS |
                Operator::I32LtU |
                Operator::I32GtS |
                Operator::I32GtU |
                Operator::I32LeS |
                Operator::I32LeU |
                Operator::I32GeS |
                Operator::I32GeU |
                Operator::I64Eqz |
                Operator::I64Eq |
                Operator::I64Ne |
                Operator::I64LtS |
                Operator::I64LtU |
                Operator::I64GtS |
                Operator::I64GtU |
                Operator::I64LeS |
                Operator::I64LeU |
                Operator::I64GeS |
                Operator::I64GeU |
                Operator::F32Eq |
                Operator::F32Ne |
                Operator::F32Lt |
                Operator::F32Gt |
                Operator::F32Le |
                Operator::F32Ge |
                Operator::F64Eq |
                Operator::F64Ne |
                Operator::F64Lt |
                Operator::F64Gt |
                Operator::F64Le |
                Operator::F64Ge => count_compare(wasm, globals),
                Operator::I32Clz |
                Operator::I32Ctz |
                Operator::I32Popcnt |
                Operator::I32Add |
                Operator::I32Sub |
                Operator::I32Mul |
                Operator::I32DivS |
                Operator::I32DivU |
                Operator::I32RemS |
                Operator::I32RemU |
                Operator::I32And |
                Operator::I32Or |
                Operator::I32Xor |
                Operator::I32Shl |
                Operator::I32ShrS |
                Operator::I32ShrU |
                Operator::I32Rotl |
                Operator::I32Rotr |
                Operator::I64Clz |
                Operator::I64Ctz |
                Operator::I64Popcnt |
                Operator::I64Add |
                Operator::I64Sub |
                Operator::I64Mul |
                Operator::I64DivS |
                Operator::I64DivU |
                Operator::I64RemS |
                Operator::I64RemU |
                Operator::I64And |
                Operator::I64Or |
                Operator::I64Xor |
                Operator::I64Shl |
                Operator::I64ShrS |
                Operator::I64ShrU |
                Operator::I64Rotl |
                Operator::I64Rotr |
                Operator::F32Abs |
                Operator::F32Neg |
                Operator::F32Ceil |
                Operator::F32Floor |
                Operator::F32Trunc |
                Operator::F32Nearest |
                Operator::F32Sqrt |
                Operator::F32Add |
                Operator::F32Sub |
                Operator::F32Mul |
                Operator::F32Div |
                Operator::F32Min |
                Operator::F32Max |
                Operator::F32Copysign |
                Operator::F64Abs |
                Operator::F64Neg |
                Operator::F64Ceil |
                Operator::F64Floor |
                Operator::F64Trunc |
                Operator::F64Nearest |
                Operator::F64Sqrt |
                Operator::F64Add |
                Operator::F64Sub |
                Operator::F64Mul |
                Operator::F64Div |
                Operator::F64Min |
                Operator::F64Max |
                Operator::F64Copysign => count_arith(wasm, globals),
                Operator::I32WrapI64 |
                Operator::I32TruncF32S |
                Operator::I32TruncF32U |
                Operator::I32TruncF64S |
                Operator::I32TruncF64U |
                Operator::I64ExtendI32S |
                Operator::I64ExtendI32U |
                Operator::I64TruncF32S |
                Operator::I64TruncF32U |
                Operator::I64TruncF64S |
                Operator::I64TruncF64U |
                Operator::F32ConvertI32S |
                Operator::F32ConvertI32U |
                Operator::F32ConvertI64S |
                Operator::F32ConvertI64U |
                Operator::F32DemoteF64 |
                Operator::F64ConvertI32S |
                Operator::F64ConvertI32U |
                Operator::F64ConvertI64S |
                Operator::F64ConvertI64U |
                Operator::F64PromoteF32 |
                Operator::I32ReinterpretF32 |
                Operator::I64ReinterpretF64 |
                Operator::F32ReinterpretI32 |
                Operator::F64ReinterpretI64 |
                Operator::I32Extend8S |
                Operator::I32Extend16S |
                Operator::I64Extend8S |
                Operator::I64Extend16S |
                Operator::I64Extend32S => count_convert(wasm, globals),
                Operator::RefEq |
                Operator::StructNew { .. } |
                Operator::StructNewDefault { .. } |
                Operator::StructGet { .. } |
                Operator::StructGetS { .. } |
                Operator::StructGetU { .. } |
                Operator::StructSet { .. } |
                Operator::ArrayNew { .. } |
                Operator::ArrayNewDefault { .. } |
                Operator::ArrayNewFixed { .. } |
                Operator::ArrayNewData { .. } |
                Operator::ArrayNewElem { .. } |
                Operator::ArrayGet { .. } |
                Operator::ArrayGetS { .. } |
                Operator::ArrayGetU { .. } |
                Operator::ArraySet { .. } |
                Operator::ArrayLen |
                Operator::ArrayFill { .. } |
                Operator::ArrayCopy { .. } |
                Operator::ArrayInitData { .. } |
                Operator::ArrayInitElem { .. } |
                Operator::RefTestNonNull { .. } |
                Operator::RefTestNullable { .. } |
                Operator::RefCastNonNull { .. } |
                Operator::AnyConvertExtern |
                Operator::ExternConvertAny |
                Operator::RefI31 |
                Operator::I31GetS |
                Operator::I31GetU |
                Operator::RefNull { .. } |
                Operator::RefIsNull |
                Operator::RefFunc { .. } |
                Operator::RefCastNullable { .. } => count_gc(wasm, globals),
                Operator::BrOnCast { .. } |
                Operator::BrOnCastFail { .. } => count_control(wasm, globals),
                Operator::I32TruncSatF32S |
                Operator::I32TruncSatF32U |
                Operator::I32TruncSatF64S |
                Operator::I32TruncSatF64U |
                Operator::I64TruncSatF32S |
                Operator::I64TruncSatF32U |
                Operator::I64TruncSatF64S |
                Operator::I64TruncSatF64U => count_convert(wasm, globals),
                Operator::MemoryInit { .. } |
                Operator::DataDrop { .. } |
                Operator::MemoryCopy { .. } |
                Operator::MemoryDiscard { .. } |
                Operator::MemoryFill { .. } => count_mem(wasm, globals),
                Operator::ElemDrop { .. } |
                Operator::TableInit { .. } |
                Operator::TableFill { .. } |
                Operator::TableGet { .. } |
                Operator::TableSet { .. } |
                Operator::TableGrow { .. } |
                Operator::TableSize { .. } |
                Operator::TryTable { .. } |
                Operator::TableCopy { .. } => count_table(wasm, globals),
                Operator::ReturnCall { .. } |
                Operator::ReturnCallIndirect { .. } |
                Operator::TypedSelect { .. } => count_control(wasm, globals),
                Operator::MemoryAtomicNotify { .. } |
                Operator::MemoryAtomicWait32 { .. } |
                Operator::MemoryAtomicWait64 { .. } |
                Operator::AtomicFence |
                Operator::I32AtomicLoad { .. } |
                Operator::I64AtomicLoad { .. } |
                Operator::I32AtomicLoad8U { .. } |
                Operator::I32AtomicLoad16U { .. } |
                Operator::I64AtomicLoad8U { .. } |
                Operator::I64AtomicLoad16U { .. } |
                Operator::I64AtomicLoad32U { .. } |
                Operator::I32AtomicStore { .. } |
                Operator::I64AtomicStore { .. } |
                Operator::I32AtomicStore8 { .. } |
                Operator::I32AtomicStore16 { .. } |
                Operator::I64AtomicStore8 { .. } |
                Operator::I64AtomicStore16 { .. } |
                Operator::I64AtomicStore32 { .. } |
                Operator::I32AtomicRmwAdd { .. } |
                Operator::I64AtomicRmwAdd { .. } |
                Operator::I32AtomicRmw8AddU { .. } |
                Operator::I32AtomicRmw16AddU { .. } |
                Operator::I64AtomicRmw8AddU { .. } |
                Operator::I64AtomicRmw16AddU { .. } |
                Operator::I64AtomicRmw32AddU { .. } |
                Operator::I32AtomicRmwSub { .. } |
                Operator::I64AtomicRmwSub { .. } |
                Operator::I32AtomicRmw8SubU { .. } |
                Operator::I32AtomicRmw16SubU { .. } |
                Operator::I64AtomicRmw8SubU { .. } |
                Operator::I64AtomicRmw16SubU { .. } |
                Operator::I64AtomicRmw32SubU { .. } |
                Operator::I32AtomicRmwAnd { .. } |
                Operator::I64AtomicRmwAnd { .. } |
                Operator::I32AtomicRmw8AndU { .. } |
                Operator::I32AtomicRmw16AndU { .. } |
                Operator::I64AtomicRmw8AndU { .. } |
                Operator::I64AtomicRmw16AndU { .. } |
                Operator::I64AtomicRmw32AndU { .. } |
                Operator::I32AtomicRmwOr { .. } |
                Operator::I64AtomicRmwOr { .. } |
                Operator::I32AtomicRmw8OrU { .. } |
                Operator::I32AtomicRmw16OrU { .. } |
                Operator::I64AtomicRmw8OrU { .. } |
                Operator::I64AtomicRmw16OrU { .. } |
                Operator::I64AtomicRmw32OrU { .. } |
                Operator::I32AtomicRmwXor { .. } |
                Operator::I64AtomicRmwXor { .. } |
                Operator::I32AtomicRmw8XorU { .. } |
                Operator::I32AtomicRmw16XorU { .. } |
                Operator::I64AtomicRmw8XorU { .. } |
                Operator::I64AtomicRmw16XorU { .. } |
                Operator::I64AtomicRmw32XorU { .. } |
                Operator::I32AtomicRmwXchg { .. } |
                Operator::I64AtomicRmwXchg { .. } |
                Operator::I32AtomicRmw8XchgU { .. } |
                Operator::I32AtomicRmw16XchgU { .. } |
                Operator::I64AtomicRmw8XchgU { .. } |
                Operator::I64AtomicRmw16XchgU { .. } |
                Operator::I64AtomicRmw32XchgU { .. } |
                Operator::I32AtomicRmwCmpxchg { .. } |
                Operator::I64AtomicRmwCmpxchg { .. } |
                Operator::I32AtomicRmw8CmpxchgU { .. } |
                Operator::I32AtomicRmw16CmpxchgU { .. } |
                Operator::I64AtomicRmw8CmpxchgU { .. } |
                Operator::I64AtomicRmw16CmpxchgU { .. } |
                Operator::I64AtomicRmw32CmpxchgU { .. } => count_atomic(wasm, globals),
                Operator::Throw { .. } |
                Operator::ThrowRef => count_exn(wasm, globals),
                Operator::BrOnNull { .. } |
                Operator::BrOnNonNull { .. } |
                Operator::CallRef { .. } |
                Operator::ReturnCallRef { .. } => count_control(wasm, globals),
                Operator::RefAsNonNull => count_convert(wasm, globals),
                _ => {}
            }
        }

        if wasm.next().is_none() {
            break;
        };
    }
}

// ==== HELPERS ====

fn count(wasm: &mut ModuleIterator, global: GlobalID) {
    wasm.before();

    wasm.global_get(global)
        .i32_const(1)
        .i32_add()
        .global_set(global);
}

fn count_const(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals._const);
}
fn count_misc(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.misc);
}
fn count_control(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.control);
}
fn count_local(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.local);
}
fn count_global(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.global);
}
fn count_table(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.table);
}
fn count_load(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.load);
}
fn count_store(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.store);
}
fn count_mem(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.mem);
}
fn count_arith(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.arith);
}
fn count_compare(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.compare);
}
fn count_convert(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.convert);
}
fn count_exn(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.exn);
}
fn count_gc(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.gc);
}
fn count_atomic(wasm: &mut ModuleIterator, globals: &Globals) {
    count(wasm, globals.atomic);
}

fn add_global(wasm: &mut Module) -> GlobalID {
    wasm.add_global(
        InitExpr::new(vec![Instructions::Value(Value::I32(0))]),
        DataType::I32,
        true,
        false,
    )
}