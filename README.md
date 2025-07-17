# WebAssembly Bytecode Instrumenter

Injects instrumentation bytecode directly into Wasm bytecode to perform some simple dynamic analyses. Built using [Orca](https://github.com/thesuhas/orca), a Wasm transformation library.

This was implemented as part of an experiment comparing various instrumentation implementations with the [Whamm](https://github.com/ejrgilbert/whamm) DSL.

### Monitors

- **Hotness monitor**: Inserts counting bytecode at every instruction and then produces a summary of hot execution paths.

- **Branch monitor**: Instruments all `if`, `br_if`, and `br_table` instructions in the program and uses the top-of-stack to predict the direction each branch will take.

- **Instruction Mix monitor**: Instruments all opcodes and counts the number of times each category of monitor is executed dynamically.

- **Cache Simulator monitor**: Simulates a cache by performing a cache lookup at each `load`/`store` Wasm opcode. Must be run with the [cache_simulator](https://github.com/ejrgilbert/whamm/tree/master/user_libs/cache) and [whamm_core](https://github.com/ejrgilbert/whamm/tree/master/whamm_core) libraries.

- **Memory Access Tracer**: Traces the memory accesses that the application performs and tracks the number of times a specific address is accessed along with whether it was a write operation. Must be run with the [whamm_core](https://github.com/ejrgilbert/whamm/tree/master/whamm_core) library.

### Usage

```bash
./rewriting-monitor <monitor> <filename>
```

This should generate a new Wasm program injected with instrumentation bytecode that you can run using
any Wasm engine that supports [multi-memory](https://github.com/WebAssembly/multi-memory) as it uses a separate memory region to store counts.

To run, you must link with the [whamm core library](https://github.com/ejrgilbert/whamm/tree/master/whamm_core) (provides the ability to print to the console using WASI).
For the cache, you also need to link with the [cache library](https://github.com/ejrgilbert/whamm/tree/master/user_libs/cache).

Example run (cache simulator):
```
wizeng -ext:multi-memory --env=TO_CONSOLE=true   ../git/whamm/whamm_core/target/wasm32-wasip1/release/whamm_core.wasm ../git/whamm/user_libs/cache/target/wasm32-wasip1/release/cache.wasm ./malloc_init-cache.wasm
```