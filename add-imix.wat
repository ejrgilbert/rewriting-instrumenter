(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0)
    global.get 2
    i32.const 1
    i32.add
    global.set 2
  )
  (func (;1;) (type 0)
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    call 0
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    call 0
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    call 0
    global.get 2
    i32.const 1
    i32.add
    global.set 2
  )
  (memory (;0;) 1)
  (global (;0;) (mut i32) i32.const 0)
  (global (;1;) (mut i32) i32.const 0)
  (global (;2;) (mut i32) i32.const 0)
  (global (;3;) (mut i32) i32.const 0)
  (global (;4;) (mut i32) i32.const 0)
  (global (;5;) (mut i32) i32.const 0)
  (global (;6;) (mut i32) i32.const 0)
  (global (;7;) (mut i32) i32.const 0)
  (global (;8;) (mut i32) i32.const 0)
  (global (;9;) (mut i32) i32.const 0)
  (global (;10;) (mut i32) i32.const 0)
  (global (;11;) (mut i32) i32.const 0)
  (global (;12;) (mut i32) i32.const 0)
  (global (;13;) (mut i32) i32.const 0)
  (global (;14;) (mut i32) i32.const 0)
  (start 1)
)
