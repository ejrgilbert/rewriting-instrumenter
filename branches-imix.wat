(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func))
  (func $basic_br (;0;) (type 0) (param i32) (result i32)
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    block $eq
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      block $neq
        global.get 5
        i32.const 1
        i32.add
        global.set 5
        local.get 0
        global.get 2
        i32.const 1
        i32.add
        global.set 2
        i32.const 1
        global.get 12
        i32.const 1
        i32.add
        global.set 12
        i32.eq
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        br_if $eq
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        br $neq
        global.get 4
        i32.const 1
        i32.add
        global.set 4
      end
      global.get 2
      i32.const 1
      i32.add
      global.set 2
      i32.const 0
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      return
      global.get 4
      i32.const 1
      i32.add
      global.set 4
    end
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    return
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $more_nesting (;1;) (type 0) (param i32) (result i32)
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    block $gt
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      block $neq
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        block $eq
          global.get 5
          i32.const 1
          i32.add
          global.set 5
          local.get 0
          global.get 2
          i32.const 1
          i32.add
          global.set 2
          i32.const 0
          global.get 12
          i32.const 1
          i32.add
          global.set 12
          i32.eq
          global.get 4
          i32.const 1
          i32.add
          global.set 4
          br_if $eq
          global.get 5
          i32.const 1
          i32.add
          global.set 5
          local.get 0
          global.get 2
          i32.const 1
          i32.add
          global.set 2
          i32.const 2
          global.get 12
          i32.const 1
          i32.add
          global.set 12
          i32.gt_u
          global.get 4
          i32.const 1
          i32.add
          global.set 4
          br_if $gt
          global.get 4
          i32.const 1
          i32.add
          global.set 4
          br $neq
          global.get 4
          i32.const 1
          i32.add
          global.set 4
        end
        global.get 2
        i32.const 1
        i32.add
        global.set 2
        i32.const 1
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        return
        global.get 4
        i32.const 1
        i32.add
        global.set 4
      end
      global.get 2
      i32.const 1
      i32.add
      global.set 2
      i32.const 0
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      return
      global.get 4
      i32.const 1
      i32.add
      global.set 4
    end
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 2
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    return
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $br_table (;2;) (type 0) (param i32) (result i32)
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    block $two
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      block $one
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        block $zero
          global.get 5
          i32.const 1
          i32.add
          global.set 5
          local.get 0
          global.get 4
          i32.const 1
          i32.add
          global.set 4
          br_table $zero $one $two $two
          global.get 4
          i32.const 1
          i32.add
          global.set 4
        end
        global.get 2
        i32.const 1
        i32.add
        global.set 2
        i32.const 0
        global.get 4
        i32.const 1
        i32.add
        global.set 4
        return
        global.get 4
        i32.const 1
        i32.add
        global.set 4
      end
      global.get 2
      i32.const 1
      i32.add
      global.set 2
      i32.const 1
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      return
      global.get 4
      i32.const 1
      i32.add
      global.set 4
    end
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 2
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $if_stmt (;3;) (type 0) (param i32) (result i32)
    global.get 5
    i32.const 1
    i32.add
    global.set 5
    local.get 0
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 12
    i32.const 1
    i32.add
    global.set 12
    i32.eq
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    if ;; label = @1
      global.get 2
      i32.const 1
      i32.add
      global.set 2
      i32.const 1
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      return
      global.get 4
      i32.const 1
      i32.add
      global.set 4
    else
      global.get 2
      i32.const 1
      i32.add
      global.set 2
      i32.const 0
      global.get 4
      i32.const 1
      i32.add
      global.set 4
      return
      global.get 4
      i32.const 1
      i32.add
      global.set 4
    end
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $select_stmt (;4;) (type 0) (param i32) (result i32)
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 5
    i32.const 1
    i32.add
    global.set 5
    local.get 0
    global.get 12
    i32.const 1
    i32.add
    global.set 12
    i32.eqz
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    select
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $main (;5;) (type 1)
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $basic_br
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.set $var0
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $more_nesting
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.get $var1
    global.get 11
    i32.const 1
    i32.add
    global.set 11
    i32.add
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.set $var1
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $more_nesting
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.get $var1
    global.get 11
    i32.const 1
    i32.add
    global.set 11
    i32.add
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.set $var1
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $more_nesting
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.get $var1
    global.get 11
    i32.const 1
    i32.add
    global.set 11
    i32.add
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.set $var1
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 3
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $more_nesting
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.get $var1
    global.get 11
    i32.const 1
    i32.add
    global.set 11
    i32.add
    global.get 6
    i32.const 1
    i32.add
    global.set 6
    global.set $var1
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $br_table
    global.get 3
    i32.const 1
    i32.add
    global.set 3
    drop
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 1
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $br_table
    global.get 3
    i32.const 1
    i32.add
    global.set 3
    drop
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 2
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $br_table
    global.get 3
    i32.const 1
    i32.add
    global.set 3
    drop
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $if_stmt
    global.get 3
    i32.const 1
    i32.add
    global.set 3
    drop
    global.get 2
    i32.const 1
    i32.add
    global.set 2
    i32.const 0
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $select_stmt
    global.get 3
    i32.const 1
    i32.add
    global.set 3
    drop
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (func $start (;6;) (type 1)
    global.get 4
    i32.const 1
    i32.add
    global.set 4
    call $main
    global.get 4
    i32.const 1
    i32.add
    global.set 4
  )
  (global $var0 (;0;) (mut i32) i32.const 0)
  (global $var1 (;1;) (mut i32) i32.const 0)
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
  (global (;15;) (mut i32) i32.const 0)
  (global (;16;) (mut i32) i32.const 0)
  (export "main" (func $main))
  (export "_start" (func $start))
)
