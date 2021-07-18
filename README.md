# Kroha

Improve your assembly experience with Kroha!
This language is more comfortable than a pure assembly.

## Examples

Instead of documentation

You can find [more examples here](./test/examples).

### Frames

```asm
program {
  manual frame act {
    mov ax, [bp-4]
    inc ax
    leave
    ret
  }

  frame main {
    reg a : ax
    a = 5
    call <act> (a)
  }
}
```

Compiled

```asm
section .text
act:
  mov ax, [bp-4]
  inc ax
  leave
  ret

section .text
main:
  mov ax, 5
  push ax
  call act
  add sp, 2
leave
ret

```

### Variables

```asm
program {
  var a : int16 = 32
  var b : int8 = 1
  const c : int16 = 32
  const d : int8 = 1

  manual var arr : &int8 {
    times 64 db 0
  }

  frame main {
    reg ra : ax
    reg ptr : bx
    var sb : int16
    ra = 5
    sb = 6
    ptr = b
  }
}
```

Compiled

```asm
section .data
a: dw 32

section .data
b: db 1

section .rodata
c: dw 32

section .rodata
d: db 1

section .data
arr: 
  times 64 db 0
    

section .text
main:
  mov ax, 5
  mov [bp - 2], 6
  mov bx, [b]
leave
ret
```

### Conditions and loops

```asm
program {
  frame main {
    reg val : ax
    val = 0

    loop (LOOP) {
      if (val > 5, CMP) {
        break (LOOP)
      }
      else {
        !dec bx
      }
      !inc ax
    }
  }
}
```

Compiled

```asm
section .text
main:
  mov ax, 0
  LOOP_begin:
    cmp ax, 5
    jg CMP_begin
      dec bx
    jmp CMP_end
    CMP_begin:
      jmp LOOP_end
    CMP_end:
    inc ax
  jmp LOOP_begin
  LOOP_end:
leave
ret
```

Check [more examples](./test/examples).


## Build and install

Build using [stack](https://docs.haskellstack.org).

```
stack build
```

Install using [stack](https://docs.haskellstack.org).

```
stack install
```

## Run Kroha

It compiles each file individually and prints nasm to the terminal.

```sh
Kroha ./file1 ./file2 ./file3
```
