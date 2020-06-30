# HLasm

Improve your assembly experience with HLasm!
This language is more comfortable than a pure assembly.

## Example

Instead of documentation

### Frames

```asm
program {
    fake frame <act>

    frame (main) {
        reg a : ax
        a = 5
        call <act> (a)
    }

    frame (act) {
        !mov ax, [bp-4];
        !inc ax;
    }
}
```

Compiled

```asm
section .text

main:
    push bp
    mov bp, sp
    sub sp, 0
    
    mov ax, 5
    push ax
    call act
    
    add sp, 0
leave
ret

act:
    push bp
    mov bp, sp
    sub sp, 0
    mov ax, [bp-4]
    inc ax
leave
ret
```

### Variables

```asm
program {
    var a : int(16) = 32
    var b : int(8) = 1
    const c : int(16) = 32
    const d : int(8) = 1

    frame (main) {
        reg ra : ax
        reg ptr : bx
        var sb : int(16)
        ra = 5
        sb = 6
        ptr = b
    }
}
```

Compiled

```asm
section .text

main:
    push bp
    mov bp, sp
    
    sub sp, 2
    mov ax, 5
    mov WORD [bp-2], 6
    mov bx, b
leave
ret

section .data
a: DW 32
b: DB 1

section .rodata
c: DW 32
d: DB 1
```

### Conditions and loops

```asm
program {
    frame (main) {
        reg val : ax
        val = 0

        while (LOOP) {
            if (val > 5, CMP) {
                break (LOOP)
            }
            !inc ax;
        }
    }
}
```

Compiled

```asm
section .text

main:
push bp
    mov bp, sp
    sub sp, 0
    mov ax, 0
    LOOPbegin:
        cmp ax, 5
        jg CMP1
            jmp CMPend
        CMP1:
            jmp LOOPend
            jmp CMPend
        CMPend:

        inc ax
    jmp LOOPbegin
    LOOPend:
leave
ret
```

> Generated code in these examples was manually formatted.

## Build and install

Build using [stack](https://docs.haskellstack.org).

```
stack build
```

Install using [stack](https://docs.haskellstack.org).

```
stack install
```

## Run HLasm

It compiles each file individually and prints nasm to the terminal.

```sh
HLasm ./file1 ./file2 ./file3
```
