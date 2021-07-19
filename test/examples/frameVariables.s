section .text
register:
  mov ax, 5
  mov bx, ax
leave
ret

section .text
stackNreg:
  mov word [bp - 2], 5
  mov ax, [bp - 2]
  mov word [bp - 2], ax
  mov ax, 6
leave
ret

