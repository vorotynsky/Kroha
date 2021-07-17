section .text
if_eq:
  cmp ax, 5
  je A_begin
  jmp A_end
  A_begin:
      inc ax
  A_end:
  nop
leave
ret

section .text
if_comp:
  cmp ax, 5
  jg B_begin
  jmp B_end
  B_begin:
      add ax, 5
  B_end:
  cmp ax, 7
  jl C_begin
  jmp C_end
  C_begin:
      sub ax, 7
  C_end:
  nop
leave
ret

