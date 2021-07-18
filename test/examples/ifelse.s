section .text
ifelse_eq:
  cmp ax, 5
  je A_begin
      inc ax
  jmp A_end
  A_begin:
      dec ax
  A_end:
  nop
leave
ret

section .text
ifelse_comp:
  cmp ax, 5
  jl B_begin
      inc ax
  jmp B_end
  B_begin:
      dec ax
  B_end:
  nop
leave
ret

