section .text
infLoop:
  IL_begin:
      inc ax
  jmp IL_begin
  IL_end:
leave
ret

section .text
main:
  JL_begin:
      inc ax
      cmp ax, 5
      jg BL_begin
      jmp BL_end
      BL_begin:
          jmp JL_end
      BL_end:
  jmp JL_begin
  JL_end:
  nop
leave
ret

