section .text
justFunc:
  nop
leave
ret

section .text
callFunction:
  call justFunc
  add sp, 0
  nop
leave
ret

