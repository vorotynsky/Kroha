section .text
plot:
  cmp ax, 0
  jg XP_begin
      cmp ax, 0
      jl XP2_begin
          cmp bx, 0
          je O_begin
              mov dx, 10
          jmp O_end
          O_begin:
              mov dx, 0
          O_end:
      jmp XP2_end
      XP2_begin:
          cmp bx, 0
          jg YP3_begin
              cmp bx, 0
              jl YP4_begin
                  mov dx, 10
              jmp YP4_end
              YP4_begin:
                  mov dx, 3
              YP4_end:
          jmp YP3_end
          YP3_begin:
              mov dx, 2
          YP3_end:
      XP2_end:
  jmp XP_end
  XP_begin:
      cmp bx, 0
      jg YP_begin
          cmp bx, 0
          jl YP2_begin
              mov dx, 20
          jmp YP2_end
          YP2_begin:
              mov dx, 4
          YP2_end:
      jmp YP_end
      YP_begin:
          mov dx, 1
      YP_end:
  XP_end:
leave
ret

