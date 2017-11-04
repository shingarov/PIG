
/**** INSTRUCTION SEMANTICS ****/

transfer(gpr(Rdst), uword(D, Nbits)) => ori(0,Rdst,D) :-
        Nbits =< 16.

transfer(gpr(Rdst), uword(D, Nbits)<<16) => oris(0,Rdst,D) :-
        Nbits =< 16.

transfer(gpr(Rdst), gpr(Rsrc)) => ore(Rdst,0,Rsrc).

transfer(gpr(Rdst),sword(D, Nbits)) => orrrrri(Rdst,0,D) :-
        Nbits =< 16.


/* hypotetic load / store register-addressed */
transfer(gpr(Rdst), memref(gpr(Rbase))) => l(Rdst,Rbase).
transfer(memref(gpr(Rbase)), gpr(Rsrc)) => s(Rsrc,Rbase).



/*
transfer(Op2,Op3) => ori(Op1,Op2,Op3) :-
  Op1 = 0,
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  true.


transfer(Op1,or(Op2,Op3)) => ori(Op1,Op2,Op3) :-
  is_GPR(Op1),
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  true.


transfer(Op1,Op3) => addi(Op1,Op2,Op3) :-
  Op2 = 0,
  is_GPR(Op1),
  integer(Op3), Op3 < 65536,
  true.


transfer(Op1,+(Op2,Op3)) => addi(Op1,Op2,Op3) :-
  is_GPR(Op1),
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  true.


transfer(memref(+(Op2,Op3)),Op1) => stw(Op1,Op2,Op3) :-
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  is_GPR(Op1),
  true.


transfer(Op1,memref(+(Op2,Op3))) => lwz(Op1,Op2,Op3) :-
  is_GPR(Op1),
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  true.


transfer(Op1,or(Op2,Op3)) => ore(Op1,Op2,Op3) :-
  is_GPR(Op1),
  is_GPR(Op2),
  is_GPR(Op3),
  true.


transfer(Op1,shl(Op2,Op3)) => rlwinm(Op1,Op2,Op3,Op4,Op5) :-
  Op5 = 31,
  Op4 = 0,
  is_GPR(Op1),
  is_GPR(Op2),
  integer(Op3), Op3 < 65536,
  true.
*/

/**** EMD OF INSTRUCTION SEMANTICS ****/


/* ALGEBRA */

transfer(gpr(N), uword(X,32)) => [Lo, Hi] :-
        e(transfer(gpr(N), uword(low16(X),16)), Lo),
        e(transfer(gpr(N), uword(hi16(X),16)<<16),  Hi).

/* abstract shift */
/*
transfer(Rd, (V << S)) => [X,Y] :-
                integer(V), is_GPR(Rd), is_GPR(Rtmp),
                        e(transfer(Rtmp,V), X), e(transfer(Rd,shl(Rtmp,S)),Y).

transfer(R,S) => transfer(R,+(S,0)) :- is_GPR(R), is_GPR(S).
*/

/* abstract or */
transfer(Rd, Ximm \/ Yimm) => [X,Y,Z] :-
        integer(Ximm), integer(Yimm),
        is_GPR(Rd), is_GPR(R1), is_GPR(R2),
        e(transfer(R1,Ximm),X), e(transfer(R2,Yimm),Y), e(transfer(Rd,or(R1,R2)),Z).
/* Integer decomposition */
transfer(R,Imm) => transfer(R,(Hi << 16)) :- integer(Imm), (Imm/\65535) =:= 0, Hi is (Imm >> 16). /* easy degenerate case, Lo==0 */
transfer(R,Imm) => transfer(R, Hi \/ Lo) :- integer(Imm), Hi is (Imm /\ 0xFFFF0000), Lo is (Imm /\ 0xFFFF).


/* we ran out of primitive transfers, try composite */
/*
transfer(A,C) => [Y,X] :- is_GPR(B), e(transfer(A,B), X), e(transfer(B,C), Y).
In GENLLVMBE, this is much less aggressive,
only talking about memory->memory transfers via a register:
*/
transfer(memref(M), memref(N)) => [I,J] :-
        e(transfer(gpr(availableGPR(TMP)), memref(N)), I),
        e(transfer(memref(M), gpr(availableGPR(TMP))), J).
