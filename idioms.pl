
idiom_pattern(
    movRR(Rdst, Rsrc),
    transfer(gpr(Rdst), gpr(Rsrc))
).

idiom_pattern(
    movSI16R(D,Rdst),
    transfer(gpr(Rdst), uword(D,16))
).

idiom_pattern(
    movI32R(X,Rdst),
    transfer(gpr(Rdst), uword(X,32))
).

