#lang datalog

type(z, tz).

listof(tz, ez).

int(ez).

casepair(z, za, tza, zb, tzb).

pair(TA, TE, null) :-
    casepair(V, VA, TA, VB, TB),
    type(V, TV),
    listof(TV, TE).

pair(A, B, C)?
