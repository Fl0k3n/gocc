%start S
%%

S
    : X X
    ;

X
    : 'a' X
    | 'b'
    ;
