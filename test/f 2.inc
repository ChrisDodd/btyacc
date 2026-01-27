expr: expr op1 expr %prec LO
    | expr op2 expr %prec HI
    | unary expr %prec UNARY
    ;

