/*
 * [test] btyacc
 * [test] cc -c -Wall -Werror
 *
 * testing trailing spaces/tabs on %include lines
 */
%include f1.inc          

%%

%include        f 2.inc   

op1 : '+'
    | '-'
    ;

op2 : '*'
    | '/'
    | '%'
    ;

unary : '+' | '-' | '*' | '&' ;

