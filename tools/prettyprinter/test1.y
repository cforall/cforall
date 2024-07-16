%token A

%start rules
%%
rules	: /* empty */		/* no rules */
	| lhs rhs		/* start new rule */
	| lhs rhs ';'		/* start new rule */
	|
	;

lhs	: A | ':' ;

rhs	: /* empty */		/* empty rule */
        | rhs more ;

more	: lhs			/* start next rule */
	| ';' lhs		/* start next rule */
	| foo			/* component of current rule */
	| '|'			/* empty rule */
	;

foo	: A | 'l' | 'X'

bar:
