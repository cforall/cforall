/* adasd1 */   

%token<tokenp>	A 1 , B 2 C 3			// %%
%type<tokenp>	A , B C				// %%
%type	A , B C				// %%

/* adsad2 */
%locations
%define parse.error verbose
%%

rules2	: pop xxx %prec '.' yyy push %prec '.'	/* XX */
		{ $$ = build_nt (CALL_EXPR, $1, $2, NULL_TREE); }
		{ $$ = build_nt (CALL_EXPR, $1, $2, NULL_TREE); }
	| xxx yyy ';'
	;

rules1	:
	/* empty */
	{}
	| xxx
	  /* fred */ yyy
	| xxx
		{}
	  yyy ';'
	|
	;

mark		: MARK
		| error					/* missing %% */
			{
			    cerr << "no input grammar, missing %% mark" << endl;
			    exit( -1 );
			}
		;

x	: xxx yyy

y	:
	;

w	:
	  xxx
	|
	  yyy
	;
%%
{
    {
    }
}
