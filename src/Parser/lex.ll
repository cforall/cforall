/*
 * Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
 *
 * The contents of this file are covered under the licence agreement in the
 * file "LICENCE" distributed with Cforall.
 *
 * lex.ll --
 *
 * Author           : Peter A. Buhr
 * Created On       : Sat Sep 22 08:58:10 2001
 * Last Modified By : Peter A. Buhr
 * Last Modified On : Wed Sep 11 17:16:23 2024
 * Update Count     : 791
 */

%option yylineno
%option noyywrap
%option nounput

%{
// The lexer assumes the program has been preprocessed by cpp. Hence, all user level preprocessor directive have been
// performed and removed from the source. The only exceptions are preprocessor directives passed to the compiler (e.g.,
// line-number directives) and C/C++ style comments, which are ignored.

// *************************** Includes and Defines ****************************

#ifdef __clang__
#pragma GCC diagnostic ignored "-Wnull-conversion"
#endif

// trigger before each matching rule's action
#define YY_USER_ACTION \
	yylloc.first_line = yylineno; \
	yylloc.first_column = column; \
	column += yyleng; \
	yylloc.last_column = column; \
	yylloc.last_line = yylineno; \
	yylloc.filename = yyfilename ? yyfilename : "";
unsigned int column = 0;								// position of the end of the last token parsed

#include <string>
#include <cstdio>										// FILENAME_MAX
using namespace std;

#include "config.h"										// configure info
#include "DeclarationNode.hpp"                          // for DeclarationNode
#include "ExpressionNode.hpp"                           // for LabelNode
#include "InitializerNode.hpp"                          // for InitializerNode
#include "ParseNode.hpp"
#include "ParserTypes.hpp"                              // for Token
#include "StatementNode.hpp"                            // for CondCtl, ForCtrl
#include "TypedefTable.hpp"
// This (generated) header must come late as it is missing includes.
#include "parser.hh"                                    // generated info

string * build_postfix_name( string * name );

char *yyfilename;
string *strtext;										// accumulate parts of character and string constant value

#define RETURN_LOCN(x)		yylval.tok.loc.file = yyfilename; yylval.tok.loc.line = yylineno; return( x )
#define RETURN_VAL(x)		yylval.tok.str = new string( yytext ); RETURN_LOCN( x )
#define RETURN_CHAR(x)		yylval.tok.str = nullptr; RETURN_LOCN( x )
#define RETURN_STR(x)		yylval.tok.str = strtext; RETURN_LOCN( x )

#define WHITE_RETURN(x)		// do nothing
#define NEWLINE_RETURN()	column = 0; WHITE_RETURN( '\n' )
#define ASCIIOP_RETURN()	RETURN_CHAR( (int)yytext[0] ) // single character operator
#define NAMEDOP_RETURN(x)	RETURN_CHAR( x )			// multichar operator, with a name
#define NUMERIC_RETURN(x)	rm_underscore(); RETURN_VAL( x ) // numeric constant
#define KEYWORD_RETURN(x)	RETURN_CHAR( x )			// keyword
#define QKEYWORD_RETURN(x)	RETURN_VAL(x);				// quasi-keyword
#define IDENTIFIER_RETURN()	RETURN_VAL( typedefTable.isKind( yytext ) )

#ifdef HAVE_KEYWORDS_FLOATXX							// GCC >= 7 => keyword, otherwise typedef
#define FLOATXX(v) KEYWORD_RETURN(v);
#else
#define FLOATXX(v) IDENTIFIER_RETURN();
#endif // HAVE_KEYWORDS_FLOATXX

void rm_underscore() {
	// SKULLDUGGERY: remove underscores (ok to shorten?)
	yyleng = 0;
	for ( int i = 0; yytext[i] != '\0'; i += 1 ) {		// copying non-underscore characters to front of string
		if ( yytext[i] != '_' ) {
			yytext[yyleng] = yytext[i];
			yyleng += 1;
		} // if
	} // for
	yytext[yyleng] = '\0';
} // rm_underscore

// Stop warning due to incorrectly generated flex code.
#pragma GCC diagnostic ignored "-Wsign-compare"
%}

binary [0-1]
octal [0-7]
nonzero [1-9]
decimal [0-9]
hex [0-9a-fA-F]
universal_char "\\"((u"_"?{hex_quad})|(U"_"?{hex_quad}{2}))

				// identifier, GCC: $ in identifier
identifier ([a-zA-Z_$]|{universal_char})([0-9a-zA-Z_$]|{universal_char})*

				// numeric constants, CFA: '_' in constant
hex_quad {hex}("_"?{hex}){3}
size_opt (8|16|32|64|128)?
				// CFA: explicit l8/l16/l32/l64/l128, char 'hh', short 'h', int 'n'
length ("ll"|"LL"|[lL]{size_opt})|("hh"|"HH"|[hHnN])
				// CFA: size_t 'z', pointer 'p', which define a sign and length
integer_suffix_opt ("_"?(([uU]({length}?[iI]?)|([iI]{length}))|([iI]({length}?[uU]?)|([uU]{length}))|({length}([iI]?[uU]?)|([uU][iI]))|[zZ]|[pP]))?

octal_digits ({octal})|({octal}({octal}|"_")*{octal})
octal_prefix "0""_"?
octal_constant (("0")|({octal_prefix}{octal_digits})){integer_suffix_opt}

nonzero_digits ({nonzero})|({nonzero}({decimal}|"_")*{decimal})
decimal_constant {nonzero_digits}{integer_suffix_opt}

binary_digits ({binary})|({binary}({binary}|"_")*{binary})
binary_prefix "0"[bB]"_"?
binary_constant {binary_prefix}{binary_digits}{integer_suffix_opt}

hex_digits ({hex})|({hex}({hex}|"_")*{hex})
hex_prefix "0"[xX]"_"?
hex_constant {hex_prefix}{hex_digits}{integer_suffix_opt}

				// GCC: floating D (double), imaginary iI, and decimal floating DF, DD, DL
exponent "_"?[eE]"_"?[+-]?{decimal_digits}
floating_size 16|32|32x|64|64x|80|128|128x
floating_length ([fFdDlLwWqQ]|[fF]{floating_size})
floating_suffix ({floating_length}?[iI]?)|([iI]{floating_length})
decimal_floating_suffix [dD][fFdDlL]
floating_suffix_opt ("_"?({floating_suffix}|{decimal_floating_suffix}))?
decimal_digits ({decimal})|({decimal}({decimal}|"_")*{decimal})
floating_decimal {decimal_digits}"."{exponent}?{floating_suffix_opt}
floating_fraction "."{decimal_digits}{exponent}?{floating_suffix_opt}
floating_constant ({decimal_digits}{exponent}{floating_suffix_opt})|({decimal_digits}{floating_fraction})

binary_exponent "_"?[pP]"_"?[+-]?{decimal_digits}
hex_floating_suffix_opt ("_"?({floating_suffix}))?
hex_floating_fraction ({hex_digits}?"."{hex_digits})|({hex_digits}".")
hex_floating_constant {hex_prefix}(({hex_floating_fraction}{binary_exponent})|({hex_digits}{binary_exponent})){hex_floating_suffix_opt}

				// character escape sequence, GCC: \e => esc character
simple_escape "\\"[abefnrtv'"?\\]
				// ' stop editor highlighting
octal_escape "\\"{octal}("_"?{octal}){0,2}
hex_escape "\\""x""_"?{hex_digits}
escape_seq {simple_escape}|{octal_escape}|{hex_escape}|{universal_char}
cwide_prefix "L"|"U"|"u"
swide_prefix {cwide_prefix}|"u8"

				// display/white-space characters
h_tab [\011]
form_feed [\014]
v_tab [\013]
c_return [\015]
h_white [ ]|{h_tab}

				// overloadable operators
op_unary_only "~"|"!"
op_unary_binary "+"|"-"|"*"
op_unary_pre_post "++"|"--"
op_unary {op_unary_only}|{op_unary_binary}|{op_unary_pre_post}

op_binary_only "/"|"%"|"\\"|"^"|"&"|"|"|"<"|">"|"="|"=="|"!="|"<<"|">>"|"<="|">="|"+="|"-="|"*="|"/="|"%="|"\\="|"&="|"|="|"^="|"<<="|">>="
op_binary_over {op_unary_binary}|{op_binary_only}
				// op_binary_not_over "?"|"->"|"."|"&&"|"||"|"@="
				// operator {op_unary_pre_post}|{op_binary_over}|{op_binary_not_over}

%x COMMENT
%x BKQUOTE
%x QUOTE
%x STRING

%%
				/* line directives */
^{h_white}*"#"{h_white}*[0-9]+{h_white}*["][^"\n]+["].*"\n" {
	/* " stop editor highlighting */
	static char filename[FILENAME_MAX];					// temporarily store current source-file name
	char *end_num;
	char *begin_string, *end_string;
	long lineno, length;
	lineno = strtol( yytext + 1, &end_num, 0 );
	begin_string = strchr( end_num, '"' );
	if ( begin_string ) {								// file name ?
		end_string = strchr( begin_string + 1, '"' );	// look for ending delimiter
		assert( end_string );							// closing quote ?
		length = end_string - begin_string - 1;			// file-name length without quotes or sentinel
		assert( length < FILENAME_MAX );				// room for sentinel ?
		memcpy( &filename, begin_string + 1, length );	// copy file name from yytext
		filename[ length ] = '\0';						// terminate string with sentinel
		//cout << "file " << filename << " line " << lineno << endl;
		yylineno = lineno;
		yyfilename = filename;
	} // if
}

				/* preprocessor-style directives */
^{h_white}*"#"[^\n]*"\n" { RETURN_VAL( DIRECTIVE ); }

				/* ignore C style comments (ALSO HANDLED BY CPP) */
"/*"			{ BEGIN COMMENT; }
<COMMENT>.|\n	;
<COMMENT>"*/"	{ BEGIN 0; }

				/* ignore C++ style comments (ALSO HANDLED BY CPP) */
"//"[^\n]*"\n"	;

				/* ignore whitespace */
{h_white}+		{ WHITE_RETURN(' '); }
({v_tab}|{c_return}|{form_feed})+ { WHITE_RETURN(' '); }
({h_white}|{v_tab}|{c_return}|{form_feed})*"\n" { NEWLINE_RETURN(); }

				/* keywords */
alignas			{ KEYWORD_RETURN(ALIGNAS); }			// CFA
_Alignas		{ KEYWORD_RETURN(ALIGNAS); }			// C11
alignof			{ KEYWORD_RETURN(ALIGNOF); }			// CFA
_Alignof		{ KEYWORD_RETURN(ALIGNOF); }			// C11
__alignof		{ KEYWORD_RETURN(ALIGNOF); }			// GCC
__alignof__		{ KEYWORD_RETURN(ALIGNOF); }			// GCC
and				{ QKEYWORD_RETURN(WAND); }				// CFA
asm				{ KEYWORD_RETURN(ASM); }
__asm			{ KEYWORD_RETURN(ASM); }				// GCC
__asm__			{ KEYWORD_RETURN(ASM); }				// GCC
_Atomic			{ KEYWORD_RETURN(ATOMIC); }				// C11
__attribute		{ KEYWORD_RETURN(ATTRIBUTE); }			// GCC
__attribute__	{ KEYWORD_RETURN(ATTRIBUTE); }			// GCC
auto			{ KEYWORD_RETURN(AUTO); }
__auto_type		{ KEYWORD_RETURN(AUTO_TYPE); }
basetypeof		{ KEYWORD_RETURN(BASETYPEOF); }			// CFA
_Bool			{ KEYWORD_RETURN(BOOL); }				// C99
__SVBool_t		{ KEYWORD_RETURN(SVBOOL); }				// gcc (ARM)
break			{ KEYWORD_RETURN(BREAK); }
case			{ KEYWORD_RETURN(CASE); }
catch			{ QKEYWORD_RETURN(CATCH); }				// CFA
catchResume		{ QKEYWORD_RETURN(CATCHRESUME); }		// CFA
char			{ KEYWORD_RETURN(CHAR); }
choose			{ KEYWORD_RETURN(CHOOSE); }				// CFA
coerce			{ KEYWORD_RETURN(COERCE); }				// CFA
corun			{ KEYWORD_RETURN(CORUN); }				// CFA
cofor			{ KEYWORD_RETURN(COFOR); }				// CFA
_Complex		{ KEYWORD_RETURN(COMPLEX); }			// C99
__complex		{ KEYWORD_RETURN(COMPLEX); }			// GCC
__complex__		{ KEYWORD_RETURN(COMPLEX); }			// GCC
const			{ KEYWORD_RETURN(CONST); }
__const			{ KEYWORD_RETURN(CONST); }				// GCC
__const__		{ KEYWORD_RETURN(CONST); }				// GCC
continue		{ KEYWORD_RETURN(CONTINUE); }
coroutine		{ KEYWORD_RETURN(COROUTINE); }			// CFA
_Decimal32		{ KEYWORD_RETURN(DECIMAL32); }			// GCC
_Decimal64		{ KEYWORD_RETURN(DECIMAL64); }			// GCC
_Decimal128		{ KEYWORD_RETURN(DECIMAL128); }			// GCC
default			{ KEYWORD_RETURN(DEFAULT); }
disable			{ KEYWORD_RETURN(DISABLE); }			// CFA
do				{ KEYWORD_RETURN(DO); }
double			{ KEYWORD_RETURN(DOUBLE); }
dtype			{ KEYWORD_RETURN(DTYPE); }				// CFA
else			{ KEYWORD_RETURN(ELSE); }
enable			{ KEYWORD_RETURN(ENABLE); }				// CFA
enum			{ KEYWORD_RETURN(ENUM); }
exception		{ KEYWORD_RETURN(EXCEPTION); }			// CFA
__extension__	{ KEYWORD_RETURN(EXTENSION); }			// GCC
extern			{ KEYWORD_RETURN(EXTERN); }
fallthrough		{ KEYWORD_RETURN(FALLTHROUGH); }		// CFA
fallthru		{ KEYWORD_RETURN(FALLTHRU); }			// CFA
finally			{ QKEYWORD_RETURN(FINALLY); }			// CFA
fixup			{ QKEYWORD_RETURN(FIXUP); }				// CFA
float			{ KEYWORD_RETURN(FLOAT); }
__float80		{ KEYWORD_RETURN(FLOAT80); }			// GCC
float80			{ KEYWORD_RETURN(FLOAT80); }			// GCC
__float128		{ KEYWORD_RETURN(uuFLOAT128); }			// GCC
float128		{ KEYWORD_RETURN(uuFLOAT128); }			// GCC
_Float16		{ FLOATXX(FLOAT16); }					// GCC
_Float32		{ FLOATXX(FLOAT32); }					// GCC
_Float32x		{ FLOATXX(FLOAT32X); }					// GCC
_Float64		{ FLOATXX(FLOAT64); }					// GCC
_Float64x		{ FLOATXX(FLOAT64X); }					// GCC
_Float128		{ FLOATXX(FLOAT128); }					// GCC
_Float128x		{ FLOATXX(FLOAT128X); }					// GCC
__Float32x4_t	{ FLOATXX(FLOAT32X4); }					// GCC (ARM)
__Float64x2_t	{ FLOATXX(FLOAT64X2); }					// GCC (ARM)
__SVFloat32_t	{ FLOATXX(SVFLOAT32); }					// GCC (ARM)
__SVFloat64_t	{ FLOATXX(SVFLOAT64); }					// GCC (ARM)
for				{ KEYWORD_RETURN(FOR); }
forall			{ KEYWORD_RETURN(FORALL); }				// CFA
fortran			{ KEYWORD_RETURN(FORTRAN); }
ftype			{ KEYWORD_RETURN(FTYPE); }				// CFA
generator		{ KEYWORD_RETURN(GENERATOR); }			// CFA
_Generic		{ KEYWORD_RETURN(GENERIC); }			// C11
goto			{ KEYWORD_RETURN(GOTO); }
if				{ KEYWORD_RETURN(IF); }
_Imaginary		{ KEYWORD_RETURN(IMAGINARY); }			// C99
__imag			{ KEYWORD_RETURN(IMAGINARY); }			// GCC
__imag__		{ KEYWORD_RETURN(IMAGINARY); }			// GCC
inline			{ KEYWORD_RETURN(INLINE); }				// C99
__inline		{ KEYWORD_RETURN(INLINE); }				// GCC
__inline__		{ KEYWORD_RETURN(INLINE); }				// GCC
int				{ KEYWORD_RETURN(INT); }
int128			{ KEYWORD_RETURN(INT128); }				// CFA
__int128		{ KEYWORD_RETURN(INT128); }				// GCC
__int128_t		{ KEYWORD_RETURN(INT128); }				// GCC
__label__		{ KEYWORD_RETURN(LABEL); }				// GCC
long			{ KEYWORD_RETURN(LONG); }
monitor			{ KEYWORD_RETURN(MONITOR); }			// CFA
mutex			{ KEYWORD_RETURN(MUTEX); }				// CFA
_Noreturn		{ KEYWORD_RETURN(NORETURN); }			// C11
__builtin_offsetof { KEYWORD_RETURN(OFFSETOF); }		// GCC
one_t			{ NUMERIC_RETURN(ONE_T); }				// CFA
or				{ QKEYWORD_RETURN(WOR); }				// CFA
otype			{ KEYWORD_RETURN(OTYPE); }				// CFA
recover			{ QKEYWORD_RETURN(RECOVER); }			// CFA
register		{ KEYWORD_RETURN(REGISTER); }
report			{ KEYWORD_RETURN(THROWRESUME); }		// CFA
restrict		{ KEYWORD_RETURN(RESTRICT); }			// C99
__restrict		{ KEYWORD_RETURN(RESTRICT); }			// GCC
__restrict__	{ KEYWORD_RETURN(RESTRICT); }			// GCC
return			{ KEYWORD_RETURN(RETURN); }
 /* resume			{ KEYWORD_RETURN(RESUME); }				// CFA */
short			{ KEYWORD_RETURN(SHORT); }
signed			{ KEYWORD_RETURN(SIGNED); }
__signed		{ KEYWORD_RETURN(SIGNED); }				// GCC
__signed__		{ KEYWORD_RETURN(SIGNED); }				// GCC
sizeof			{ KEYWORD_RETURN(SIZEOF); }
countof			{ KEYWORD_RETURN(COUNTOF); }			// GCC
static			{ KEYWORD_RETURN(STATIC); }
_Static_assert	{ KEYWORD_RETURN(STATICASSERT); }		// C11
static_assert	{ KEYWORD_RETURN(STATICASSERT); }		// C23
struct			{ KEYWORD_RETURN(STRUCT); }
suspend			{ KEYWORD_RETURN(SUSPEND); }			// CFA
switch			{ KEYWORD_RETURN(SWITCH); }
thread			{ KEYWORD_RETURN(THREAD); }				// C11
__thread		{ KEYWORD_RETURN(THREADLOCALGCC); }		// GCC
_Thread_local	{ KEYWORD_RETURN(THREADLOCALC11); }		// C11
thread_local	{ KEYWORD_RETURN(THREADLOCALC11); }		// C23
throw			{ KEYWORD_RETURN(THROW); }				// CFA
throwResume		{ KEYWORD_RETURN(THROWRESUME); }		// CFA
timeout			{ QKEYWORD_RETURN(TIMEOUT); }			// CFA
trait			{ KEYWORD_RETURN(TRAIT); }				// CFA
try				{ KEYWORD_RETURN(TRY); }				// CFA
ttype			{ KEYWORD_RETURN(TTYPE); }				// CFA
typedef			{ KEYWORD_RETURN(TYPEDEF); }
typeof			{ KEYWORD_RETURN(TYPEOF); }				// GCC
__typeof		{ KEYWORD_RETURN(TYPEOF); }				// GCC
__typeof__		{ KEYWORD_RETURN(TYPEOF); }				// GCC
typeid			{ KEYWORD_RETURN(TYPEID); }				// GCC
union			{ KEYWORD_RETURN(UNION); }
__uint128_t		{ KEYWORD_RETURN(UINT128); }			// GCC
unsigned		{ KEYWORD_RETURN(UNSIGNED); }
__builtin_va_arg { KEYWORD_RETURN(VA_ARG); }			// GCC
__builtin_va_list { KEYWORD_RETURN(VA_LIST); }			// GCC
virtual			{ KEYWORD_RETURN(VIRTUAL); }			// CFA
void			{ KEYWORD_RETURN(VOID); }
volatile		{ KEYWORD_RETURN(VOLATILE); }
__volatile		{ KEYWORD_RETURN(VOLATILE); }			// GCC
__volatile__	{ KEYWORD_RETURN(VOLATILE); }			// GCC
vtable			{ KEYWORD_RETURN(VTABLE); }				// CFA
waitfor			{ KEYWORD_RETURN(WAITFOR); }			// CFA
waituntil		{ KEYWORD_RETURN(WAITUNTIL); }			// CFA
when			{ KEYWORD_RETURN(WHEN); }				// CFA
while			{ KEYWORD_RETURN(WHILE); }
with			{ KEYWORD_RETURN(WITH); }				// CFA
zero_t			{ NUMERIC_RETURN(ZERO_T); }				// CFA

				/* identifier */
{identifier}	{ IDENTIFIER_RETURN(); }
"``"{identifier} {										// CFA
	yytext[yyleng] = '\0'; yytext += 2;					// SKULLDUGGERY: remove backquotes (ok to shorten?)
	IDENTIFIER_RETURN();
}

				/* numeric constants */
{binary_constant} { NUMERIC_RETURN(INTEGERconstant); }
{octal_constant} { NUMERIC_RETURN(INTEGERconstant); }
{decimal_constant} { NUMERIC_RETURN(INTEGERconstant); }
{hex_constant}	{ NUMERIC_RETURN(INTEGERconstant); }
{floating_decimal}	{ NUMERIC_RETURN(FLOATING_DECIMALconstant); } // must appear before floating_constant
{floating_fraction}	{ NUMERIC_RETURN(FLOATING_FRACTIONconstant); } // must appear before floating_constant
{floating_constant}	{ NUMERIC_RETURN(FLOATINGconstant); }
{hex_floating_constant}	{ NUMERIC_RETURN(FLOATINGconstant); }

				/* character constant, allows empty value */
({cwide_prefix}[_]?)?['] { BEGIN QUOTE; rm_underscore(); strtext = new string( yytext, yyleng ); }
<QUOTE>[^'\\\n]* { strtext->append( yytext, yyleng ); }
<QUOTE>['\n]	{ BEGIN 0; strtext->append( yytext, yyleng ); RETURN_STR(CHARACTERconstant); }
				/* ' stop editor highlighting */

				/* string constant */
({swide_prefix}[_]?)?["] { BEGIN STRING; rm_underscore(); strtext = new string( yytext, yyleng ); }
<STRING>[^"\\\n]* { strtext->append( yytext, yyleng ); }
<STRING>["\n]	{ BEGIN 0; strtext->append( yytext, yyleng ); RETURN_STR(STRINGliteral); }
				/* " stop editor highlighting */

				/* common character/string constant */
<QUOTE,STRING>{escape_seq} { rm_underscore(); strtext->append( yytext, yyleng ); }
<QUOTE,STRING>"\\"{h_white}*"\n" {}						// continuation (ALSO HANDLED BY CPP)
<QUOTE,STRING>"\\" { strtext->append( yytext, yyleng ); } // unknown escape character

				/* punctuation */
"@"				{ ASCIIOP_RETURN(); }
"`"				{ ASCIIOP_RETURN(); }
"["				{ ASCIIOP_RETURN(); }
"]"				{ ASCIIOP_RETURN(); }
"("				{ ASCIIOP_RETURN(); }
")"				{ ASCIIOP_RETURN(); }
"{"				{ ASCIIOP_RETURN(); }
"}"				{ ASCIIOP_RETURN(); }
","				{ ASCIIOP_RETURN(); }					// also operator
":"				{ ASCIIOP_RETURN(); }
";"				{ ASCIIOP_RETURN(); }
"."				{ ASCIIOP_RETURN(); }					// also operator
"@@"			{ NAMEDOP_RETURN(ATTR); }				// CFA, attribute shorthand
"..."			{ NAMEDOP_RETURN(ELLIPSIS); }

				/* alternative C99 brackets, "<:" & "<:<:" handled by preprocessor */
"<:"			{ RETURN_VAL('['); }
":>"			{ RETURN_VAL(']'); }
"<%"			{ RETURN_VAL('{'); }
"%>"			{ RETURN_VAL('}'); }

				/* operators */
"!"				{ ASCIIOP_RETURN(); }
"+"				{ ASCIIOP_RETURN(); }
"-"				{ ASCIIOP_RETURN(); }
"*"				{ ASCIIOP_RETURN(); }
"\\"			{ ASCIIOP_RETURN(); }					// CFA, exponentiation
"/"				{ ASCIIOP_RETURN(); }
"%"				{ ASCIIOP_RETURN(); }
"^"				{ ASCIIOP_RETURN(); }
"~"				{ ASCIIOP_RETURN(); }
"&"				{ ASCIIOP_RETURN(); }
"|"				{ ASCIIOP_RETURN(); }
"<"				{ ASCIIOP_RETURN(); }
">"				{ ASCIIOP_RETURN(); }
"="				{ ASCIIOP_RETURN(); }
"?"				{ ASCIIOP_RETURN(); }

"++"			{ NAMEDOP_RETURN(ICR); }
"--"			{ NAMEDOP_RETURN(DECR); }
"=="			{ NAMEDOP_RETURN(EQ); }
"!="			{ NAMEDOP_RETURN(NE); }
"<<"			{ NAMEDOP_RETURN(LS); }
">>"			{ NAMEDOP_RETURN(RS); }
"<="			{ NAMEDOP_RETURN(LE); }
">="			{ NAMEDOP_RETURN(GE); }
"&&"			{ NAMEDOP_RETURN(ANDAND); }
"||"			{ NAMEDOP_RETURN(OROR); }
"->"			{ NAMEDOP_RETURN(ARROW); }
"+="			{ NAMEDOP_RETURN(PLUSassign); }
"-="			{ NAMEDOP_RETURN(MINUSassign); }
"\\="			{ NAMEDOP_RETURN(EXPassign); }			// CFA, exponentiation
"*="			{ NAMEDOP_RETURN(MULTassign); }
"/="			{ NAMEDOP_RETURN(DIVassign); }
"%="			{ NAMEDOP_RETURN(MODassign); }
"&="			{ NAMEDOP_RETURN(ANDassign); }
"|="			{ NAMEDOP_RETURN(ORassign); }
"^="			{ NAMEDOP_RETURN(ERassign); }
"<<="			{ NAMEDOP_RETURN(LSassign); }
">>="			{ NAMEDOP_RETURN(RSassign); }

"@="			{ NAMEDOP_RETURN(ATassign); }			// CFA
"+~"			{ NAMEDOP_RETURN(ErangeUp); }			// CFA
"~="			{ NAMEDOP_RETURN(ErangeUpEq); }			// CFA
"+~="			{ NAMEDOP_RETURN(ErangeUpEq); }			// CFA
"-~"			{ NAMEDOP_RETURN(ErangeDown); }			// CFA
"-~="			{ NAMEDOP_RETURN(ErangeDownEq); }		// CFA

				/* CFA, operator identifier */
{op_unary}"?"	{ IDENTIFIER_RETURN(); }				// unary
"?"({op_unary_pre_post}|"()"|"[?]"|"{}") { IDENTIFIER_RETURN(); }
"^?{}"			{ IDENTIFIER_RETURN(); }
"?`"{identifier} {										// postfix operator
	yylval.tok.str = new string( &yytext[2] );			// remove ?`
	yylval.tok.str = build_postfix_name( yylval.tok.str ); // add prefix
	RETURN_LOCN( typedefTable.isKind( *yylval.tok.str ) );
}
"?"{op_binary_over}"?"	{ IDENTIFIER_RETURN(); }		// binary
	/*
	  This rule handles ambiguous cases with operator identifiers, e.g., "int *?*?()", where the string "*?*?"  can be
	  lexed as "*?"/"*?" or "*"/"?*?". Since it is common practise to put a unary operator juxtaposed to an identifier,
	  e.g., "*i", users will be annoyed if they cannot do this with respect to operator identifiers. Therefore, there is
	  a lexical look-ahead for the second case, with backtracking to return the leading unary operator and then
	  reparsing the trailing operator identifier.  Otherwise a space is needed between the unary operator and operator
	  identifier to disambiguate this common case.

	  A similar issue occurs with the dereference, *?(...), and routine-call, ?()(...) identifiers.  The ambiguity
	  occurs when the deference operator has no parameters, *?() and *?()(...), requiring arbitrary whitespace
	  look-ahead for the routine-call parameter-list to disambiguate.  However, the dereference operator must have a
	  parameter/argument to dereference *?(...).  Hence, always interpreting the string *?() as * ?() does not preclude
	  any meaningful program.

	  The remaining cases are with the increment/decrement operators and conditional expression:

	  i++? ...(...);
	  i?++ ...(...);

	  requiring arbitrary whitespace look-ahead for the operator parameter-list, even though that interpretation is an
      incorrect expression (juxtaposed identifiers).  Therefore, it is necessary to disambiguate these cases with a
      space:

	  i++ ? i : 0;
	  i? ++i : 0;
	*/
{op_unary}"?"({op_unary_pre_post}|"()"|"[?]"|{op_binary_over}"?") {
	// 1 or 2 character unary operator ?
	int i = yytext[1] == '?' ? 1 : 2;
	yyless( i );		// put back characters up to first '?'
	if ( i > 1 ) {
		NAMEDOP_RETURN( yytext[0] == '+' ? ICR : DECR );
	} else {
		ASCIIOP_RETURN();
	} // if
}

				/* unknown character */
.				{ yyerror( "unknown character" ); }

%%

// ----end of lexer----

void yyerror( const char * errmsg ) {
	SemanticErrorThrow = true;
	cerr << (yyfilename ? yyfilename : "*unknown file*") << ':' << yylineno << ':' << column - yyleng + 1
		 << ": " << ErrorHelpers::error_str() << errmsg << " before token \"" << (yytext[0] == '\0' ? "EOF" : yytext) << '"' << endl;
}

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
