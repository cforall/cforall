//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// parser.yy --
//
// Author           : Peter A. Buhr
// Created On       : Sat Sep  1 20:22:55 2001
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 11:06:13 2022
// Update Count     : 5167
//

// This grammar is based on the ANSI99/11 C grammar, specifically parts of EXPRESSION and STATEMENTS, and on the C
// grammar by James A. Roskind, specifically parts of DECLARATIONS and EXTERNAL DEFINITIONS.  While parts have been
// copied, important changes have been made in all sections; these changes are sufficient to constitute a new grammar.
// In particular, this grammar attempts to be more syntactically precise, i.e., it parses less incorrect language syntax
// that must be subsequently rejected by semantic checks.  Nevertheless, there are still several semantic checks
// required and many are noted in the grammar. Finally, the grammar is extended with GCC and CFA language extensions.

// Acknowledgments to Richard Bilson, Glen Ditchfield, and Rodolfo Gabriel Esteves who all helped when I got stuck with
// the grammar.

// The root language for this grammar is ANSI99/11 C. All of ANSI99/11 is parsed, except for:
//
//   designation with '=' (use ':' instead)
//
// This incompatibility is discussed in detail before the "designation" grammar rule.  Most of the syntactic extensions
// from ANSI90 to ANSI11 C are marked with the comment "C99/C11".

// This grammar also has two levels of extensions. The first extensions cover most of the GCC C extensions. All of the
// syntactic extensions for GCC C are marked with the comment "GCC". The second extensions are for Cforall (CFA), which
// fixes several of C's outstanding problems and extends C with many modern language concepts. All of the syntactic
// extensions for CFA C are marked with the comment "CFA".

%{
#define YYDEBUG_LEXER_TEXT( yylval )					// lexer loads this up each time
#define YYDEBUG 1										// get the pretty debugging code to compile
#define YYERROR_VERBOSE									// more information in syntax errors

#undef __GNUC_MINOR__

#include <cstdio>
#include <stack>
using namespace std;

#include "SynTree/Declaration.h"
#include "ParseNode.h"
#include "TypedefTable.h"
#include "TypeData.h"
#include "SynTree/LinkageSpec.h"
#include "Common/SemanticError.h"						// error_str
#include "Common/utility.h"								// for maybeMoveBuild, maybeBuild, CodeLo...

extern DeclarationNode * parseTree;
extern LinkageSpec::Spec linkage;
extern TypedefTable typedefTable;

stack<LinkageSpec::Spec> linkageStack;

bool appendStr( string & to, string & from ) {
	// 1. Multiple strings are concatenated into a single string but not combined internally. The reason is that
	//    "\x12" "3" is treated as 2 characters versus 1 because "escape sequences are converted into single members of
	//    the execution character set just prior to adjacent string literal concatenation" (C11, Section 6.4.5-8). It is
	//    easier to let the C compiler handle this case.
	//
	// 2. String encodings are transformed into canonical form (one encoding at start) so the encoding can be found
	//    without searching the string, e.g.: "abc" L"def" L"ghi" => L"abc" "def" "ghi". Multiple encodings must match,
	//    e.g., u"a" U"b" L"c" is disallowed.

	if ( from[0] != '"' ) {								// encoding ?
		if ( to[0] != '"' ) {							// encoding ?
			if ( to[0] != from[0] || to[1] != from[1] ) { // different encodings ?
				yyerror( "non-matching string encodings for string-literal concatenation" );
				return false;							// parse error, must call YYERROR in action
			} else if ( from[1] == '8' ) {
				from.erase( 0, 1 );						// remove 2nd encoding
			} // if
		} else {
			if ( from[1] == '8' ) {						// move encoding to start
				to = "u8" + to;
				from.erase( 0, 1 );						// remove 2nd encoding
			} else {
				to = from[0] + to;
			} // if
		} // if
		from.erase( 0, 1 );								// remove 2nd encoding
	} // if
	to += " " + from;									// concatenated into single string
	return true;
} // appendStr

DeclarationNode * distAttr( DeclarationNode * specifier, DeclarationNode * declList ) {
	// distribute declaration_specifier across all declared variables, e.g., static, const, __attribute__.
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( specifier );
	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		// SKULLDUGGERY: Distributions are parsed inside out, so qualifiers are added to declarations inside out. Since
		// addQualifiers appends to the back of the list, the forall clauses are in the wrong order (right to left). To
		// get the qualifiers in the correct order and still use addQualifiers (otherwise, 90% of addQualifiers has to
		// be copied to add to front), the appropriate forall pointers are interchanged before calling addQualifiers.
		DeclarationNode * clone = qualifiers->clone();
		if ( qualifiers->type ) {						// forall clause ? (handles SC)
			if ( iter->type->kind == TypeData::Aggregate ) { // struct/union ?
				swap( clone->type->forall, iter->type->aggregate.params );
				iter->addQualifiers( clone );
			} else if ( iter->type->kind == TypeData::AggregateInst && iter->type->aggInst.aggregate->aggregate.body ) { // struct/union ?
				// Create temporary node to hold aggregate, call addQualifiers as above, then put nodes back together.
				DeclarationNode newnode;
				swap( newnode.type, iter->type->aggInst.aggregate );
				swap( clone->type->forall, newnode.type->aggregate.params );
				newnode.addQualifiers( clone );
				swap( newnode.type, iter->type->aggInst.aggregate );
			} else if ( iter->type->kind == TypeData::Function ) { // routines ?
				swap( clone->type->forall, iter->type->forall );
				iter->addQualifiers( clone );
			} // if
		} else {										// just SC qualifiers
			iter->addQualifiers( clone );
		} // if
	} // for
	delete qualifiers;
} // distQual

// There is an ambiguity for inline generic-routine return-types and generic routines.
//   forall( otype T ) struct S { int i; } bar( T ) {}
// Does the forall bind to the struct or the routine, and how would it be possible to explicitly specify the binding.
//   forall( otype T ) struct S { int T; } forall( otype W ) bar( W ) {}
// Currently, the forall is associated with the routine, and the generic type has to be separately defined:
//   forall( otype T ) struct S { int T; };
//   forall( otype W ) bar( W ) {}

void rebindForall( DeclarationNode * declSpec, DeclarationNode * funcDecl ) {
	if ( declSpec->type->kind == TypeData::Aggregate ) { // ignore aggregate definition
		funcDecl->type->forall = declSpec->type->aggregate.params; // move forall from aggregate to function type
		declSpec->type->aggregate.params = nullptr;
	} // if
} // rebindForall

string * build_postfix_name( string * name ) {
	*name = string("__postfix_func_") + *name;
	return name;
} // build_postfix_name

DeclarationNode * fieldDecl( DeclarationNode * typeSpec, DeclarationNode * fieldList ) {
	if ( ! fieldList ) {								// field declarator ?
		if ( ! ( typeSpec->type && (typeSpec->type->kind == TypeData::Aggregate || typeSpec->type->kind == TypeData::Enum) ) ) {
			stringstream ss;
			typeSpec->type->print( ss );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		fieldList = DeclarationNode::newName( nullptr );
	} // if
	return distAttr( typeSpec, fieldList );				// mark all fields in list
} // fieldDecl

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild<Expression>(type), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
//	type = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__for_control_index_constraints__" ) ) ), type ) );
	return new ForCtrl(
		distAttr( DeclarationNode::newTypeof( type, true ), DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) ) ),
		// NULL comp/inc => leave blank
		comp ? new ExpressionNode( build_binary_val( compop, new ExpressionNode( build_varref( new string( *index ) ) ), comp ) ) : 0,
		inc ? new ExpressionNode( build_binary_val( compop == OperKinds::LThan || compop == OperKinds::LEThan ? // choose += or -= for upto/downto
							OperKinds::PlusAssn : OperKinds::MinusAssn, new ExpressionNode( build_varref( new string( *index ) ) ), inc ) ) : 0 );
} // forCtrl

ForCtrl * forCtrl( ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( NameExpr * identifier = dynamic_cast<NameExpr *>(index->expr.get()) ) {
		return forCtrl( type, new string( identifier->name ), start, compop, comp, inc );
	} else if ( CommaExpr * commaExpr = dynamic_cast<CommaExpr *>(index->expr.get()) ) {
		if ( NameExpr * identifier = dynamic_cast<NameExpr *>(commaExpr->arg1 ) ) {
			return forCtrl( type, new string( identifier->name ), start, compop, comp, inc );
		} else {
			SemanticError( yylloc, "Expression disallowed. Only loop-index name allowed." ); return nullptr;
		} // if
	} else {
		SemanticError( yylloc, "Expression disallowed. Only loop-index name allowed." ); return nullptr;
	} // if
} // forCtrl

bool forall = false;									// aggregate have one or more forall qualifiers ?

// https://www.gnu.org/software/bison/manual/bison.html#Location-Type
#define YYLLOC_DEFAULT(Cur, Rhs, N)												\
if ( N ) {																		\
	(Cur).first_line   = YYRHSLOC( Rhs, 1 ).first_line;							\
	(Cur).first_column = YYRHSLOC( Rhs, 1 ).first_column;						\
	(Cur).last_line    = YYRHSLOC( Rhs, N ).last_line;							\
	(Cur).last_column  = YYRHSLOC( Rhs, N ).last_column;						\
	(Cur).filename     = YYRHSLOC( Rhs, 1 ).filename;							\
} else {																		\
	(Cur).first_line   = (Cur).last_line = YYRHSLOC( Rhs, 0 ).last_line;		\
	(Cur).first_column = (Cur).last_column = YYRHSLOC( Rhs, 0 ).last_column;	\
	(Cur).filename     = YYRHSLOC( Rhs, 0 ).filename;							\
}
%}

%define parse.error verbose

// Types declaration for productions
%union {
	Token tok;
	ParseNode * pn;
	ExpressionNode * en;
	DeclarationNode * decl;
	AggregateDecl::Aggregate aggKey;
	TypeDecl::Kind tclass;
	StatementNode * sn;
	WaitForStmt * wfs;
	Expression * constant;
	CondCtl * ifctl;
	ForCtrl * fctl;
	enum OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;
}

//************************* TERMINAL TOKENS ********************************

// keywords
%token TYPEDEF
%token EXTERN STATIC AUTO REGISTER
%token THREADLOCAL										// C11
%token INLINE FORTRAN									// C99, extension ISO/IEC 9899:1999 Section J.5.9(1)
%token NORETURN											// C11
%token CONST VOLATILE
%token RESTRICT											// C99
%token ATOMIC											// C11
%token FORALL MUTEX VIRTUAL VTABLE COERCE				// CFA
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED
%token BOOL COMPLEX IMAGINARY							// C99
%token INT128 UINT128 uuFLOAT80 uuFLOAT128				// GCC
%token uFLOAT16 uFLOAT32 uFLOAT32X uFLOAT64 uFLOAT64X uFLOAT128 // GCC
%token DECIMAL32 DECIMAL64 DECIMAL128					// GCC
%token ZERO_T ONE_T										// CFA
%token SIZEOF TYPEOF VALIST AUTO_TYPE					// GCC
%token OFFSETOF BASETYPEOF TYPEID						// CFA
%token ENUM STRUCT UNION
%token EXCEPTION										// CFA
%token GENERATOR COROUTINE MONITOR THREAD				// CFA
%token OTYPE FTYPE DTYPE TTYPE TRAIT					// CFA
// %token RESUME											// CFA
%token LABEL											// GCC
%token SUSPEND											// CFA
%token ATTRIBUTE EXTENSION								// GCC
%token IF ELSE SWITCH CASE DEFAULT DO WHILE FOR BREAK CONTINUE GOTO RETURN
%token CHOOSE FALLTHRU FALLTHROUGH WITH WHEN WAITFOR	// CFA
%token DISABLE ENABLE TRY THROW THROWRESUME AT			// CFA
%token ASM												// C99, extension ISO/IEC 9899:1999 Section J.5.10(1)
%token ALIGNAS ALIGNOF GENERIC STATICASSERT				// C11

// names and constants: lexer differentiates between identifier and typedef names
%token<tok> IDENTIFIER		QUOTED_IDENTIFIER	TYPEDIMname		TYPEDEFname		TYPEGENname
%token<tok> TIMEOUT			WOR					CATCH			RECOVER			CATCHRESUME		FIXUP		FINALLY		// CFA
%token<tok> INTEGERconstant	CHARACTERconstant	STRINGliteral
%token<tok> DIRECTIVE
// Floating point constant is broken into three kinds of tokens because of the ambiguity with tuple indexing and
// overloading constants 0/1, e.g., x.1 is lexed as (x)(.1), where (.1) is a factional constant, but is semantically
// converted into the tuple index (.)(1). e.g., 3.x
%token<tok>	FLOATING_DECIMALconstant	FLOATING_FRACTIONconstant	FLOATINGconstant

// multi-character operators
%token ARROW											// ->
%token ICR DECR											// ++	--
%token LS RS											// <<	>>
%token LE GE EQ NE										// <=	>=	==	!=
%token ANDAND OROR										// &&	||
%token ELLIPSIS											// ...

%token EXPassign	MULTassign	DIVassign	MODassign	// \=	*=	/=	%=
%token PLUSassign	MINUSassign							// +=	-=
%token LSassign		RSassign							// <<=	>>=
%token ANDassign	ERassign	ORassign				// &=	^=	|=

%token ErangeUpEq	ErangeDown	ErangeDownEq			// ~=	-~	-~=
%token ATassign											// @=

%type<tok> identifier					identifier_at				identifier_or_type_name		attr_name
%type<tok> quasi_keyword
%type<constant> string_literal
%type<str> string_literal_list

// expressions
%type<en> constant
%type<en> tuple							tuple_expression_list
%type<op> ptrref_operator				unary_operator				assignment_operator			simple_assignment_operator	compound_assignment_operator
%type<en> primary_expression			postfix_expression			unary_expression
%type<en> cast_expression_list			cast_expression				exponential_expression		multiplicative_expression	additive_expression
%type<en> shift_expression				relational_expression		equality_expression
%type<en> AND_expression				exclusive_OR_expression		inclusive_OR_expression
%type<en> logical_AND_expression		logical_OR_expression
%type<en> conditional_expression		constant_expression			assignment_expression		assignment_expression_opt
%type<en> comma_expression				comma_expression_opt
%type<en> argument_expression_list_opt	argument_expression_list	argument_expression			default_initializer_opt
%type<ifctl> conditional_declaration
%type<fctl> for_control_expression		for_control_expression_list
%type<compop> inclexcl
%type<en> subrange
%type<decl> asm_name_opt
%type<en> asm_operands_opt				asm_operands_list			asm_operand
%type<label> label_list
%type<en> asm_clobbers_list_opt
%type<flag> asm_volatile_opt
%type<en> handler_predicate_opt
%type<genexpr> generic_association		generic_assoc_list

// statements
%type<sn> statement						labeled_statement			compound_statement
%type<sn> statement_decl				statement_decl_list			statement_list_nodecl
%type<sn> selection_statement			if_statement
%type<sn> switch_clause_list_opt		switch_clause_list
%type<en> case_value
%type<sn> case_clause					case_value_list				case_label					case_label_list
%type<sn> iteration_statement			jump_statement
%type<sn> expression_statement			asm_statement
%type<sn> with_statement
%type<en> with_clause_opt
%type<sn> exception_statement			handler_clause				finally_clause
%type<catch_kind> handler_key
%type<sn> mutex_statement
%type<en> when_clause					when_clause_opt				waitfor						timeout
%type<sn> waitfor_statement
%type<wfs> waitfor_clause

// declarations
%type<decl> abstract_declarator abstract_ptr abstract_array abstract_function array_dimension multi_array_dimension
%type<decl> abstract_parameter_declarator abstract_parameter_ptr abstract_parameter_array abstract_parameter_function array_parameter_dimension array_parameter_1st_dimension
%type<decl> abstract_parameter_declaration

%type<aggKey> aggregate_key aggregate_data aggregate_control
%type<decl> aggregate_type aggregate_type_nobody

%type<decl> assertion assertion_list assertion_list_opt

%type<en> bit_subrange_size_opt bit_subrange_size

%type<decl> basic_declaration_specifier basic_type_name basic_type_specifier direct_type indirect_type
%type<decl> vtable vtable_opt default_opt

%type<decl> trait_declaration trait_declaration_list trait_declaring_list trait_specifier

%type<decl> declaration declaration_list declaration_list_opt declaration_qualifier_list
%type<decl> declaration_specifier declaration_specifier_nobody declarator declaring_list

%type<decl> elaborated_type elaborated_type_nobody

%type<decl> enumerator_list enum_type enum_type_nobody
%type<en> enumerator_value_opt

%type<decl> external_definition external_definition_list external_definition_list_opt

%type<decl> exception_declaration

%type<decl> field_declaration_list_opt field_declaration field_declaring_list_opt field_declarator field_abstract_list_opt field_abstract
%type<en> field field_name_list field_name fraction_constants_opt

%type<decl> external_function_definition function_definition function_array function_declarator function_no_ptr function_ptr

%type<decl> identifier_parameter_declarator identifier_parameter_ptr identifier_parameter_array identifier_parameter_function
%type<decl> identifier_list

%type<decl> cfa_abstract_array cfa_abstract_declarator_no_tuple cfa_abstract_declarator_tuple
%type<decl> cfa_abstract_function cfa_abstract_parameter_declaration cfa_abstract_parameter_list
%type<decl> cfa_abstract_ptr cfa_abstract_tuple

%type<decl> cfa_array_parameter_1st_dimension

%type<decl> cfa_trait_declaring_list cfa_declaration cfa_field_declaring_list cfa_field_abstract_list
%type<decl> cfa_function_declaration cfa_function_return cfa_function_specifier

%type<decl> cfa_identifier_parameter_array cfa_identifier_parameter_declarator_no_tuple
%type<decl> cfa_identifier_parameter_declarator_tuple cfa_identifier_parameter_ptr

%type<decl> cfa_parameter_declaration cfa_parameter_list cfa_parameter_ellipsis_list_opt

%type<decl> cfa_typedef_declaration cfa_variable_declaration cfa_variable_specifier

%type<decl> c_declaration static_assert
%type<decl> KR_function_declarator KR_function_no_ptr KR_function_ptr KR_function_array
%type<decl> KR_parameter_list KR_parameter_list_opt

%type<decl> parameter_declaration parameter_list parameter_type_list_opt

%type<decl> paren_identifier paren_type

%type<decl> storage_class storage_class_list

%type<decl> sue_declaration_specifier sue_declaration_specifier_nobody sue_type_specifier sue_type_specifier_nobody

%type<tclass> type_class new_type_class
%type<decl> type_declarator type_declarator_name type_declaring_list

%type<decl> type_declaration_specifier type_type_specifier type_name typegen_name
%type<decl> typedef_name typedef_declaration typedef_expression

%type<decl> variable_type_redeclarator type_ptr type_array type_function

%type<decl> type_parameter_redeclarator type_parameter_ptr type_parameter_array type_parameter_function

%type<decl> type type_no_function
%type<decl> type_parameter type_parameter_list type_initializer_opt

%type<en> type_parameters_opt type_list

%type<decl> type_qualifier type_qualifier_name forall type_qualifier_list_opt type_qualifier_list
%type<decl> type_specifier type_specifier_nobody

%type<decl> variable_declarator variable_ptr variable_array variable_function
%type<decl> variable_abstract_declarator variable_abstract_ptr variable_abstract_array variable_abstract_function

%type<decl> attribute_list_opt attribute_list attribute attribute_name_list attribute_name

// initializers
%type<in>  initializer initializer_list_opt initializer_opt

// designators
%type<en>  designator designator_list designation


// Handle shift/reduce conflict for dangling else by shifting the ELSE token. For example, this string is ambiguous:
//   .---------.				matches IF '(' comma_expression ')' statement . (reduce)
//   if ( C ) S1 else S2
//   `-----------------'		matches IF '(' comma_expression ')' statement . (shift) ELSE statement */
// Similar issues exit with the waitfor statement.

// Order of these lines matters (low-to-high precedence). THEN is left associative over WOR/TIMEOUT/ELSE, WOR is left
// associative over TIMEOUT/ELSE, and TIMEOUT is left associative over ELSE.
%precedence THEN		// rule precedence for IF/WAITFOR statement
%precedence WOR			// token precedence for start of WOR in WAITFOR statement
%precedence TIMEOUT		// token precedence for start of TIMEOUT in WAITFOR statement
%precedence CATCH		// token precedence for start of TIMEOUT in WAITFOR statement
%precedence RECOVER		// token precedence for start of TIMEOUT in WAITFOR statement
%precedence CATCHRESUME	// token precedence for start of TIMEOUT in WAITFOR statement
%precedence FIXUP		// token precedence for start of TIMEOUT in WAITFOR statement
%precedence FINALLY		// token precedence for start of TIMEOUT in WAITFOR statement
%precedence ELSE		// token precedence for start of else clause in IF/WAITFOR statement


// Handle shift/reduce conflict for generic type by shifting the '(' token. For example, this string is ambiguous:
//   forall( otype T ) struct Foo { T v; };
//       .-----.				matches pointer to function returning a generic (which is impossible without a type)
//   Foo ( *fp )( int );
//   `---'						matches start of TYPEGENname '('
// must be:
//   Foo( int ) ( *fp )( int );
// The same problem occurs here:
//   forall( otype T ) struct Foo { T v; } ( *fp )( int );
// must be:
//   forall( otype T ) struct Foo { T v; } ( int ) ( *fp )( int );

// Order of these lines matters (low-to-high precedence).
%precedence TYPEGENname
%precedence '}'
%precedence '('

// %precedence RESUME
// %precedence '{'
// %precedence ')'

%locations												// support location tracking for error messages

%start translation_unit									// parse-tree root

%%
//************************* Namespace Management ********************************

// The C grammar is not context free because it relies on the distinct terminal symbols "identifier" and "TYPEDEFname",
// which are lexically identical.
//
//   typedef int foo; // identifier foo must now be scanned as TYPEDEFname
//   foo f;           // to allow it to appear in this context
//
// While it may be possible to write a purely context-free grammar, such a grammar would obscure the relationship
// between syntactic and semantic constructs.  Cforall compounds this problem by introducing type names local to the
// scope of a declaration (for instance, those introduced through "forall" qualifiers), and by introducing "type
// generators" -- parameterized types.  This latter type name creates a third class of identifiers, "TYPEGENname", which
// must be distinguished by the lexical scanner.
//
// Since the scanner cannot distinguish among the different classes of identifiers without some context information,
// there is a type table (typedefTable), which holds type names and identifiers that override type names, for each named
// scope. During parsing, semantic actions update the type table by adding new identifiers in the current scope. For
// each context that introduces a name scope, a new level is created in the type table and that level is popped on
// exiting the scope.  Since type names can be local to a particular declaration, each declaration is itself a scope.
// This requires distinguishing between type names that are local to the current declaration scope and those that
// persist past the end of the declaration (i.e., names defined in "typedef" or "otype" declarations).
//
// The non-terminals "push" and "pop" denote the opening and closing of named scopes. Every push has a matching pop in
// the production rule. There are multiple lists of declarations, where each declaration is a named scope, so pop/push
// around the list separator.
//
//  int f( forall(T) T (*f1) T , forall( S ) S (*f2)( S ) );
//      push               pop   push                   pop

push:
		{ typedefTable.enterScope(); }
	;

pop:
		{ typedefTable.leaveScope(); }
	;

//************************* CONSTANTS ********************************

constant:
		// ENUMERATIONconstant is not included here; it is treated as a variable with type "enumeration constant".
	INTEGERconstant								{ $$ = new ExpressionNode( build_constantInteger( *$1 ) ); }
	| FLOATING_DECIMALconstant					{ $$ = new ExpressionNode( build_constantFloat( *$1 ) ); }
	| FLOATING_FRACTIONconstant					{ $$ = new ExpressionNode( build_constantFloat( *$1 ) ); }
	| FLOATINGconstant							{ $$ = new ExpressionNode( build_constantFloat( *$1 ) ); }
	| CHARACTERconstant							{ $$ = new ExpressionNode( build_constantChar( *$1 ) ); }
	;

quasi_keyword:											// CFA
	TIMEOUT
	| WOR
	| CATCH
	| RECOVER
	| CATCHRESUME
	| FIXUP
	| FINALLY
	;

identifier:
	IDENTIFIER
	| quasi_keyword
	;

identifier_at:
	identifier
	| '@'												// CFA
		{ Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; $$ = tok; }
	;

string_literal:
	string_literal_list							{ $$ = build_constantStr( *$1 ); }
	;

string_literal_list:									// juxtaposed strings are concatenated
	STRINGliteral								{ $$ = $1; } // conversion from tok to str
	| string_literal_list STRINGliteral
		{
			if ( ! appendStr( *$1, *$2 ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete $2;									// allocated by lexer
			$$ = $1;									// conversion from tok to str
		}
	;

//************************* EXPRESSIONS ********************************

primary_expression:
	IDENTIFIER											// typedef name cannot be used as a variable name
		{ $$ = new ExpressionNode( build_varref( $1 ) ); }
	| quasi_keyword
		{ $$ = new ExpressionNode( build_varref( $1 ) ); }
	| TYPEDIMname										// CFA, generic length argument
		// { $$ = new ExpressionNode( new TypeExpr( maybeMoveBuildType( DeclarationNode::newFromTypedef( $1 ) ) ) ); }
		// { $$ = new ExpressionNode( build_varref( $1 ) ); }
		{ $$ = new ExpressionNode( build_dimensionref( $1 ) ); }
	| tuple
	| '(' comma_expression ')'
		{ $$ = $2; }
	| '(' compound_statement ')'						// GCC, lambda expression
		{ $$ = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>($2) ) ) ); }
	| type_name '.' identifier							// CFA, nested type
		{ SemanticError( yylloc, "Qualified name is currently unimplemented." ); $$ = nullptr; }
	| type_name '.' '[' field_name_list ']'				// CFA, nested type / tuple field selector
		{ SemanticError( yylloc, "Qualified name is currently unimplemented." ); $$ = nullptr; }
	| GENERIC '(' assignment_expression ',' generic_assoc_list ')' // C11
		{
			// add the missing control expression to the GenericExpr and return it
			$5->control = maybeMoveBuild<Expression>( $3 );
			$$ = new ExpressionNode( $5 );
		}
	// | RESUME '(' comma_expression ')'
	//   	{ SemanticError( yylloc, "Resume expression is currently unimplemented." ); $$ = nullptr; }
	// | RESUME '(' comma_expression ')' compound_statement
	//   	{ SemanticError( yylloc, "Resume expression is currently unimplemented." ); $$ = nullptr; }
	;

generic_assoc_list:										// C11
	generic_association
	| generic_assoc_list ',' generic_association
		{
			// steal the association node from the singleton and delete the wrapper
			$1->associations.splice($1->associations.end(), $3->associations);
			delete $3;
			$$ = $1;
		}
	;

generic_association:									// C11
	type_no_function ':' assignment_expression
		{
			// create a GenericExpr wrapper with one association pair
			$$ = new GenericExpr( nullptr, { { maybeMoveBuildType($1), maybeMoveBuild<Expression>( $3 ) } } );
		}
	| DEFAULT ':' assignment_expression
		{ $$ = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( $3 ) } } ); }
	;

postfix_expression:
	primary_expression
	| postfix_expression '[' assignment_expression ',' tuple_expression_list ']'
			// Historic, transitional: Disallow commas in subscripts.
			// Switching to this behaviour may help check if a C compatibilty case uses comma-exprs in subscripts.
		// { SemanticError( yylloc, "New array subscript is currently unimplemented." ); $$ = nullptr; }
			// Current: Commas in subscripts make tuples.
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Index, $1, new ExpressionNode( build_tuple( (ExpressionNode *)($3->set_last( $5 ) ) )) ) ); }
	| postfix_expression '[' assignment_expression ']'
		// CFA, comma_expression disallowed in this context because it results in a common user error: subscripting a
		// matrix with x[i,j] instead of x[i][j]. While this change is not backwards compatible, there seems to be
		// little advantage to this feature and many disadvantages. It is possible to write x[(i,j)] in CFA, which is
		// equivalent to the old x[i,j].
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Index, $1, $3 ) ); }
	| postfix_expression '{' argument_expression_list_opt '}' // CFA, constructor call
		{
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			$$ = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( $1 )->set_last( $3 ) ) ) );
		}
	| postfix_expression '(' argument_expression_list_opt ')'
		{ $$ = new ExpressionNode( build_func( $1, $3 ) ); }
	| postfix_expression '`' identifier					// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( $3 ) ) ), $1 ) ); }
	| constant '`' identifier							// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( $3 ) ) ), $1 ) ); }
	| string_literal '`' identifier						// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( $3 ) ) ), new ExpressionNode( $1 ) ) ); }
	| postfix_expression '.' identifier
		{ $$ = new ExpressionNode( build_fieldSel( $1, build_varref( $3 ) ) ); }
	| postfix_expression '.' INTEGERconstant			// CFA, tuple index
		{ $$ = new ExpressionNode( build_fieldSel( $1, build_constantInteger( *$3 ) ) ); }
	| postfix_expression FLOATING_FRACTIONconstant		// CFA, tuple index
		{ $$ = new ExpressionNode( build_fieldSel( $1, build_field_name_FLOATING_FRACTIONconstant( *$2 ) ) ); }
	| postfix_expression '.' '[' field_name_list ']'	// CFA, tuple field selector
		{ $$ = new ExpressionNode( build_fieldSel( $1, build_tuple( $4 ) ) ); }
	| postfix_expression '.' aggregate_control
		{ $$ = new ExpressionNode( build_keyword_cast( $3, $1 ) ); }
	| postfix_expression ARROW identifier
		{ $$ = new ExpressionNode( build_pfieldSel( $1, build_varref( $3 ) ) ); }
	| postfix_expression ARROW INTEGERconstant			// CFA, tuple index
		{ $$ = new ExpressionNode( build_pfieldSel( $1, build_constantInteger( *$3 ) ) ); }
	| postfix_expression ARROW '[' field_name_list ']'	// CFA, tuple field selector
		{ $$ = new ExpressionNode( build_pfieldSel( $1, build_tuple( $4 ) ) ); }
	| postfix_expression ICR
	  	{ $$ = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, $1 ) ); }
	| postfix_expression DECR
	  	{ $$ = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, $1 ) ); }
	| '(' type_no_function ')' '{' initializer_list_opt comma_opt '}' // C99, compound-literal
		{ $$ = new ExpressionNode( build_compoundLiteral( $2, new InitializerNode( $5, true ) ) ); }
	| '(' type_no_function ')' '@' '{' initializer_list_opt comma_opt '}' // CFA, explicit C compound-literal
		{ $$ = new ExpressionNode( build_compoundLiteral( $2, (new InitializerNode( $6, true ))->set_maybeConstructed( false ) ) ); }
	| '^' primary_expression '{' argument_expression_list_opt '}' // CFA, destructor call
		{
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			$$ = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( $2 )->set_last( $4 ) ) );
		}
	;

argument_expression_list_opt:
	// empty
		{ $$ = nullptr; }
	| argument_expression_list
	;

argument_expression_list:
	argument_expression
	| argument_expression_list_opt ',' argument_expression
		{ $$ = (ExpressionNode *)($1->set_last( $3 )); }
	;

argument_expression:
	'@'													// CFA, default parameter
		{ SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); $$ = nullptr; }
	 	// { $$ = new ExpressionNode( build_constantInteger( *new string( "2" ) ) ); }
	| assignment_expression
	;

field_name_list:										// CFA, tuple field selector
	field
	| field_name_list ',' field					{ $$ = (ExpressionNode *)($1->set_last( $3 )); }
	;

field:													// CFA, tuple field selector
	field_name
	| FLOATING_DECIMALconstant field
		{ $$ = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *$1 ) ), maybeMoveBuild<Expression>( $2 ) ) ); }
	| FLOATING_DECIMALconstant '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *$1 ) ), build_tuple( $3 ) ) ); }
	| field_name '.' field
		{ $$ = new ExpressionNode( build_fieldSel( $1, maybeMoveBuild<Expression>( $3 ) ) ); }
	| field_name '.' '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_fieldSel( $1, build_tuple( $4 ) ) ); }
	| field_name ARROW field
		{ $$ = new ExpressionNode( build_pfieldSel( $1, maybeMoveBuild<Expression>( $3 ) ) ); }
	| field_name ARROW '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_pfieldSel( $1, build_tuple( $4 ) ) ); }
	;

field_name:
	INTEGERconstant	fraction_constants_opt
		{ $$ = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *$1 ), $2 ) ); }
	| FLOATINGconstant fraction_constants_opt
		{ $$ = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *$1 ), $2 ) ); }
	| identifier_at fraction_constants_opt				// CFA, allow anonymous fields
		{
			$$ = new ExpressionNode( build_field_name_fraction_constants( build_varref( $1 ), $2 ) );
		}
	;

fraction_constants_opt:
	// empty
		{ $$ = nullptr; }
	| fraction_constants_opt FLOATING_FRACTIONconstant
		{
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *$2 );
			$$ = $1 != nullptr ? new ExpressionNode( build_fieldSel( $1,  constant ) ) : new ExpressionNode( constant );
		}
	;

unary_expression:
	postfix_expression
		// first location where constant/string can have operator applied: sizeof 3/sizeof "abc" still requires
		// semantics checks, e.g., ++3, 3--, *3, &&3
	| constant
	| string_literal
		{ $$ = new ExpressionNode( $1 ); }
	| EXTENSION cast_expression							// GCC
		{ $$ = $2->set_extension( true ); }
		// '*' ('&') is separated from unary_operator because of shift/reduce conflict in:
		//		{ * X; }	 // dereference X
		//		{ * int X; } // CFA declaration of pointer to int
	| ptrref_operator cast_expression					// CFA
		{
			switch ( $1 ) {
			  case OperKinds::AddressOf:
				$$ = new ExpressionNode( new AddressExpr( maybeMoveBuild<Expression>( $2 ) ) );
				break;
			  case OperKinds::PointTo:
				$$ = new ExpressionNode( build_unary_val( $1, $2 ) );
				break;
			  case OperKinds::And:
				$$ = new ExpressionNode( new AddressExpr( new AddressExpr( maybeMoveBuild<Expression>( $2 ) ) ) );
				break;
			  default:
				assert( false );
			}
		}
	| unary_operator cast_expression
	  	{ $$ = new ExpressionNode( build_unary_val( $1, $2 ) ); }
	| ICR unary_expression
	  	{ $$ = new ExpressionNode( build_unary_ptr( OperKinds::Incr, $2 ) ); }
	| DECR unary_expression
	  	{ $$ = new ExpressionNode( build_unary_ptr( OperKinds::Decr, $2 ) ); }
	| SIZEOF unary_expression
		{ $$ = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( $2 ) ) ); }
	| SIZEOF '(' type_no_function ')'
		{ $$ = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( $3 ) ) ); }
	| ALIGNOF unary_expression							// GCC, variable alignment
		{ $$ = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( $2 ) ) ); }
	| ALIGNOF '(' type_no_function ')'					// GCC, type alignment
		{ $$ = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( $3 ) ) ); }
	| OFFSETOF '(' type_no_function ',' identifier ')'
		{ $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) ); }
	| TYPEID '(' type_no_function ')'
		{
			SemanticError( yylloc, "typeid name is currently unimplemented." ); $$ = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
	;

ptrref_operator:
	'*'											{ $$ = OperKinds::PointTo; }
	| '&'										{ $$ = OperKinds::AddressOf; }
		// GCC, address of label must be handled by semantic check for ref,ref,label
	| ANDAND									{ $$ = OperKinds::And; }
	;

unary_operator:
	'+'											{ $$ = OperKinds::UnPlus; }
	| '-'										{ $$ = OperKinds::UnMinus; }
	| '!'										{ $$ = OperKinds::Neg; }
	| '~'										{ $$ = OperKinds::BitNeg; }
	;

cast_expression:
	unary_expression
	| '(' type_no_function ')' cast_expression
		{ $$ = new ExpressionNode( build_cast( $2, $4 ) ); }
	| '(' aggregate_control '&' ')' cast_expression		// CFA
		{ $$ = new ExpressionNode( build_keyword_cast( $2, $5 ) ); }
	| '(' aggregate_control '*' ')' cast_expression		// CFA
		{ $$ = new ExpressionNode( build_keyword_cast( $2, $5 ) ); }
	| '(' VIRTUAL ')' cast_expression					// CFA
		{ $$ = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( $4 ), maybeMoveBuildType( nullptr ) ) ); }
	| '(' VIRTUAL type_no_function ')' cast_expression	// CFA
		{ $$ = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( $5 ), maybeMoveBuildType( $3 ) ) ); }
	| '(' RETURN type_no_function ')' cast_expression	// CFA
		{ SemanticError( yylloc, "Return cast is currently unimplemented." ); $$ = nullptr; }
	| '(' COERCE type_no_function ')' cast_expression	// CFA
		{ SemanticError( yylloc, "Coerce cast is currently unimplemented." ); $$ = nullptr; }
	| '(' qualifier_cast_list ')' cast_expression		// CFA
		{ SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); $$ = nullptr; }
//	| '(' type_no_function ')' tuple
//		{ $$ = new ExpressionNode( build_cast( $2, $4 ) ); }
	;

qualifier_cast_list:
	cast_modifier type_qualifier_name
	| cast_modifier MUTEX
	| qualifier_cast_list cast_modifier type_qualifier_name
	| qualifier_cast_list cast_modifier MUTEX
	;

cast_modifier:
	'-'
	| '+'
	;

exponential_expression:
	cast_expression
	| exponential_expression '\\' cast_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Exp, $1, $3 ) ); }
	;

multiplicative_expression:
	exponential_expression
	| multiplicative_expression '*' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Mul, $1, $3 ) ); }
	| multiplicative_expression '/' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Div, $1, $3 ) ); }
	| multiplicative_expression '%' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Mod, $1, $3 ) ); }
	;

additive_expression:
	multiplicative_expression
	| additive_expression '+' multiplicative_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Plus, $1, $3 ) ); }
	| additive_expression '-' multiplicative_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Minus, $1, $3 ) ); }
	;

shift_expression:
	additive_expression
	| shift_expression LS additive_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::LShift, $1, $3 ) ); }
	| shift_expression RS additive_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::RShift, $1, $3 ) ); }
	;

relational_expression:
	shift_expression
	| relational_expression '<' shift_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::LThan, $1, $3 ) ); }
	| relational_expression '>' shift_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::GThan, $1, $3 ) ); }
	| relational_expression LE shift_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::LEThan, $1, $3 ) ); }
	| relational_expression GE shift_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::GEThan, $1, $3 ) ); }
	;

equality_expression:
	relational_expression
	| equality_expression EQ relational_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Eq, $1, $3 ) ); }
	| equality_expression NE relational_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Neq, $1, $3 ) ); }
	;

AND_expression:
	equality_expression
	| AND_expression '&' equality_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::BitAnd, $1, $3 ) ); }
	;

exclusive_OR_expression:
	AND_expression
	| exclusive_OR_expression '^' AND_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::Xor, $1, $3 ) ); }
	;

inclusive_OR_expression:
	exclusive_OR_expression
	| inclusive_OR_expression '|' exclusive_OR_expression
		{ $$ = new ExpressionNode( build_binary_val( OperKinds::BitOr, $1, $3 ) ); }
	;

logical_AND_expression:
	inclusive_OR_expression
	| logical_AND_expression ANDAND inclusive_OR_expression
		{ $$ = new ExpressionNode( build_and_or( $1, $3, true ) ); }
	;

logical_OR_expression:
	logical_AND_expression
	| logical_OR_expression OROR logical_AND_expression
		{ $$ = new ExpressionNode( build_and_or( $1, $3, false ) ); }
	;

conditional_expression:
	logical_OR_expression
	| logical_OR_expression '?' comma_expression ':' conditional_expression
		{ $$ = new ExpressionNode( build_cond( $1, $3, $5 ) ); }
		// FIX ME: computes $1 twice
	| logical_OR_expression '?' /* empty */ ':' conditional_expression // GCC, omitted first operand
		{ $$ = new ExpressionNode( build_cond( $1, $1, $4 ) ); }
	;

constant_expression:
	conditional_expression
	;

assignment_expression:
		// CFA, assignment is separated from assignment_operator to ensure no assignment operations for tuples
	conditional_expression
	| unary_expression assignment_operator assignment_expression
		{
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				$$ = new ExpressionNode( build_binary_val( $2, $1, $3 ) );
//			} // if
		}
	| unary_expression '=' '{' initializer_list_opt comma_opt '}'
		{ SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); $$ = nullptr; }
	;

assignment_expression_opt:
	// empty
		{ $$ = nullptr; }
	| assignment_expression
	;

assignment_operator:
	simple_assignment_operator
	| compound_assignment_operator
	;

simple_assignment_operator:
	'='											{ $$ = OperKinds::Assign; }
	| ATassign									{ $$ = OperKinds::AtAssn; } // CFA
	;

compound_assignment_operator:
	EXPassign									{ $$ = OperKinds::ExpAssn; }
	| MULTassign								{ $$ = OperKinds::MulAssn; }
	| DIVassign									{ $$ = OperKinds::DivAssn; }
	| MODassign									{ $$ = OperKinds::ModAssn; }
	| PLUSassign								{ $$ = OperKinds::PlusAssn; }
	| MINUSassign								{ $$ = OperKinds::MinusAssn; }
	| LSassign									{ $$ = OperKinds::LSAssn; }
	| RSassign									{ $$ = OperKinds::RSAssn; }
	| ANDassign									{ $$ = OperKinds::AndAssn; }
	| ERassign									{ $$ = OperKinds::ERAssn; }
	| ORassign									{ $$ = OperKinds::OrAssn; }
	;

tuple:													// CFA, tuple
		// CFA, one assignment_expression is factored out of comma_expression to eliminate a shift/reduce conflict with
		// comma_expression in cfa_identifier_parameter_array and cfa_abstract_array
//	'[' ']'
//		{ $$ = new ExpressionNode( build_tuple() ); }
//	| '[' push assignment_expression pop ']'
//		{ $$ = new ExpressionNode( build_tuple( $3 ) ); }
	'[' ',' tuple_expression_list ']'
		{ $$ = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( $3 ) ) ); }
	| '[' push assignment_expression pop ',' tuple_expression_list ']'
		{ $$ = new ExpressionNode( build_tuple( (ExpressionNode *)($3->set_last( $6 ) ) )); }
	;

tuple_expression_list:
	assignment_expression
	| '@'												// CFA
		{ SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); $$ = nullptr; }
	| tuple_expression_list ',' assignment_expression
		{ $$ = (ExpressionNode *)($1->set_last( $3 )); }
	| tuple_expression_list ',' '@'
		{ SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); $$ = nullptr; }
	;

comma_expression:
	assignment_expression
	| comma_expression ',' assignment_expression
		{ $$ = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( $1 ), maybeMoveBuild<Expression>( $3 ) ) ); }
	;

comma_expression_opt:
	// empty
		{ $$ = nullptr; }
	| comma_expression
	;

//*************************** STATEMENTS *******************************

statement:
	labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| with_statement
	| mutex_statement
	| waitfor_statement
	| exception_statement
	| enable_disable_statement
		{ SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); $$ = nullptr; }
	| asm_statement
	| DIRECTIVE
		{ $$ = new StatementNode( build_directive( $1 ) ); }
	;

labeled_statement:
		// labels cannot be identifiers 0 or 1
	identifier_or_type_name ':' attribute_list_opt statement
		{ $$ = $4->add_label( $1, $3 ); }
	;

compound_statement:
	'{' '}'
		{ $$ = new StatementNode( build_compound( (StatementNode *)0 ) ); }
	| '{' push
	  local_label_declaration_opt						// GCC, local labels appear at start of block
	  statement_decl_list								// C99, intermix declarations and statements
	  pop '}'
		{ $$ = new StatementNode( build_compound( $4 ) ); }
	;

statement_decl_list:									// C99
	statement_decl
	| statement_decl_list statement_decl
		{ assert( $1 ); $1->set_last( $2 ); $$ = $1; }
	;

statement_decl:
	declaration											// CFA, new & old style declarations
		{ $$ = new StatementNode( $1 ); }
	| EXTENSION declaration								// GCC
		{ distExt( $2 ); $$ = new StatementNode( $2 ); }
	| function_definition
		{ $$ = new StatementNode( $1 ); }
	| EXTENSION function_definition						// GCC
		{ distExt( $2 ); $$ = new StatementNode( $2 ); }
	| statement
	;

statement_list_nodecl:
	statement
	| statement_list_nodecl statement
		{ assert( $1 ); $1->set_last( $2 ); $$ = $1; }
	;

expression_statement:
	comma_expression_opt ';'
		{ $$ = new StatementNode( build_expr( $1 ) ); }
	| MUTEX '(' ')' comma_expression ';'
		{ $$ = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( $4 ) ) ) ); }
		// { SemanticError( yylloc, "Mutex expression is currently unimplemented." ); $$ = nullptr; }
	;

selection_statement:
			// pop causes a S/R conflict without separating the IF statement into a non-terminal even after resolving
			// the inherent S/R conflict with THEN/ELSE.
	push if_statement pop
		{ $$ = $2; }
	| SWITCH '(' comma_expression ')' case_clause
		{ $$ = new StatementNode( build_switch( true, $3, $5 ) ); }
	| SWITCH '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}' // CFA
		{
			StatementNode *sw = new StatementNode( build_switch( true, $3, $8 ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			$$ = $7 ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( $7 ))->set_last( sw )) ) ) : sw;
		}
	| CHOOSE '(' comma_expression ')' case_clause		// CFA
		{ $$ = new StatementNode( build_switch( false, $3, $5 ) ); }
	| CHOOSE '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}' // CFA
		{
			StatementNode *sw = new StatementNode( build_switch( false, $3, $8 ) );
			$$ = $7 ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( $7 ))->set_last( sw )) ) ) : sw;
		}
	;

if_statement:
	IF '(' conditional_declaration ')' statement		%prec THEN
		// explicitly deal with the shift/reduce conflict on if/else
		{ $$ = new StatementNode( build_if( $3, maybe_build_compound( $5 ), nullptr ) ); }
	| IF '(' conditional_declaration ')' statement ELSE statement
		{ $$ = new StatementNode( build_if( $3, maybe_build_compound( $5 ), maybe_build_compound( $7 ) ) ); }
	;

conditional_declaration:
	comma_expression
		{ $$ = new CondCtl( nullptr, $1 ); }
	| c_declaration										// no semi-colon
		{ $$ = new CondCtl( $1, nullptr ); }
	| cfa_declaration									// no semi-colon
		{ $$ = new CondCtl( $1, nullptr ); }
	| declaration comma_expression						// semi-colon separated
		{ $$ = new CondCtl( $1, $2 ); }
 	;

// CASE and DEFAULT clauses are only allowed in the SWITCH statement, precluding Duff's device. In addition, a case
// clause allows a list of values and subranges.

case_value:												// CFA
	constant_expression							{ $$ = $1; }
	| constant_expression ELLIPSIS constant_expression	// GCC, subrange
		{ $$ = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( $1 ), maybeMoveBuild<Expression>( $3 ) ) ); }
	| subrange											// CFA, subrange
	;

case_value_list:										// CFA
	case_value									{ $$ = new StatementNode( build_case( $1 ) ); }
		// convert case list, e.g., "case 1, 3, 5:" into "case 1: case 3: case 5"
	| case_value_list ',' case_value			{ $$ = (StatementNode *)($1->set_last( new StatementNode( build_case( $3 ) ) ) ); }
	;

case_label:												// CFA
	CASE case_value_list ':'					{ $$ = $2; }
	| DEFAULT ':'								{ $$ = new StatementNode( build_default() ); }
		// A semantic check is required to ensure only one default clause per switch/choose statement.
	;

//label_list_opt:
//	// empty
//	| identifier_or_type_name ':'
//	| label_list_opt identifier_or_type_name ':'
//	;

case_label_list:										// CFA
	case_label
	| case_label_list case_label				{ $$ = (StatementNode *)( $1->set_last( $2 )); }
	;

case_clause:											// CFA
	case_label_list statement					{ $$ = $1->append_last_case( maybe_build_compound( $2 ) ); }
	;

switch_clause_list_opt:									// CFA
	// empty
		{ $$ = nullptr; }
	| switch_clause_list
	;

switch_clause_list:										// CFA
	case_label_list statement_list_nodecl
		{ $$ = $1->append_last_case( new StatementNode( build_compound( $2 ) ) ); }
	| switch_clause_list case_label_list statement_list_nodecl
		{ $$ = (StatementNode *)( $1->set_last( $2->append_last_case( new StatementNode( build_compound( $3 ) ) ) ) ); }
	;

iteration_statement:
	WHILE '(' ')' statement								// CFA => while ( 1 )
		{ $$ = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( $4 ) ) ); }
	| WHILE '(' conditional_declaration ')' statement	%prec THEN
		{ $$ = new StatementNode( build_while( $3, maybe_build_compound( $5 ) ) ); }
	| WHILE '(' conditional_declaration ')' statement ELSE statement // CFA
		// { SemanticError( yylloc, "Loop default block is currently unimplemented." ); $$ = nullptr; }
		{ $$ = new StatementNode( build_while( $3, maybe_build_compound( $5 ), $7 ) ); }
	| DO statement WHILE '(' ')' ';'					// CFA => do while( 1 )
		{ $$ = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( $2 ) ) ); }
	| DO statement WHILE '(' comma_expression ')' ';'	%prec THEN
		{ $$ = new StatementNode( build_do_while( $5, maybe_build_compound( $2 ) ) ); }
	| DO statement WHILE '(' comma_expression ')' ELSE statement // CFA
		// { SemanticError( yylloc, "Loop default block is currently unimplemented." ); $$ = nullptr; }
		{ $$ = new StatementNode( build_do_while( $5, maybe_build_compound( $2 ), $8 ) ); }
	| FOR '(' ')' statement								// CFA => for ( ;; )
		{ $$ = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( $4 ) ) ); }
	| FOR '(' for_control_expression_list ')' statement	%prec THEN
	  	{ $$ = new StatementNode( build_for( $3, maybe_build_compound( $5 ) ) ); }
	| FOR '(' for_control_expression_list ')' statement ELSE statement // CFA
		// { SemanticError( yylloc, "Loop default block is currently unimplemented." ); $$ = nullptr; }
		{ $$ = new StatementNode( build_for( $3, maybe_build_compound( $5 ), $7 ) ); }
	;

for_control_expression_list:
	for_control_expression
	| for_control_expression_list ':' for_control_expression
		// ForCtrl + ForCtrl:
		//    init + init => multiple declaration statements that are hoisted
		//    condition + condition => (expression) && (expression)
		//    change + change => (expression), (expression)
		{
			$1->init->set_last( $3->init );
			if ( $1->condition ) {
				if ( $3->condition ) {
					$1->condition->expr.reset( new LogicalExpr( $1->condition->expr.release(), $3->condition->expr.release(), true ) );
				} // if
			} else $1->condition = $3->condition;
			if ( $1->change ) {
				if ( $3->change ) {
					$1->change->expr.reset( new CommaExpr( $1->change->expr.release(), $3->change->expr.release() ) );
				} // if
			} else $1->change = $3->change;
			$$ = $1;
		}
	;

for_control_expression:
	';' comma_expression_opt ';' comma_expression_opt
		{ $$ = new ForCtrl( (ExpressionNode * )nullptr, $2, $4 ); }
	| comma_expression ';' comma_expression_opt ';' comma_expression_opt
		{ $$ = new ForCtrl( $1, $3, $5 ); }
	| declaration comma_expression_opt ';' comma_expression_opt // C99, declaration has ';'
		{ $$ = new ForCtrl( $1, $2, $4 ); }

	| comma_expression									// CFA
		{ $$ = forCtrl( $1, new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, $1->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| '=' comma_expression								// CFA
		{ $$ = forCtrl( $2, new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, $2->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression inclexcl comma_expression		// CFA
		{ $$ = forCtrl( $1, new string( DeclarationNode::anonymous.newName() ), $1->clone(), $2, $3, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression inclexcl comma_expression '~' comma_expression // CFA
		{ $$ = forCtrl( $1, new string( DeclarationNode::anonymous.newName() ), $1->clone(), $2, $3, $5 ); }
	| comma_expression ';'								// CFA
		{ $$ = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), $1, nullptr, OperKinds::LThan, nullptr, nullptr ); }
	| comma_expression ';' comma_expression				// CFA
		{ $$ = forCtrl( $3, $1, new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, $3->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression ';' '=' comma_expression			// CFA
		{ $$ = forCtrl( $4, $1, new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, $4->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression ';' comma_expression inclexcl comma_expression // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), $4, $5, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression ';' comma_expression inclexcl comma_expression '~' comma_expression // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), $4, $5, $7 ); }

	| comma_expression ';' TYPEDEFname					// CFA, array type
		{
			SemanticError( yylloc, "Array interator is currently unimplemented." ); $$ = nullptr;
			$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}

		// There is a S/R conflicit if ~ and -~ are factored out.
	| comma_expression ';' comma_expression '~' '@'		// CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression ';' comma_expression ErangeDown '@' // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
	| comma_expression ';' comma_expression '~' '@' '~' comma_expression // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), OperKinds::LThan, nullptr, $7 ); }
	| comma_expression ';' comma_expression ErangeDown '@' '~' comma_expression // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), OperKinds::GThan, nullptr, $7 ); }
	| comma_expression ';' comma_expression '~' '@' '~' '@' // CFA
		{ $$ = forCtrl( $3, $1, $3->clone(), OperKinds::LThan, nullptr, nullptr ); }
 	;

inclexcl:
	'~'
		{ $$ = OperKinds::LThan; }
	| ErangeUpEq
		{ $$ = OperKinds::LEThan; }
	| ErangeDown
		{ $$ = OperKinds::GThan; }
	| ErangeDownEq
		{ $$ = OperKinds::GEThan; }
 	;

jump_statement:
	GOTO identifier_or_type_name ';'
		{ $$ = new StatementNode( build_branch( $2, BranchStmt::Goto ) ); }
	| GOTO '*' comma_expression ';'						// GCC, computed goto
		// The syntax for the GCC computed goto violates normal expression precedence, e.g., goto *i+3; => goto *(i+3);
		// whereas normal operator precedence yields goto (*i)+3;
		{ $$ = new StatementNode( build_computedgoto( $3 ) ); }
		// A semantic check is required to ensure fallthru appears only in the body of a choose statement.
	| fall_through_name ';'								// CFA
		{ $$ = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
	| fall_through_name identifier_or_type_name ';'		// CFA
		{ $$ = new StatementNode( build_branch( $2, BranchStmt::FallThrough ) ); }
	| fall_through_name DEFAULT ';'						// CFA
		{ $$ = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
	| CONTINUE ';'
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement.
		{ $$ = new StatementNode( build_branch( BranchStmt::Continue ) ); }
	| CONTINUE identifier_or_type_name ';'				// CFA, multi-level continue
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement, and
		// the target of the transfer appears only at the start of an iteration statement.
		{ $$ = new StatementNode( build_branch( $2, BranchStmt::Continue ) ); }
	| BREAK ';'
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement.
		{ $$ = new StatementNode( build_branch( BranchStmt::Break ) ); }
	| BREAK identifier_or_type_name ';'					// CFA, multi-level exit
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement, and
		// the target of the transfer appears only at the start of an iteration statement.
		{ $$ = new StatementNode( build_branch( $2, BranchStmt::Break ) ); }
	| RETURN comma_expression_opt ';'
		{ $$ = new StatementNode( build_return( $2 ) ); }
	| RETURN '{' initializer_list_opt comma_opt '}' ';'
		{ SemanticError( yylloc, "Initializer return is currently unimplemented." ); $$ = nullptr; }
	| SUSPEND ';'
		{ $$ = new StatementNode( build_suspend( nullptr ) ); }
	| SUSPEND compound_statement
		{ $$ = new StatementNode( build_suspend( $2 ) ); }
	| SUSPEND COROUTINE ';'
		{ $$ = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
	| SUSPEND COROUTINE compound_statement
		{ $$ = new StatementNode( build_suspend( $3, SuspendStmt::Coroutine ) ); }
	| SUSPEND GENERATOR ';'
		{ $$ = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
	| SUSPEND GENERATOR compound_statement
		{ $$ = new StatementNode( build_suspend( $3, SuspendStmt::Generator ) ); }
	| THROW assignment_expression_opt ';'				// handles rethrow
		{ $$ = new StatementNode( build_throw( $2 ) ); }
	| THROWRESUME assignment_expression_opt ';'			// handles reresume
		{ $$ = new StatementNode( build_resume( $2 ) ); }
	| THROWRESUME assignment_expression_opt AT assignment_expression ';' // handles reresume
		{ $$ = new StatementNode( build_resume_at( $2, $4 ) ); }
	;

fall_through_name:										// CFA
	FALLTHRU
	| FALLTHROUGH
	;

with_statement:
	WITH '(' tuple_expression_list ')' statement
		{ $$ = new StatementNode( build_with( $3, $5 ) ); }
	;

// If MUTEX becomes a general qualifier, there are shift/reduce conflicts, so change syntax to "with mutex".
mutex_statement:
	MUTEX '(' argument_expression_list ')' statement
		{ $$ = new StatementNode( build_mutex( $3, $5 ) ); }
	;

when_clause:
	WHEN '(' comma_expression ')'				{ $$ = $3; }
	;

when_clause_opt:
	// empty
		{ $$ = nullptr; }
	| when_clause
	;

waitfor:
	WAITFOR '(' cast_expression ')'
		{ $$ = $3; }
//	| WAITFOR '(' cast_expression ',' argument_expression_list_opt ')'
//	  	{ $$ = (ExpressionNode *)$3->set_last( $5 ); }
	| WAITFOR '(' cast_expression_list ':' argument_expression_list_opt ')'
		{ $$ = (ExpressionNode *)($3->set_last( $5 )); }
	;

cast_expression_list:
	cast_expression
	| cast_expression_list ',' cast_expression
		// { $$ = (ExpressionNode *)($1->set_last( $3 )); }
		{ SemanticError( yylloc, "List of mutex member is currently unimplemented." ); $$ = nullptr; }
	;

timeout:
	TIMEOUT '(' comma_expression ')'	 		{ $$ = $3; }
	;

waitfor_clause:
	when_clause_opt waitfor statement					%prec THEN
		{ $$ = build_waitfor( $2, maybe_build_compound( $3 ), $1 ); }
	| when_clause_opt waitfor statement WOR waitfor_clause
		{ $$ = build_waitfor( $2, maybe_build_compound( $3 ), $1, $5 ); }
	| when_clause_opt timeout statement					%prec THEN
		{ $$ = build_waitfor_timeout( $2, maybe_build_compound( $3 ), $1 ); }
	| when_clause_opt ELSE statement
		{ $$ = build_waitfor_timeout( nullptr, maybe_build_compound( $3 ), $1 ); }
		// "else" must be conditional after timeout or timeout is never triggered (i.e., it is meaningless)
	| when_clause_opt timeout statement WOR ELSE statement
		{ SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); $$ = nullptr; }
	| when_clause_opt timeout statement WOR when_clause ELSE statement
		{ $$ = build_waitfor_timeout( $2, maybe_build_compound( $3 ), $1, maybe_build_compound( $7 ), $5 ); }
	;

waitfor_statement:
	when_clause_opt waitfor statement					%prec THEN
 		{ $$ = new StatementNode( build_waitfor( $2, $3, $1 ) ); }
	| when_clause_opt waitfor statement WOR waitfor_clause
 		{ $$ = new StatementNode( build_waitfor( $2, $3, $1, $5 ) ); }
	;

exception_statement:
	TRY compound_statement handler_clause 					%prec THEN
		{ $$ = new StatementNode( build_try( $2, $3, 0 ) ); }
	| TRY compound_statement finally_clause
		{ $$ = new StatementNode( build_try( $2, 0, $3 ) ); }
	| TRY compound_statement handler_clause finally_clause
		{ $$ = new StatementNode( build_try( $2, $3, $4 ) ); }
	;

handler_clause:
	handler_key '(' push exception_declaration pop handler_predicate_opt ')' compound_statement
		{ $$ = new StatementNode( build_catch( $1, $4, $6, $8 ) ); }
	| handler_clause handler_key '(' push exception_declaration pop handler_predicate_opt ')' compound_statement
		{ $$ = (StatementNode *)$1->set_last( new StatementNode( build_catch( $2, $5, $7, $9 ) ) ); }
	;

handler_predicate_opt:
	// empty
		{ $$ = nullptr; }
	| ';' conditional_expression				{ $$ = $2; }
	;

handler_key:
	CATCH										{ $$ = CatchStmt::Terminate; }
	| RECOVER									{ $$ = CatchStmt::Terminate; }
	| CATCHRESUME								{ $$ = CatchStmt::Resume; }
	| FIXUP										{ $$ = CatchStmt::Resume; }
	;

finally_clause:
	FINALLY compound_statement					{ $$ = new StatementNode( build_finally( $2 ) ); }
	;

exception_declaration:
		// No SUE declaration in parameter list.
	type_specifier_nobody
	| type_specifier_nobody declarator
		{ $$ = $2->addType( $1 ); }
	| type_specifier_nobody variable_abstract_declarator
		{ $$ = $2->addType( $1 ); }
	| cfa_abstract_declarator_tuple identifier			// CFA
		{ $$ = $1->addName( $2 ); }
	| cfa_abstract_declarator_tuple						// CFA
	;

enable_disable_statement:
	enable_disable_key identifier_list compound_statement
	;

enable_disable_key:
	ENABLE
	| DISABLE
	;

asm_statement:
	ASM asm_volatile_opt '(' string_literal ')' ';'
		{ $$ = new StatementNode( build_asm( $2, $4, 0 ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ')' ';' // remaining GCC
		{ $$ = new StatementNode( build_asm( $2, $4, $6 ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ')' ';'
		{ $$ = new StatementNode( build_asm( $2, $4, $6, $8 ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers_list_opt ')' ';'
		{ $$ = new StatementNode( build_asm( $2, $4, $6, $8, $10 ) ); }
	| ASM asm_volatile_opt GOTO '(' string_literal ':' ':' asm_operands_opt ':' asm_clobbers_list_opt ':' label_list ')' ';'
		{ $$ = new StatementNode( build_asm( $2, $5, 0, $8, $10, $12 ) ); }
	;

asm_volatile_opt:										// GCC
	// empty
		{ $$ = false; }
	| VOLATILE
		{ $$ = true; }
	;

asm_operands_opt:										// GCC
	// empty
		{ $$ = nullptr; }								// use default argument
	| asm_operands_list
	;

asm_operands_list:										// GCC
	asm_operand
	| asm_operands_list ',' asm_operand
		{ $$ = (ExpressionNode *)($1->set_last( $3 )); }
	;

asm_operand:											// GCC
	string_literal '(' constant_expression ')'
		{ $$ = new ExpressionNode( new AsmExpr( nullptr, $1, maybeMoveBuild<Expression>( $3 ) ) ); }
	| '[' IDENTIFIER ']' string_literal '(' constant_expression ')'
		{ $$ = new ExpressionNode( new AsmExpr( $2, $4, maybeMoveBuild<Expression>( $6 ) ) ); }
	;

asm_clobbers_list_opt:									// GCC
	// empty
		{ $$ = nullptr; }								// use default argument
	| string_literal
		{ $$ = new ExpressionNode( $1 ); }
	| asm_clobbers_list_opt ',' string_literal
		{ $$ = (ExpressionNode *)($1->set_last( new ExpressionNode( $3 ) )); }
	;

label_list:
	identifier
		{
			$$ = new LabelNode(); $$->labels.push_back( *$1 );
			delete $1;									// allocated by lexer
		}
	| label_list ',' identifier
		{
			$$ = $1; $1->labels.push_back( *$3 );
			delete $3;									// allocated by lexer
		}
	;

//******************************* DECLARATIONS *********************************

declaration_list_opt:									// used at beginning of switch statement
	// empty
		{ $$ = nullptr; }
	| declaration_list
	;

declaration_list:
	declaration
	| declaration_list declaration
		{ $$ = $1->appendList( $2 ); }
	;

KR_parameter_list_opt:									// used to declare parameter types in K&R style functions
	// empty
		{ $$ = nullptr; }
	| KR_parameter_list
	;

KR_parameter_list:
	push c_declaration pop ';'
		{ $$ = $2; }
	| KR_parameter_list push c_declaration pop ';'
		{ $$ = $1->appendList( $3 ); }
	;

local_label_declaration_opt:							// GCC, local label
	// empty
	| local_label_declaration_list
	;

local_label_declaration_list:							// GCC, local label
	LABEL local_label_list ';'
	| local_label_declaration_list LABEL local_label_list ';'
	;

local_label_list:										// GCC, local label
	identifier_or_type_name
	| local_label_list ',' identifier_or_type_name
	;

declaration:											// old & new style declarations
	c_declaration ';'
	| cfa_declaration ';'								// CFA
	| static_assert										// C11
	;

static_assert:
	STATICASSERT '(' constant_expression ',' string_literal ')' ';' // C11
		{ $$ = DeclarationNode::newStaticAssert( $3, $5 ); }
	| STATICASSERT '(' constant_expression ')' ';'		// CFA
		{ $$ = DeclarationNode::newStaticAssert( $3, build_constantStr( *new string( "\"\"" ) ) ); }

// C declaration syntax is notoriously confusing and error prone. Cforall provides its own type, variable and function
// declarations. CFA declarations use the same declaration tokens as in C; however, CFA places declaration modifiers to
// the left of the base type, while C declarations place modifiers to the right of the base type. CFA declaration
// modifiers are interpreted from left to right and the entire type specification is distributed across all variables in
// the declaration list (as in Pascal).  ANSI C and the new CFA declarations may appear together in the same program
// block, but cannot be mixed within a specific declaration.
//
//			CFA					C
//		[10] int x;			int x[10];		// array of 10 integers
//		[10] * char y;		char *y[10];	// array of 10 pointers to char

cfa_declaration:										// CFA
	cfa_variable_declaration
	| cfa_typedef_declaration
	| cfa_function_declaration
	| type_declaring_list
		{ SemanticError( yylloc, "otype declaration is currently unimplemented." ); $$ = nullptr; }
	| trait_specifier
	;

cfa_variable_declaration:								// CFA
	cfa_variable_specifier initializer_opt
		{ $$ = $1->addInitializer( $2 ); }
	| declaration_qualifier_list cfa_variable_specifier initializer_opt
		// declaration_qualifier_list also includes type_qualifier_list, so a semantic check is necessary to preclude
		// them as a type_qualifier cannot appear in that context.
		{ $$ = $2->addQualifiers( $1 )->addInitializer( $3 ); }
	| cfa_variable_declaration pop ',' push identifier_or_type_name initializer_opt
		{ $$ = $1->appendList( $1->cloneType( $5 )->addInitializer( $6 ) ); }
	;

cfa_variable_specifier:									// CFA
		// A semantic check is required to ensure asm_name only appears on declarations with implicit or explicit static
		// storage-class
	cfa_abstract_declarator_no_tuple identifier_or_type_name asm_name_opt
		{ $$ = $1->addName( $2 )->addAsmName( $3 ); }
	| cfa_abstract_tuple identifier_or_type_name asm_name_opt
		{ $$ = $1->addName( $2 )->addAsmName( $3 ); }
	| type_qualifier_list cfa_abstract_tuple identifier_or_type_name asm_name_opt
		{ $$ = $2->addQualifiers( $1 )->addName( $3 )->addAsmName( $4 ); }
	;

cfa_function_declaration:								// CFA
	cfa_function_specifier
	| type_qualifier_list cfa_function_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| declaration_qualifier_list cfa_function_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| declaration_qualifier_list type_qualifier_list cfa_function_specifier
		{ $$ = $3->addQualifiers( $1 )->addQualifiers( $2 ); }
	| cfa_function_declaration ',' identifier_or_type_name '(' push cfa_parameter_ellipsis_list_opt pop ')'
		{
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( $1->type->base );
			$$ = $1->appendList( DeclarationNode::newFunction( $3, ret, $6, nullptr ) );
		}
	;

cfa_function_specifier:									// CFA
//	'[' ']' identifier_or_type_name '(' push cfa_parameter_ellipsis_list_opt pop ')' // S/R conflict
//		{
//			$$ = DeclarationNode::newFunction( $3, DeclarationNode::newTuple( 0 ), $6, 0, true );
//		}
//	'[' ']' identifier '(' push cfa_parameter_ellipsis_list_opt pop ')'
//		{
//			typedefTable.setNextIdentifier( *$5 );
//			$$ = DeclarationNode::newFunction( $5, DeclarationNode::newTuple( 0 ), $8, 0, true );
//		}
//	| '[' ']' TYPEDEFname '(' push cfa_parameter_ellipsis_list_opt pop ')'
//		{
//			typedefTable.setNextIdentifier( *$5 );
//			$$ = DeclarationNode::newFunction( $5, DeclarationNode::newTuple( 0 ), $8, 0, true );
//		}
//	| '[' ']' typegen_name
		// identifier_or_type_name must be broken apart because of the sequence:
		//
		//   '[' ']' identifier_or_type_name '(' cfa_parameter_ellipsis_list_opt ')'
		//   '[' ']' type_specifier
		//
		// type_specifier can resolve to just TYPEDEFname (e.g., typedef int T; int f( T );). Therefore this must be
		// flattened to allow lookahead to the '(' without having to reduce identifier_or_type_name.
	cfa_abstract_tuple identifier_or_type_name '(' push cfa_parameter_ellipsis_list_opt pop ')' attribute_list_opt
		// To obtain LR(1 ), this rule must be factored out from function return type (see cfa_abstract_declarator).
		{ $$ = DeclarationNode::newFunction( $2, $1, $5, 0 )->addQualifiers( $8 ); }
	| cfa_function_return identifier_or_type_name '(' push cfa_parameter_ellipsis_list_opt pop ')' attribute_list_opt
		{ $$ = DeclarationNode::newFunction( $2, $1, $5, 0 )->addQualifiers( $8 ); }
	;

cfa_function_return:									// CFA
	'[' push cfa_parameter_list pop ']'
		{ $$ = DeclarationNode::newTuple( $3 ); }
	| '[' push cfa_parameter_list pop ',' push cfa_abstract_parameter_list pop ']'
		// To obtain LR(1 ), the last cfa_abstract_parameter_list is added into this flattened rule to lookahead to the ']'.
		{ $$ = DeclarationNode::newTuple( $3->appendList( $7 ) ); }
	;

cfa_typedef_declaration:								// CFA
	TYPEDEF cfa_variable_specifier
		{
			typedefTable.addToEnclosingScope( *$2->name, TYPEDEFname, "1" );
			$$ = $2->addTypedef();
		}
	| TYPEDEF cfa_function_specifier
		{
			typedefTable.addToEnclosingScope( *$2->name, TYPEDEFname, "2" );
			$$ = $2->addTypedef();
		}
	| cfa_typedef_declaration pop ',' push identifier
		{
			typedefTable.addToEnclosingScope( *$5, TYPEDEFname, "3" );
			$$ = $1->appendList( $1->cloneType( $5 ) );
		}
	;

// Traditionally typedef is part of storage-class specifier for syntactic convenience only. Here, it is factored out as
// a separate form of declaration, which syntactically precludes storage-class specifiers and initialization.

typedef_declaration:
	TYPEDEF type_specifier declarator
		{
			typedefTable.addToEnclosingScope( *$3->name, TYPEDEFname, "4" );
			$$ = $3->addType( $2 )->addTypedef();
		}
	| typedef_declaration pop ',' push declarator
		{
			typedefTable.addToEnclosingScope( *$5->name, TYPEDEFname, "5" );
			$$ = $1->appendList( $1->cloneBaseType( $5 )->addTypedef() );
		}
	| type_qualifier_list TYPEDEF type_specifier declarator // remaining OBSOLESCENT (see 2 )
		{
			typedefTable.addToEnclosingScope( *$4->name, TYPEDEFname, "6" );
			$$ = $4->addType( $3 )->addQualifiers( $1 )->addTypedef();
		}
	| type_specifier TYPEDEF declarator
		{
			typedefTable.addToEnclosingScope( *$3->name, TYPEDEFname, "7" );
			$$ = $3->addType( $1 )->addTypedef();
		}
	| type_specifier TYPEDEF type_qualifier_list declarator
		{
			typedefTable.addToEnclosingScope( *$4->name, TYPEDEFname, "8" );
			$$ = $4->addQualifiers( $1 )->addTypedef()->addType( $1 );
		}
	;

typedef_expression:
		// deprecated GCC, naming expression type: typedef name = exp; gives a name to the type of an expression
	TYPEDEF identifier '=' assignment_expression
		{
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); $$ = nullptr;
		}
	| typedef_expression pop ',' push identifier '=' assignment_expression
		{
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); $$ = nullptr;
		}
	;

c_declaration:
	declaration_specifier declaring_list
		{ $$ = distAttr( $1, $2 ); }
	| typedef_declaration
	| typedef_expression								// deprecated GCC, naming expression type
	| sue_declaration_specifier
	;

declaring_list:
		// A semantic check is required to ensure asm_name only appears on declarations with implicit or explicit static
		// storage-class
	declarator asm_name_opt initializer_opt
		{ $$ = $1->addAsmName( $2 )->addInitializer( $3 ); }
	| declaring_list ',' attribute_list_opt declarator asm_name_opt initializer_opt
		{ $$ = $1->appendList( $4->addQualifiers( $3 )->addAsmName( $5 )->addInitializer( $6 ) ); }
	;

declaration_specifier:									// type specifier + storage class
	basic_declaration_specifier
	| sue_declaration_specifier
	| type_declaration_specifier
	;

declaration_specifier_nobody:							// type specifier + storage class - {...}
		// Preclude SUE declarations in restricted scopes:
		//
		//    int f( struct S { int i; } s1, Struct S s2 ) { struct S s3; ... }
		//
		// because it is impossible to call f due to name equivalence.
	basic_declaration_specifier
	| sue_declaration_specifier_nobody
	| type_declaration_specifier
	;

type_specifier:											// type specifier
	basic_type_specifier
	| sue_type_specifier
	| type_type_specifier
	;

type_specifier_nobody:									// type specifier - {...}
		// Preclude SUE declarations in restricted scopes:
		//
		//    int f( struct S { int i; } s1, Struct S s2 ) { struct S s3; ... }
		//
		// because it is impossible to call f due to name equivalence.
	basic_type_specifier
	| sue_type_specifier_nobody
	| type_type_specifier
	;

type_qualifier_list_opt:								// GCC, used in asm_statement
	// empty
		{ $$ = nullptr; }
	| type_qualifier_list
	;

type_qualifier_list:
		// A semantic check is necessary to ensure a type qualifier is appropriate for the kind of declaration.
		//
		// ISO/IEC 9899:1999 Section 6.7.3(4 ) : If the same qualifier appears more than once in the same
		// specifier-qualifier-list, either directly or via one or more typedefs, the behavior is the same as if it
		// appeared only once.
	type_qualifier
	| type_qualifier_list type_qualifier
		{ $$ = $1->addQualifiers( $2 ); }
	;

type_qualifier:
	type_qualifier_name
	| attribute											// trick handles most atrribute locations
	;

type_qualifier_name:
	CONST
		{ $$ = DeclarationNode::newTypeQualifier( Type::Const ); }
	| RESTRICT
		{ $$ = DeclarationNode::newTypeQualifier( Type::Restrict ); }
	| VOLATILE
		{ $$ = DeclarationNode::newTypeQualifier( Type::Volatile ); }
	| ATOMIC
		{ $$ = DeclarationNode::newTypeQualifier( Type::Atomic ); }
	| forall
	;

forall:
	FORALL '(' type_parameter_list ')'					// CFA
		{ $$ = DeclarationNode::newForall( $3 ); }
	;

declaration_qualifier_list:
	storage_class_list
	| type_qualifier_list storage_class_list			// remaining OBSOLESCENT (see 2 )
		{ $$ = $1->addQualifiers( $2 ); }
	| declaration_qualifier_list type_qualifier_list storage_class_list
		{ $$ = $1->addQualifiers( $2 )->addQualifiers( $3 ); }
	;

storage_class_list:
		// A semantic check is necessary to ensure a storage class is appropriate for the kind of declaration and that
		// only one of each is specified, except for inline, which can appear with the others.
		//
		// ISO/IEC 9899:1999 Section 6.7.1(2) : At most, one storage-class specifier may be given in the declaration
		// specifiers in a declaration.
	storage_class
	| storage_class_list storage_class
		{ $$ = $1->addQualifiers( $2 ); }
	;

storage_class:
	EXTERN
		{ $$ = DeclarationNode::newStorageClass( Type::Extern ); }
	| STATIC
		{ $$ = DeclarationNode::newStorageClass( Type::Static ); }
	| AUTO
		{ $$ = DeclarationNode::newStorageClass( Type::Auto ); }
	| REGISTER
		{ $$ = DeclarationNode::newStorageClass( Type::Register ); }
	| THREADLOCAL										// C11
		{ $$ = DeclarationNode::newStorageClass( Type::Threadlocal ); }
		// Put function specifiers here to simplify parsing rules, but separate them semantically.
	| INLINE											// C99
		{ $$ = DeclarationNode::newFuncSpecifier( Type::Inline ); }
	| FORTRAN											// C99
		{ $$ = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
	| NORETURN											// C11
		{ $$ = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
	;

basic_type_name:
	VOID
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Void ); }
	| BOOL												// C99
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
	| CHAR
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Char ); }
	| INT
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Int ); }
	| INT128
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
	| UINT128
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
	| FLOAT
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Float ); }
	| DOUBLE
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Double ); }
	| uuFLOAT80
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
	| uuFLOAT128
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
	| uFLOAT16
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
	| uFLOAT32
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
	| uFLOAT32X
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
	| uFLOAT64
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
	| uFLOAT64X
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
	| uFLOAT128
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
	| DECIMAL32
		{ SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); $$ = nullptr; }
	| DECIMAL64
		{ SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); $$ = nullptr; }
	| DECIMAL128
		{ SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); $$ = nullptr; }
	| COMPLEX											// C99
		{ $$ = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
	| IMAGINARY											// C99
		{ $$ = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
	| SIGNED
		{ $$ = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
	| UNSIGNED
		{ $$ = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
	| SHORT
		{ $$ = DeclarationNode::newLength( DeclarationNode::Short ); }
	| LONG
		{ $$ = DeclarationNode::newLength( DeclarationNode::Long ); }
	| VALIST											// GCC, __builtin_va_list
		{ $$ = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
	| AUTO_TYPE
		{ $$ = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
	| vtable
	;

vtable_opt:
	// empty
		{ $$ = nullptr; }
	| vtable
	;

vtable:
	VTABLE '(' type_name ')' default_opt
		{ $$ = DeclarationNode::newVtableType( $3 ); }
		// { SemanticError( yylloc, "vtable is currently unimplemented." ); $$ = nullptr; }
	;

default_opt:
	// empty
		{ $$ = nullptr; }
	| DEFAULT
		{ SemanticError( yylloc, "vtable default is currently unimplemented." ); $$ = nullptr; }
	;

basic_declaration_specifier:
		// A semantic check is necessary for conflicting storage classes.
	basic_type_specifier
	| declaration_qualifier_list basic_type_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| basic_declaration_specifier storage_class			// remaining OBSOLESCENT (see 2)
		{ $$ = $1->addQualifiers( $2 ); }
	| basic_declaration_specifier storage_class type_qualifier_list
		{ $$ = $1->addQualifiers( $2 )->addQualifiers( $3 ); }
	| basic_declaration_specifier storage_class basic_type_specifier
		{ $$ = $3->addQualifiers( $2 )->addType( $1 ); }
	;

basic_type_specifier:
	direct_type
		// Cannot have type modifiers, e.g., short, long, etc.
	| type_qualifier_list_opt indirect_type type_qualifier_list_opt
		{ $$ = $2->addQualifiers( $1 )->addQualifiers( $3 ); }
	;

direct_type:
	basic_type_name
	| type_qualifier_list basic_type_name
		{ $$ = $2->addQualifiers( $1 ); }
	| direct_type type_qualifier
		{ $$ = $1->addQualifiers( $2 ); }
	| direct_type basic_type_name
		{ $$ = $1->addType( $2 ); }
	;

indirect_type:
	TYPEOF '(' type ')'									// GCC: typeof( x ) y;
		{ $$ = $3; }
	| TYPEOF '(' comma_expression ')'					// GCC: typeof( a+b ) y;
		{ $$ = DeclarationNode::newTypeof( $3 ); }
	| BASETYPEOF '(' type ')'							// CFA: basetypeof( x ) y;
		{ $$ = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( $3 ) ) ), true ); }
	| BASETYPEOF '(' comma_expression ')'				// CFA: basetypeof( a+b ) y;
		{ $$ = DeclarationNode::newTypeof( $3, true ); }
	| ZERO_T											// CFA
		{ $$ = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
	| ONE_T												// CFA
		{ $$ = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
	;

sue_declaration_specifier:								// struct, union, enum + storage class + type specifier
	sue_type_specifier
	| declaration_qualifier_list sue_type_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| sue_declaration_specifier storage_class			// remaining OBSOLESCENT (see 2)
		{ $$ = $1->addQualifiers( $2 ); }
	| sue_declaration_specifier storage_class type_qualifier_list
		{ $$ = $1->addQualifiers( $2 )->addQualifiers( $3 ); }
	;

sue_type_specifier:										// struct, union, enum + type specifier
	elaborated_type
	| type_qualifier_list
		{ if ( $1->type != nullptr && $1->type->forall ) forall = true; } // remember generic type
	  elaborated_type
		{ $$ = $3->addQualifiers( $1 ); }
	| sue_type_specifier type_qualifier
		{
			if ( $2->type != nullptr && $2->type->forall ) forall = true; // remember generic type
			$$ = $1->addQualifiers( $2 );
		}
	;

sue_declaration_specifier_nobody:						// struct, union, enum - {...} + storage class + type specifier
	sue_type_specifier_nobody
	| declaration_qualifier_list sue_type_specifier_nobody
		{ $$ = $2->addQualifiers( $1 ); }
	| sue_declaration_specifier_nobody storage_class	// remaining OBSOLESCENT (see 2)
		{ $$ = $1->addQualifiers( $2 ); }
	| sue_declaration_specifier_nobody storage_class type_qualifier_list
		{ $$ = $1->addQualifiers( $2 )->addQualifiers( $3 ); }
	;

sue_type_specifier_nobody:								// struct, union, enum - {...} + type specifier
	elaborated_type_nobody
	| type_qualifier_list elaborated_type_nobody
		{ $$ = $2->addQualifiers( $1 ); }
	| sue_type_specifier_nobody type_qualifier
		{ $$ = $1->addQualifiers( $2 ); }
	;

type_declaration_specifier:
	type_type_specifier
	| declaration_qualifier_list type_type_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| type_declaration_specifier storage_class			// remaining OBSOLESCENT (see 2)
		{ $$ = $1->addQualifiers( $2 ); }
	| type_declaration_specifier storage_class type_qualifier_list
		{ $$ = $1->addQualifiers( $2 )->addQualifiers( $3 ); }
	;

type_type_specifier:									// typedef types
	type_name
	| type_qualifier_list type_name
		{ $$ = $2->addQualifiers( $1 ); }
	| type_type_specifier type_qualifier
		{ $$ = $1->addQualifiers( $2 ); }
	;

type_name:
	TYPEDEFname
		{ $$ = DeclarationNode::newFromTypedef( $1 ); }
	| '.' TYPEDEFname
		{ $$ = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( $2 ) ); }
	| type_name '.' TYPEDEFname
		{ $$ = DeclarationNode::newQualifiedType( $1, DeclarationNode::newFromTypedef( $3 ) ); }
	| typegen_name
	| '.' typegen_name
		{ $$ = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), $2 ); }
	| type_name '.' typegen_name
		{ $$ = DeclarationNode::newQualifiedType( $1, $3 ); }
	;

typegen_name:											// CFA
	TYPEGENname
		{ $$ = DeclarationNode::newFromTypeGen( $1, nullptr ); }
	| TYPEGENname '(' ')'
		{ $$ = DeclarationNode::newFromTypeGen( $1, nullptr ); }
	| TYPEGENname '(' type_list ')'
		{ $$ = DeclarationNode::newFromTypeGen( $1, $3 ); }
	;

elaborated_type:										// struct, union, enum
	aggregate_type
	| enum_type
	;

elaborated_type_nobody:									// struct, union, enum - {...}
	aggregate_type_nobody
	| enum_type_nobody
	;

aggregate_type:											// struct, union
	aggregate_key attribute_list_opt
		{ forall = false; }								// reset
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{ $$ = DeclarationNode::newAggregate( $1, nullptr, $7, $5, true )->addQualifiers( $2 ); }
	| aggregate_key attribute_list_opt identifier
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{ $$ = DeclarationNode::newAggregate( $1, $3, $8, $6, true )->addQualifiers( $2 ); }
	| aggregate_key attribute_list_opt TYPEDEFname		// unqualified type name
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{
			DeclarationNode::newFromTypedef( $3 );
			$$ = DeclarationNode::newAggregate( $1, $3, $8, $6, true )->addQualifiers( $2 );
		}
	| aggregate_key attribute_list_opt TYPEGENname		// unqualified type name
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{
			DeclarationNode::newFromTypeGen( $3, nullptr );
			$$ = DeclarationNode::newAggregate( $1, $3, $8, $6, true )->addQualifiers( $2 );
		}
	| aggregate_type_nobody
	;

type_parameters_opt:
	// empty
		{ $$ = nullptr; }								%prec '}'
	| '(' type_list ')'
		{ $$ = $2; }
	;

aggregate_type_nobody:									// struct, union - {...}
	aggregate_key attribute_list_opt identifier
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			$$ = DeclarationNode::newAggregate( $1, $3, nullptr, nullptr, false )->addQualifiers( $2 );
		}
	| aggregate_key attribute_list_opt type_name
		{
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			$$ = DeclarationNode::newAggregate( $1, $3->type->symbolic.name, $3->type->symbolic.actuals, nullptr, false )->addQualifiers( $2 );
			$3->type->symbolic.name = nullptr;
			$3->type->symbolic.actuals = nullptr;
			delete $3;
		}
	;

aggregate_key:
	aggregate_data
	| aggregate_control
	;

aggregate_data:
	STRUCT vtable_opt
		{ $$ = AggregateDecl::Struct; }
	| UNION
		{ $$ = AggregateDecl::Union; }
	| EXCEPTION											// CFA
		{ $$ = AggregateDecl::Exception; }
	  //		{ SemanticError( yylloc, "exception aggregate is currently unimplemented." ); $$ = AggregateDecl::NoAggregate; }
	;

aggregate_control:										// CFA
	MONITOR
		{ $$ = AggregateDecl::Monitor; }
	| MUTEX STRUCT
		{ $$ = AggregateDecl::Monitor; }
	| GENERATOR
		{ $$ = AggregateDecl::Generator; }
	| MUTEX GENERATOR
		{ SemanticError( yylloc, "monitor generator is currently unimplemented." ); $$ = AggregateDecl::NoAggregate; }
	| COROUTINE
		{ $$ = AggregateDecl::Coroutine; }
	| MUTEX COROUTINE
		{ SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); $$ = AggregateDecl::NoAggregate; }
	| THREAD
		{ $$ = AggregateDecl::Thread; }
	| MUTEX THREAD
		{ SemanticError( yylloc, "monitor thread is currently unimplemented." ); $$ = AggregateDecl::NoAggregate; }
	;

field_declaration_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_declaration_list_opt field_declaration
		{ $$ = $1 ? $1->appendList( $2 ) : $2; }
	;

field_declaration:
	type_specifier field_declaring_list_opt ';'
		{ $$ = fieldDecl( $1, $2 ); }
	| EXTENSION type_specifier field_declaring_list_opt ';'	// GCC
		{ $$ = fieldDecl( $2, $3 ); distExt( $$ ); }
	| INLINE type_specifier field_abstract_list_opt ';'	// CFA
		{
			if ( ! $3 ) {								// field declarator ?
				$3 = DeclarationNode::newName( nullptr );
			} // if
			$3->inLine = true;
			$$ = distAttr( $2, $3 );					// mark all fields in list
			distInl( $3 );
		}
	| INLINE aggregate_control ';'						// CFA
	   	{ SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); $$ = nullptr; }
	| typedef_declaration ';'							// CFA
	| cfa_field_declaring_list ';'						// CFA, new style field declaration
	| EXTENSION cfa_field_declaring_list ';'			// GCC
		{ distExt( $2 ); $$ = $2; }						// mark all fields in list
	| INLINE cfa_field_abstract_list ';'				// CFA, new style field declaration
		{ $$ = $2; }									// mark all fields in list
	| cfa_typedef_declaration ';'						// CFA
	| static_assert										// C11
	;

field_declaring_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_declarator
	| field_declaring_list_opt ',' attribute_list_opt field_declarator
		{ $$ = $1->appendList( $4->addQualifiers( $3 ) ); }
	;

field_declarator:
	bit_subrange_size									// C special case, no field name
		{ $$ = DeclarationNode::newBitfield( $1 ); }
	| variable_declarator bit_subrange_size_opt
		// A semantic check is required to ensure bit_subrange only appears on integral types.
		{ $$ = $1->addBitfield( $2 ); }
	| variable_type_redeclarator bit_subrange_size_opt
		// A semantic check is required to ensure bit_subrange only appears on integral types.
		{ $$ = $1->addBitfield( $2 ); }
	;

field_abstract_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_abstract
	| field_abstract_list_opt ',' attribute_list_opt field_abstract
		{ $$ = $1->appendList( $4->addQualifiers( $3 ) ); }
	;

field_abstract:
		// 	no bit fields
	variable_abstract_declarator
	;

cfa_field_declaring_list:								// CFA, new style field declaration
	// bit-fields are handled by C declarations
	cfa_abstract_declarator_tuple identifier_or_type_name
		{ $$ = $1->addName( $2 ); }
	| cfa_field_declaring_list ',' identifier_or_type_name
		{ $$ = $1->appendList( $1->cloneType( $3 ) ); }
	;

cfa_field_abstract_list:								// CFA, new style field declaration
	// bit-fields are handled by C declarations
	cfa_abstract_declarator_tuple
	| cfa_field_abstract_list ','
		{ $$ = $1->appendList( $1->cloneType( 0 ) ); }
	;

bit_subrange_size_opt:
	// empty
		{ $$ = nullptr; }
	| bit_subrange_size
	;

bit_subrange_size:
	':' assignment_expression
		{ $$ = $2; }
	;

enum_type:												// enum
	ENUM attribute_list_opt '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( nullptr, $4, true )->addQualifiers( $2 ); }
	| ENUM attribute_list_opt identifier
		{ typedefTable.makeTypedef( *$3 ); }
	  '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( $3, $6, true )->addQualifiers( $2 ); }
	| ENUM attribute_list_opt typedef_name				// unqualified type name
	  '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( $3->name, $5, true )->addQualifiers( $2 ); }
	| ENUM '(' cfa_abstract_parameter_declaration ')' attribute_list_opt '{' enumerator_list comma_opt '}'
	 	{
			if ( $3->storageClasses.val != 0 || $3->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); $$ = nullptr;
		}
	| ENUM '(' cfa_abstract_parameter_declaration ')' attribute_list_opt identifier attribute_list_opt
		{
			if ( $3->storageClasses.val != 0 || $3->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *$6 );
		}
	  '{' enumerator_list comma_opt '}'
		{
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); $$ = nullptr;
		}
	| ENUM '(' cfa_abstract_parameter_declaration ')' attribute_list_opt typedef_name attribute_list_opt '{' enumerator_list comma_opt '}'
		{
			if ( $3->storageClasses.val != 0 || $3->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *$6->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); $$ = nullptr;
		}
	| enum_type_nobody
	;

enum_type_nobody:										// enum - {...}
	ENUM attribute_list_opt identifier
		{ typedefTable.makeTypedef( *$3 ); $$ = DeclarationNode::newEnum( $3, 0, false )->addQualifiers( $2 ); }
	| ENUM attribute_list_opt type_name					// qualified type name
		{ typedefTable.makeTypedef( *$3->type->symbolic.name );	$$ = DeclarationNode::newEnum( $3->type->symbolic.name, 0, false )->addQualifiers( $2 ); }
	;

enumerator_list:
	identifier_or_type_name enumerator_value_opt
		{ $$ = DeclarationNode::newEnumConstant( $1, $2 ); }
	| INLINE type_name
		{ $$ = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
	| enumerator_list ',' identifier_or_type_name enumerator_value_opt
		{ $$ = $1->appendList( DeclarationNode::newEnumConstant( $3, $4 ) ); }
	| enumerator_list ',' INLINE type_name enumerator_value_opt
		{ $$ = $1->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
	;

enumerator_value_opt:
	// empty
		{ $$ = nullptr; }
	// | '=' constant_expression
	// 	{ $$ = $2; }
	| simple_assignment_operator initializer
		{ $$ = $2->get_expression(); }					// FIX ME: enum only deals with constant_expression
	;

cfa_parameter_ellipsis_list_opt:						// CFA, abstract + real
	// empty
		{ $$ = DeclarationNode::newBasicType( DeclarationNode::Void ); }
	| ELLIPSIS
		{ $$ = nullptr; }
	| cfa_abstract_parameter_list
	| cfa_parameter_list
	| cfa_parameter_list pop ',' push cfa_abstract_parameter_list
		{ $$ = $1->appendList( $5 ); }
	| cfa_abstract_parameter_list pop ',' push ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	| cfa_parameter_list pop ',' push ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	;

cfa_parameter_list:										// CFA
		// To obtain LR(1) between cfa_parameter_list and cfa_abstract_tuple, the last cfa_abstract_parameter_list is
		// factored out from cfa_parameter_list, flattening the rules to get lookahead to the ']'.
	cfa_parameter_declaration
	| cfa_abstract_parameter_list pop ',' push cfa_parameter_declaration
		{ $$ = $1->appendList( $5 ); }
	| cfa_parameter_list pop ',' push cfa_parameter_declaration
		{ $$ = $1->appendList( $5 ); }
	| cfa_parameter_list pop ',' push cfa_abstract_parameter_list pop ',' push cfa_parameter_declaration
		{ $$ = $1->appendList( $5 )->appendList( $9 ); }
	;

cfa_abstract_parameter_list:							// CFA, new & old style abstract
	cfa_abstract_parameter_declaration
	| cfa_abstract_parameter_list pop ',' push cfa_abstract_parameter_declaration
		{ $$ = $1->appendList( $5 ); }
	;

parameter_type_list_opt:
	// empty
		{ $$ = nullptr; }
	| ELLIPSIS
		{ $$ = nullptr; }
	| parameter_list
	| parameter_list pop ',' push ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	;

parameter_list:											// abstract + real
	abstract_parameter_declaration
	| parameter_declaration
	| parameter_list pop ',' push abstract_parameter_declaration
		{ $$ = $1->appendList( $5 ); }
	| parameter_list pop ',' push parameter_declaration
		{ $$ = $1->appendList( $5 ); }
	;

// Provides optional identifier names (abstract_declarator/variable_declarator), no initialization, different semantics
// for typedef name by using type_parameter_redeclarator instead of typedef_redeclarator, and function prototypes.

cfa_parameter_declaration:								// CFA, new & old style parameter declaration
	parameter_declaration
	| cfa_identifier_parameter_declarator_no_tuple identifier_or_type_name default_initializer_opt
		{ $$ = $1->addName( $2 ); }
	| cfa_abstract_tuple identifier_or_type_name default_initializer_opt
		// To obtain LR(1), these rules must be duplicated here (see cfa_abstract_declarator).
		{ $$ = $1->addName( $2 ); }
	| type_qualifier_list cfa_abstract_tuple identifier_or_type_name default_initializer_opt
		{ $$ = $2->addName( $3 )->addQualifiers( $1 ); }
	| cfa_function_specifier
	;

cfa_abstract_parameter_declaration:						// CFA, new & old style parameter declaration
	abstract_parameter_declaration
	| cfa_identifier_parameter_declarator_no_tuple
	| cfa_abstract_tuple
		// To obtain LR(1), these rules must be duplicated here (see cfa_abstract_declarator).
	| type_qualifier_list cfa_abstract_tuple
		{ $$ = $2->addQualifiers( $1 ); }
	| cfa_abstract_function
	;

parameter_declaration:
		// No SUE declaration in parameter list.
	declaration_specifier_nobody identifier_parameter_declarator default_initializer_opt
		{ $$ = $2->addType( $1 )->addInitializer( $3 ? new InitializerNode( $3 ) : nullptr ); }
	| declaration_specifier_nobody type_parameter_redeclarator default_initializer_opt
		{ $$ = $2->addType( $1 )->addInitializer( $3 ? new InitializerNode( $3 ) : nullptr ); }
	;

abstract_parameter_declaration:
	declaration_specifier_nobody default_initializer_opt
		{ $$ = $1->addInitializer( $2 ? new InitializerNode( $2 ) : nullptr ); }
	| declaration_specifier_nobody abstract_parameter_declarator default_initializer_opt
		{ $$ = $2->addType( $1 )->addInitializer( $3 ? new InitializerNode( $3 ) : nullptr ); }
	;

// ISO/IEC 9899:1999 Section 6.9.1(6) : "An identifier declared as a typedef name shall not be redeclared as a
// parameter." Because the scope of the K&R-style parameter-list sees the typedef first, the following is based only on
// identifiers.  The ANSI-style parameter-list can redefine a typedef name.

identifier_list:										// K&R-style parameter list => no types
	identifier
		{ $$ = DeclarationNode::newName( $1 ); }
	| identifier_list ',' identifier
		{ $$ = $1->appendList( DeclarationNode::newName( $3 ) ); }
	;

identifier_or_type_name:
	identifier
	| TYPEDEFname
	| TYPEGENname
	;

type_no_function:										// sizeof, alignof, cast (constructor)
	cfa_abstract_declarator_tuple						// CFA
	| type_specifier
	| type_specifier abstract_declarator
		{ $$ = $2->addType( $1 ); }
	;

type:													// typeof, assertion
	type_no_function
	| cfa_abstract_function								// CFA
	;

initializer_opt:
	// empty
		{ $$ = nullptr; }
	| simple_assignment_operator initializer	{ $$ = $1 == OperKinds::Assign ? $2 : $2->set_maybeConstructed( false ); }
	| '=' VOID									{ $$ = new InitializerNode( true ); }
	| '{' initializer_list_opt comma_opt '}'	{ $$ = new InitializerNode( $2, true ); }
	;

initializer:
	assignment_expression						{ $$ = new InitializerNode( $1 ); }
	| '{' initializer_list_opt comma_opt '}'	{ $$ = new InitializerNode( $2, true ); }
	;

initializer_list_opt:
	// empty
		{ $$ = nullptr; }
	| initializer
	| designation initializer					{ $$ = $2->set_designators( $1 ); }
	| initializer_list_opt ',' initializer		{ $$ = (InitializerNode *)( $1->set_last( $3 ) ); }
	| initializer_list_opt ',' designation initializer { $$ = (InitializerNode *)($1->set_last( $4->set_designators( $3 ) )); }
	;

// There is an unreconcileable parsing problem between C99 and CFA with respect to designators. The problem is use of
// '=' to separator the designator from the initializer value, as in:
//
//		int x[10] = { [1] = 3 };
//
// The string "[1] = 3" can be parsed as a designator assignment or a tuple assignment.  To disambiguate this case, CFA
// changes the syntax from "=" to ":" as the separator between the designator and initializer. GCC does uses ":" for
// field selection. The optional use of the "=" in GCC, or in this case ":", cannot be supported either due to
// shift/reduce conflicts

designation:
	designator_list ':'									// C99, CFA uses ":" instead of "="
	| identifier_at ':'									// GCC, field name
		{ $$ = new ExpressionNode( build_varref( $1 ) ); }
	;

designator_list:										// C99
	designator
	| designator_list designator
		{ $$ = (ExpressionNode *)($1->set_last( $2 )); }
	//| designator_list designator						{ $$ = new ExpressionNode( $1, $2 ); }
	;

designator:
	'.' identifier_at									// C99, field name
		{ $$ = new ExpressionNode( build_varref( $2 ) ); }
	| '[' push assignment_expression pop ']'			// C99, single array element
		// assignment_expression used instead of constant_expression because of shift/reduce conflicts with tuple.
		{ $$ = $3; }
	| '[' push subrange pop ']'							// CFA, multiple array elements
		{ $$ = $3; }
	| '[' push constant_expression ELLIPSIS constant_expression pop ']' // GCC, multiple array elements
		{ $$ = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( $3 ), maybeMoveBuild<Expression>( $5 ) ) ); }
	| '.' '[' push field_name_list pop ']'				// CFA, tuple field selector
		{ $$ = $4; }
	;

// The CFA type system is based on parametric polymorphism, the ability to declare functions with type parameters,
// rather than an object-oriented type system. This required four groups of extensions:
//
// Overloading: function, data, and operator identifiers may be overloaded.
//
// Type declarations: "otype" is used to generate new types for declaring objects. Similarly, "dtype" is used for object
//     and incomplete types, and "ftype" is used for function types. Type declarations with initializers provide
//     definitions of new types. Type declarations with storage class "extern" provide opaque types.
//
// Polymorphic functions: A forall clause declares a type parameter. The corresponding argument is inferred at the call
//     site. A polymorphic function is not a template; it is a function, with an address and a type.
//
// Specifications and Assertions: Specifications are collections of declarations parameterized by one or more
//     types. They serve many of the purposes of abstract classes, and specification hierarchies resemble subclass
//     hierarchies. Unlike classes, they can define relationships between types.  Assertions declare that a type or
//     types provide the operations declared by a specification.  Assertions are normally used to declare requirements
//     on type arguments of polymorphic functions.

type_parameter_list:									// CFA
	type_parameter
	| type_parameter_list ',' type_parameter
		{ $$ = $1->appendList( $3 ); }
	;

type_initializer_opt:									// CFA
	// empty
		{ $$ = nullptr; }
	| '=' type
		{ $$ = $2; }
	;

type_parameter:											// CFA
	type_class identifier_or_type_name
		{
			typedefTable.addToScope( *$2, TYPEDEFname, "9" );
			if ( $1 == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( $1 == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( $1 == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
	  type_initializer_opt assertion_list_opt
		{ $$ = DeclarationNode::newTypeParam( $1, $2 )->addTypeInitializer( $4 )->addAssertions( $5 ); }
	| identifier_or_type_name new_type_class
		{ typedefTable.addToScope( *$1, TYPEDEFname, "9" ); }
	  type_initializer_opt assertion_list_opt
		{ $$ = DeclarationNode::newTypeParam( $2, $1 )->addTypeInitializer( $4 )->addAssertions( $5 ); }
	| '[' identifier_or_type_name ']'
		{
			typedefTable.addToScope( *$2, TYPEDIMname, "9" );
			$$ = DeclarationNode::newTypeParam( TypeDecl::Dimension, $2 );
		}
	// | type_specifier identifier_parameter_declarator
	| assertion_list
		{ $$ = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( $1 ); }
	;

new_type_class:											// CFA
	// empty
		{ $$ = TypeDecl::Otype; }
	| '&'
		{ $$ = TypeDecl::Dtype; }
	| '*'
		{ $$ = TypeDecl::DStype; }						// dtype + sized
	// | '(' '*' ')'
	// 	{ $$ = TypeDecl::Ftype; }
	| ELLIPSIS
		{ $$ = TypeDecl::Ttype; }
	;

type_class:												// CFA
	OTYPE
		{ $$ = TypeDecl::Otype; }
	| DTYPE
		{ $$ = TypeDecl::Dtype; }
	| FTYPE
		{ $$ = TypeDecl::Ftype; }
	| TTYPE
		{ $$ = TypeDecl::Ttype; }
	;

assertion_list_opt:										// CFA
	// empty
		{ $$ = nullptr; }
	| assertion_list
	;

assertion_list:											// CFA
	assertion
	| assertion_list assertion
		{ $$ = $1->appendList( $2 ); }
	;

assertion:												// CFA
	'|' identifier_or_type_name '(' type_list ')'
		{ $$ = DeclarationNode::newTraitUse( $2, $4 ); }
	| '|' '{' push trait_declaration_list pop '}'
		{ $$ = $4; }
	// | '|' '(' push type_parameter_list pop ')' '{' push trait_declaration_list pop '}' '(' type_list ')'
	// 	{ SemanticError( yylloc, "Generic data-type assertion is currently unimplemented." ); $$ = nullptr; }
	;

type_list:												// CFA
	type
		{ $$ = new ExpressionNode( new TypeExpr( maybeMoveBuildType( $1 ) ) ); }
	| assignment_expression
	| type_list ',' type
		{ $$ = (ExpressionNode *)($1->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( $3 ) ) ) )); }
	| type_list ',' assignment_expression
		{ $$ = (ExpressionNode *)( $1->set_last( $3 )); }
	;

type_declaring_list:									// CFA
	OTYPE type_declarator
		{ $$ = $2; }
	| storage_class_list OTYPE type_declarator
		{ $$ = $3->addQualifiers( $1 ); }
	| type_declaring_list ',' type_declarator
		{ $$ = $1->appendList( $3->copySpecifiers( $1 ) ); }
	;

type_declarator:										// CFA
	type_declarator_name assertion_list_opt
		{ $$ = $1->addAssertions( $2 ); }
	| type_declarator_name assertion_list_opt '=' type
		{ $$ = $1->addAssertions( $2 )->addType( $4 ); }
	;

type_declarator_name:									// CFA
	identifier_or_type_name
		{
			typedefTable.addToEnclosingScope( *$1, TYPEDEFname, "10" );
			$$ = DeclarationNode::newTypeDecl( $1, 0 );
		}
	| identifier_or_type_name '(' type_parameter_list ')'
		{
			typedefTable.addToEnclosingScope( *$1, TYPEGENname, "11" );
			$$ = DeclarationNode::newTypeDecl( $1, $3 );
		}
	;

trait_specifier:										// CFA
	TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' '}'
		{ $$ = DeclarationNode::newTrait( $2, $4, 0 ); }
	| TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' push trait_declaration_list pop '}'
		{ $$ = DeclarationNode::newTrait( $2, $4, $8 ); }
	;

trait_declaration_list:									// CFA
	trait_declaration
	| trait_declaration_list pop push trait_declaration
		{ $$ = $1->appendList( $4 ); }
	;

trait_declaration:										// CFA
	cfa_trait_declaring_list ';'
	| trait_declaring_list ';'
	;

cfa_trait_declaring_list:								// CFA
	cfa_variable_specifier
	| cfa_function_specifier
	| cfa_trait_declaring_list pop ',' push identifier_or_type_name
		{ $$ = $1->appendList( $1->cloneType( $5 ) ); }
	;

trait_declaring_list:									// CFA
	type_specifier declarator
		{ $$ = $2->addType( $1 ); }
	| trait_declaring_list pop ',' push declarator
		{ $$ = $1->appendList( $1->cloneBaseType( $5 ) ); }
	;

//***************************** EXTERNAL DEFINITIONS *****************************

translation_unit:
	// empty, input file
	| external_definition_list
		{ parseTree = parseTree ? parseTree->appendList( $1 ) : $1;	}
	;

external_definition_list:
	push external_definition pop
		{ $$ = $2; }
	| external_definition_list push external_definition pop
		{ $$ = $1 ? $1->appendList( $3 ) : $3; }
	;

external_definition_list_opt:
	// empty
		{ $$ = nullptr; }
	| external_definition_list
	;

up:
		{ typedefTable.up( forall ); forall = false; }
	;

down:
		{ typedefTable.down(); }
	;

external_definition:
	DIRECTIVE
		{ $$ = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( $1 ) ) ); }
	| declaration
	| external_function_definition
	| EXTENSION external_definition						// GCC, multiple __extension__ allowed, meaning unknown
		{
			distExt( $2 );								// mark all fields in list
			$$ = $2;
		}
	| ASM '(' string_literal ')' ';'					// GCC, global assembler statement
		{ $$ = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, $3, 0 ) ) ); }
	| EXTERN STRINGliteral								// C++-style linkage specifier
		{
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, $2 );
		}
	  '{' up external_definition_list_opt down '}'
		{
			linkage = linkageStack.top();
			linkageStack.pop();
			$$ = $6;
		}
	| type_qualifier_list
		{
			if ( $1->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( $1->type->forall ) forall = true;		// remember generic type
		}
	  '{' up external_definition_list_opt down '}'		// CFA, namespace
		{
			distQual( $5, $1 );
 			forall = false;
			$$ = $5;
		}
	| declaration_qualifier_list
		{
			if ( $1->type && $1->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( $1->type && $1->type->forall ) forall = true; // remember generic type
		}
	  '{' up external_definition_list_opt down '}'		// CFA, namespace
		{
			distQual( $5, $1 );
 			forall = false;
			$$ = $5;
		}
	| declaration_qualifier_list type_qualifier_list
		{
			if ( ($1->type && $1->type->qualifiers.val) || $2->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ($1->type && $1->type->forall) || $2->type->forall ) forall = true; // remember generic type
		}
	  '{' up external_definition_list_opt down '}'		// CFA, namespace
		{
			distQual( $6, $1->addQualifiers( $2 ) );
 			forall = false;
			$$ = $6;
		}
	;

external_function_definition:
	function_definition
		// These rules are a concession to the "implicit int" type_specifier because there is a significant amount of
		// legacy code with global functions missing the type-specifier for the return type, and assuming "int".
		// Parsing is possible because function_definition does not appear in the context of an expression (nested
		// functions preclude this concession, i.e., all nested function must have a return type). A function prototype
		// declaration must still have a type_specifier.  OBSOLESCENT (see 1)
	| function_declarator compound_statement
		{ $$ = $1->addFunctionBody( $2 ); }
	| KR_function_declarator KR_parameter_list_opt compound_statement
		{ $$ = $1->addOldDeclList( $2 )->addFunctionBody( $3 ); }
	;

with_clause_opt:
	// empty
		{ $$ = nullptr; forall = false; }
	| WITH '(' tuple_expression_list ')'
		{ $$ = $3; forall = false; }
	;

function_definition:
	cfa_function_declaration with_clause_opt compound_statement	// CFA
		{
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			$1->get_last()->addFunctionBody( $3, $2 );
			$$ = $1;
		}
	| declaration_specifier function_declarator with_clause_opt compound_statement
		{
			rebindForall( $1, $2 );
			$$ = $2->addFunctionBody( $4, $3 )->addType( $1 );
		}
	| declaration_specifier variable_type_redeclarator with_clause_opt compound_statement
		{
			rebindForall( $1, $2 );
			$$ = $2->addFunctionBody( $4, $3 )->addType( $1 );
		}
		// handles default int return type, OBSOLESCENT (see 1)
	| type_qualifier_list function_declarator with_clause_opt compound_statement
		{ $$ = $2->addFunctionBody( $4, $3 )->addQualifiers( $1 ); }
		// handles default int return type, OBSOLESCENT (see 1)
	| declaration_qualifier_list function_declarator with_clause_opt compound_statement
		{ $$ = $2->addFunctionBody( $4, $3 )->addQualifiers( $1 ); }
		// handles default int return type, OBSOLESCENT (see 1)
	| declaration_qualifier_list type_qualifier_list function_declarator with_clause_opt compound_statement
		{ $$ = $3->addFunctionBody( $5, $4 )->addQualifiers( $2 )->addQualifiers( $1 ); }

		// Old-style K&R function definition, OBSOLESCENT (see 4)
	| declaration_specifier KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement
		{
			rebindForall( $1, $2 );
			$$ = $2->addOldDeclList( $3 )->addFunctionBody( $5, $4 )->addType( $1 );
		}
		// handles default int return type, OBSOLESCENT (see 1)
	| type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement
		{ $$ = $2->addOldDeclList( $3 )->addFunctionBody( $5, $4 )->addQualifiers( $1 ); }
		// handles default int return type, OBSOLESCENT (see 1)
	| declaration_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement
		{ $$ = $2->addOldDeclList( $3 )->addFunctionBody( $5, $4 )->addQualifiers( $1 ); }
		// handles default int return type, OBSOLESCENT (see 1)
	| declaration_qualifier_list type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement
		{ $$ = $3->addOldDeclList( $4 )->addFunctionBody( $6, $5 )->addQualifiers( $2 )->addQualifiers( $1 ); }
	;

declarator:
	variable_declarator
	| variable_type_redeclarator
	| function_declarator
	;

subrange:
	constant_expression '~' constant_expression			// CFA, integer subrange
		{ $$ = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( $1 ), maybeMoveBuild<Expression>( $3 ) ) ); }
	;

asm_name_opt:											// GCC
	// empty
		{ $$ = nullptr; }
	| ASM '(' string_literal ')' attribute_list_opt
		{
			DeclarationNode * name = new DeclarationNode();
			name->asmName = $3;
			$$ = name->addQualifiers( $5 );
		}
	;

attribute_list_opt:										// GCC
	// empty
		{ $$ = nullptr; }
	| attribute_list
	;

attribute_list:											// GCC
	attribute
	| attribute_list attribute
		{ $$ = $2->addQualifiers( $1 ); }
	;

attribute:												// GCC
	ATTRIBUTE '(' '(' attribute_name_list ')' ')'
		{ $$ = $4; }
	;

attribute_name_list:									// GCC
	attribute_name
	| attribute_name_list ',' attribute_name
		{ $$ = $3->addQualifiers( $1 ); }
	;

attribute_name:											// GCC
	// empty
		{ $$ = nullptr; }
	| attr_name
		{ $$ = DeclarationNode::newAttribute( $1 ); }
	| attr_name '(' argument_expression_list_opt ')'
		{ $$ = DeclarationNode::newAttribute( $1, $3 ); }
	;

attr_name:												// GCC
	IDENTIFIER
	| quasi_keyword
	| TYPEDEFname
	| TYPEGENname
	| FALLTHROUGH
		{ $$ = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
	| CONST
		{ $$ = Token{ new string( "__const__" ), { nullptr, -1 } }; }
	;

// ============================================================================
// The following sections are a series of grammar patterns used to parse declarators. Multiple patterns are necessary
// because the type of an identifier in wrapped around the identifier in the same form as its usage in an expression, as
// in:
//
//		int (*f())[10] { ... };
//		... (*f())[3] += 1;		// definition mimics usage
//
// Because these patterns are highly recursive, changes at a lower level in the recursion require copying some or all of
// the pattern. Each of these patterns has some subtle variation to ensure correct syntax in a particular context.
// ============================================================================

// ----------------------------------------------------------------------------
// The set of valid declarators before a compound statement for defining a function is less than the set of declarators
// to define a variable or function prototype, e.g.:
//
//		valid declaration		invalid definition
//		-----------------		------------------
//		int f;					int f {}
//		int *f;					int *f {}
//		int f[10];				int f[10] {}
//		int (*f)(int);			int (*f)(int) {}
//
// To preclude this syntactic anomaly requires separating the grammar rules for variable and function declarators, hence
// variable_declarator and function_declarator.
// ----------------------------------------------------------------------------

// This pattern parses a declaration of a variable that is not redefining a typedef name. The pattern precludes
// declaring an array of functions versus a pointer to an array of functions.

paren_identifier:
	identifier_at
		{ $$ = DeclarationNode::newName( $1 ); }
	| '(' paren_identifier ')'							// redundant parenthesis
		{ $$ = $2; }
	;

variable_declarator:
	paren_identifier attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_ptr
	| variable_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

variable_ptr:
	ptrref_operator variable_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list variable_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' variable_ptr ')' attribute_list_opt			// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	| '(' attribute_list variable_ptr ')' attribute_list_opt // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addQualifiers( $5 ); }
	;

variable_array:
	paren_identifier array_dimension
		{ $$ = $1->addArray( $2 ); }
	| '(' variable_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list variable_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' variable_array ')' multi_array_dimension 		// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list variable_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' variable_array ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list variable_array ')'				// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

variable_function:
	'(' variable_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' attribute_list variable_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $3->addQualifiers( $2 )->addParamList( $7 ); }
	| '(' variable_function ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list variable_function ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

// This pattern parses a function declarator that is not redefining a typedef name. For non-nested functions, there is
// no context where a function definition can redefine a typedef name, i.e., the typedef and function name cannot exist
// is the same scope.  The pattern precludes returning arrays and functions versus pointers to arrays and functions.

function_declarator:
	function_no_ptr attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| function_ptr
	| function_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

function_no_ptr:
	paren_identifier '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $4 ); }
	| '(' function_ptr ')' '(' push parameter_type_list_opt pop ')'
		{ $$ = $2->addParamList( $6 ); }
	| '(' attribute_list function_ptr ')' '(' push parameter_type_list_opt pop ')'
		{ $$ = $3->addQualifiers( $2 )->addParamList( $7 ); }
	| '(' function_no_ptr ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list function_no_ptr ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

function_ptr:
	ptrref_operator function_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list function_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' function_ptr ')' attribute_list_opt
		{ $$ = $2->addQualifiers( $4 ); }
	| '(' attribute_list function_ptr ')' attribute_list_opt
		{ $$ = $3->addQualifiers( $2 )->addQualifiers( $5 ); }
	;

function_array:
	'(' function_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list function_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' function_array ')' multi_array_dimension		// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list function_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' function_array ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list function_array ')'				// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

// This pattern parses an old-style K&R function declarator (OBSOLESCENT, see 4)
//
//   f( a, b, c ) int a, *b, c[]; {}
//
// that is not redefining a typedef name (see function_declarator for additional comments). The pattern precludes
// returning arrays and functions versus pointers to arrays and functions.

KR_function_declarator:
	KR_function_no_ptr
	| KR_function_ptr
	| KR_function_array
	;

KR_function_no_ptr:
	paren_identifier '(' identifier_list ')'			// function_declarator handles empty parameter
		{ $$ = $1->addIdList( $3 ); }
	| '(' KR_function_ptr ')' '(' push parameter_type_list_opt pop ')'
		{ $$ = $2->addParamList( $6 ); }
	| '(' attribute_list KR_function_ptr ')' '(' push parameter_type_list_opt pop ')'
		{ $$ = $3->addQualifiers( $2 )->addParamList( $7 ); }
	| '(' KR_function_no_ptr ')'						// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list KR_function_no_ptr ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

KR_function_ptr:
	ptrref_operator KR_function_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list KR_function_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' KR_function_ptr ')'
		{ $$ = $2; }
	| '(' attribute_list KR_function_ptr ')'
		{ $$ = $3->addQualifiers( $2 ); }
	;

KR_function_array:
	'(' KR_function_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list KR_function_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' KR_function_array ')' multi_array_dimension	// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list KR_function_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' KR_function_array ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list KR_function_array ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

// This pattern parses a declaration for a variable or function prototype that redefines a type name, e.g.:
//
//		typedef int foo;
//		{
//		   int foo; // redefine typedef name in new scope
//		}
//
// The pattern precludes declaring an array of functions versus a pointer to an array of functions, and returning arrays
// and functions versus pointers to arrays and functions.

paren_type:
	typedef_name
		{
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *$1->name, IDENTIFIER, "ID" );
		}
	| '(' paren_type ')'
		{ $$ = $2; }
	;

variable_type_redeclarator:
	paren_type attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| type_ptr
	| type_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| type_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

type_ptr:
	ptrref_operator variable_type_redeclarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list variable_type_redeclarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' type_ptr ')' attribute_list_opt				// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	| '(' attribute_list type_ptr ')' attribute_list_opt // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addQualifiers( $5 ); }
	;

type_array:
	paren_type array_dimension
		{ $$ = $1->addArray( $2 ); }
	| '(' type_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list type_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' type_array ')' multi_array_dimension			// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list type_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' type_array ')'								// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list type_array ')'					// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

type_function:
	paren_type '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $4 ); }
	| '(' type_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' attribute_list type_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $3->addQualifiers( $2 )->addParamList( $7 ); }
	| '(' type_function ')'								// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list type_function ')'				// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

// This pattern parses a declaration for a parameter variable of a function prototype or actual that is not redefining a
// typedef name and allows the C99 array options, which can only appear in a parameter list.  The pattern precludes
// declaring an array of functions versus a pointer to an array of functions, and returning arrays and functions versus
// pointers to arrays and functions.

identifier_parameter_declarator:
	paren_identifier attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| '&' MUTEX paren_identifier attribute_list_opt
		{ $$ = $3->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( $4 ); }
	| identifier_parameter_ptr
	| identifier_parameter_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| identifier_parameter_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

identifier_parameter_ptr:
	ptrref_operator identifier_parameter_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list identifier_parameter_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' identifier_parameter_ptr ')' attribute_list_opt // redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	;

identifier_parameter_array:
	paren_identifier array_parameter_dimension
		{ $$ = $1->addArray( $2 ); }
	| '(' identifier_parameter_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' identifier_parameter_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' identifier_parameter_array ')'				// redundant parenthesis
		{ $$ = $2; }
	;

identifier_parameter_function:
	paren_identifier '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $4 ); }
	| '(' identifier_parameter_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' identifier_parameter_function ')'				// redundant parenthesis
		{ $$ = $2; }
	;

// This pattern parses a declaration for a parameter variable or function prototype that is redefining a typedef name,
// e.g.:
//
//		typedef int foo;
//		forall( otype T ) struct foo;
//		int f( int foo ); // redefine typedef name in new scope
//
// and allows the C99 array options, which can only appear in a parameter list.

type_parameter_redeclarator:
	typedef_name attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| '&' MUTEX typedef_name attribute_list_opt
		{ $$ = $3->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( $4 ); }
	| type_parameter_ptr
	| type_parameter_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| type_parameter_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

typedef_name:
	TYPEDEFname
		{ $$ = DeclarationNode::newName( $1 ); }
	| TYPEGENname
		{ $$ = DeclarationNode::newName( $1 ); }
	;

type_parameter_ptr:
	ptrref_operator type_parameter_redeclarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list type_parameter_redeclarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' type_parameter_ptr ')' attribute_list_opt		// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	;

type_parameter_array:
	typedef_name array_parameter_dimension
		{ $$ = $1->addArray( $2 ); }
	| '(' type_parameter_ptr ')' array_parameter_dimension
		{ $$ = $2->addArray( $4 ); }
	;

type_parameter_function:
	typedef_name '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $4 ); }
	| '(' type_parameter_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	;

// This pattern parses a declaration of an abstract variable or function prototype, i.e., there is no identifier to
// which the type applies, e.g.:
//
//		sizeof( int );
//		sizeof( int * );
//		sizeof( int [10] );
//		sizeof( int (*)() );
//		sizeof( int () );
//
// The pattern precludes declaring an array of functions versus a pointer to an array of functions, and returning arrays
// and functions versus pointers to arrays and functions.

abstract_declarator:
	abstract_ptr
	| abstract_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| abstract_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

abstract_ptr:
	ptrref_operator
		{ $$ = DeclarationNode::newPointer( 0, $1 ); }
	| ptrref_operator type_qualifier_list
		{ $$ = DeclarationNode::newPointer( $2, $1 ); }
	| ptrref_operator abstract_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list abstract_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' abstract_ptr ')' attribute_list_opt
		{ $$ = $2->addQualifiers( $4 ); }
	;

abstract_array:
	array_dimension
	| '(' abstract_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' abstract_array ')' multi_array_dimension		// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' abstract_array ')'							// redundant parenthesis
		{ $$ = $2; }
	;

abstract_function:
	'(' push parameter_type_list_opt pop ')'			// empty parameter list OBSOLESCENT (see 3)
		{ $$ = DeclarationNode::newFunction( nullptr, nullptr, $3, nullptr ); }
	| '(' abstract_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' abstract_function ')'							// redundant parenthesis
		{ $$ = $2; }
	;

array_dimension:
		// Only the first dimension can be empty.
	'[' ']'
		{ $$ = DeclarationNode::newArray( 0, 0, false ); }
	| '[' ']' multi_array_dimension
		{ $$ = DeclarationNode::newArray( 0, 0, false )->addArray( $3 ); }
	| '[' push assignment_expression pop ',' comma_expression ']'
		{ $$ = DeclarationNode::newArray( $3, 0, false )->addArray( DeclarationNode::newArray( $6, 0, false ) ); }
		// { SemanticError( yylloc, "New array dimension is currently unimplemented." ); $$ = nullptr; }
	| multi_array_dimension
	;

multi_array_dimension:
	'[' push assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $3, 0, false ); }
	| '[' push '*' pop ']'								// C99
		{ $$ = DeclarationNode::newVarArray( 0 ); }
	| multi_array_dimension '[' push assignment_expression pop ']'
		{ $$ = $1->addArray( DeclarationNode::newArray( $4, 0, false ) ); }
	| multi_array_dimension '[' push '*' pop ']'		// C99
		{ $$ = $1->addArray( DeclarationNode::newVarArray( 0 ) ); }
	;

// This pattern parses a declaration of a parameter abstract variable or function prototype, i.e., there is no
// identifier to which the type applies, e.g.:
//
//		int f( int );			// not handled here
//		int f( int * );			// abstract function-prototype parameter; no parameter name specified
//		int f( int (*)() );		// abstract function-prototype parameter; no parameter name specified
//		int f( int (int) );		// abstract function-prototype parameter; no parameter name specified
//
// The pattern precludes declaring an array of functions versus a pointer to an array of functions, and returning arrays
// and functions versus pointers to arrays and functions. In addition, the pattern handles the
// special meaning of parenthesis around a typedef name:
//
//		ISO/IEC 9899:1999 Section 6.7.5.3(11) : "In a parameter declaration, a single typedef name in
//		parentheses is taken to be an abstract declarator that specifies a function with a single parameter,
//		not as redundant parentheses around the identifier."
//
// For example:
//
//		typedef float T;
//		int f( int ( T [5] ) );					// see abstract_parameter_declarator
//		int g( int ( T ( int ) ) );				// see abstract_parameter_declarator
//		int f( int f1( T a[5] ) );				// see identifier_parameter_declarator
//		int g( int g1( T g2( int p ) ) );		// see identifier_parameter_declarator
//
// In essence, a '(' immediately to the left of typedef name, T, is interpreted as starting a parameter type list, and
// not as redundant parentheses around a redeclaration of T. Finally, the pattern also precludes declaring an array of
// functions versus a pointer to an array of functions, and returning arrays and functions versus pointers to arrays and
// functions.

abstract_parameter_declarator:
	abstract_parameter_ptr
	| '&' MUTEX attribute_list_opt
		{ $$ = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( $3 ); }
	| abstract_parameter_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| abstract_parameter_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

abstract_parameter_ptr:
	ptrref_operator
		{ $$ = DeclarationNode::newPointer( nullptr, $1 ); }
	| ptrref_operator type_qualifier_list
		{ $$ = DeclarationNode::newPointer( $2, $1 ); }
	| ptrref_operator abstract_parameter_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| ptrref_operator type_qualifier_list abstract_parameter_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' abstract_parameter_ptr ')' attribute_list_opt	// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	;

abstract_parameter_array:
	array_parameter_dimension
	| '(' abstract_parameter_ptr ')' array_parameter_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' abstract_parameter_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' abstract_parameter_array ')'					// redundant parenthesis
		{ $$ = $2; }
	;

abstract_parameter_function:
	'(' push parameter_type_list_opt pop ')'			// empty parameter list OBSOLESCENT (see 3)
		{ $$ = DeclarationNode::newFunction( nullptr, nullptr, $3, nullptr ); }
	| '(' abstract_parameter_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' abstract_parameter_function ')'				// redundant parenthesis
		{ $$ = $2; }
	;

array_parameter_dimension:
		// Only the first dimension can be empty or have qualifiers.
	array_parameter_1st_dimension
	| array_parameter_1st_dimension multi_array_dimension
		{ $$ = $1->addArray( $2 ); }
	| multi_array_dimension
	;

// The declaration of an array parameter has additional syntax over arrays in normal variable declarations:
//
//		ISO/IEC 9899:1999 Section 6.7.5.2(1) : "The optional type qualifiers and the keyword static shall appear only in
//		a declaration of a function parameter with an array type, and then only in the outermost array type derivation."

array_parameter_1st_dimension:
	'[' ']'
		{ $$ = DeclarationNode::newArray( 0, 0, false ); }
		// multi_array_dimension handles the '[' '*' ']' case
	| '[' push type_qualifier_list '*' pop ']'			// remaining C99
		{ $$ = DeclarationNode::newVarArray( $3 ); }
	| '[' push type_qualifier_list pop ']'
		{ $$ = DeclarationNode::newArray( 0, $3, false ); }
		// multi_array_dimension handles the '[' assignment_expression ']' case
	| '[' push type_qualifier_list assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $4, $3, false ); }
	| '[' push STATIC type_qualifier_list_opt assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $5, $4, true ); }
	| '[' push type_qualifier_list STATIC assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $5, $3, true ); }
	;

// This pattern parses a declaration of an abstract variable, but does not allow "int ()" for a function pointer.
//
//		struct S {
//          int;
//          int *;
//          int [10];
//          int (*)();
//      };

variable_abstract_declarator:
	variable_abstract_ptr
	| variable_abstract_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_abstract_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

variable_abstract_ptr:
	ptrref_operator
		{ $$ = DeclarationNode::newPointer( 0, $1 ); }
	| ptrref_operator type_qualifier_list
		{ $$ = DeclarationNode::newPointer( $2, $1 ); }
	| ptrref_operator variable_abstract_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| ptrref_operator type_qualifier_list variable_abstract_declarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' variable_abstract_ptr ')' attribute_list_opt	// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	;

variable_abstract_array:
	array_dimension
	| '(' variable_abstract_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' variable_abstract_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' variable_abstract_array ')'					// redundant parenthesis
		{ $$ = $2; }
	;

variable_abstract_function:
	'(' variable_abstract_ptr ')' '(' push parameter_type_list_opt pop ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $6 ); }
	| '(' variable_abstract_function ')'				// redundant parenthesis
		{ $$ = $2; }
	;

// This pattern parses a new-style declaration for a parameter variable or function prototype that is either an
// identifier or typedef name and allows the C99 array options, which can only appear in a parameter list.

cfa_identifier_parameter_declarator_tuple:				// CFA
	cfa_identifier_parameter_declarator_no_tuple
	| cfa_abstract_tuple
	| type_qualifier_list cfa_abstract_tuple
		{ $$ = $2->addQualifiers( $1 ); }
	;

cfa_identifier_parameter_declarator_no_tuple:			// CFA
	cfa_identifier_parameter_ptr
	| cfa_identifier_parameter_array
	;

cfa_identifier_parameter_ptr:							// CFA
		// No SUE declaration in parameter list.
	ptrref_operator type_specifier_nobody
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator type_specifier_nobody
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_function
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_abstract_function
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_identifier_parameter_declarator_tuple
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_identifier_parameter_declarator_tuple
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	;

cfa_identifier_parameter_array:							// CFA
		// Only the first dimension can be empty or have qualifiers. Empty dimension must be factored out due to
		// shift/reduce conflict with new-style empty (void) function return type.
	'[' ']' type_specifier_nobody
		{ $$ = $3->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
	| cfa_array_parameter_1st_dimension type_specifier_nobody
		{ $$ = $2->addNewArray( $1 ); }
	| '[' ']' multi_array_dimension type_specifier_nobody
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
	| cfa_array_parameter_1st_dimension multi_array_dimension type_specifier_nobody
		{ $$ = $3->addNewArray( $2 )->addNewArray( $1 ); }
	| multi_array_dimension type_specifier_nobody
		{ $$ = $2->addNewArray( $1 ); }

	| '[' ']' cfa_identifier_parameter_ptr
		{ $$ = $3->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
	| cfa_array_parameter_1st_dimension cfa_identifier_parameter_ptr
		{ $$ = $2->addNewArray( $1 ); }
	| '[' ']' multi_array_dimension cfa_identifier_parameter_ptr
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
	| cfa_array_parameter_1st_dimension multi_array_dimension cfa_identifier_parameter_ptr
		{ $$ = $3->addNewArray( $2 )->addNewArray( $1 ); }
	| multi_array_dimension cfa_identifier_parameter_ptr
		{ $$ = $2->addNewArray( $1 ); }
	;

cfa_array_parameter_1st_dimension:
	'[' push type_qualifier_list '*' pop ']'			// remaining C99
		{ $$ = DeclarationNode::newVarArray( $3 ); }
	| '[' push type_qualifier_list assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $4, $3, false ); }
	| '[' push declaration_qualifier_list assignment_expression pop ']'
		// declaration_qualifier_list must be used because of shift/reduce conflict with
		// assignment_expression, so a semantic check is necessary to preclude them as a type_qualifier cannot
		// appear in this context.
		{ $$ = DeclarationNode::newArray( $4, $3, true ); }
	| '[' push declaration_qualifier_list type_qualifier_list assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $5, $4->addQualifiers( $3 ), true ); }
	;

// This pattern parses a new-style declaration of an abstract variable or function prototype, i.e., there is no
// identifier to which the type applies, e.g.:
//
//		[int] f( int );				// abstract variable parameter; no parameter name specified
//		[int] f( [int] (int) );		// abstract function-prototype parameter; no parameter name specified
//
// These rules need LR(3):
//
//		cfa_abstract_tuple identifier_or_type_name
//		'[' cfa_parameter_list ']' identifier_or_type_name '(' cfa_parameter_ellipsis_list_opt ')'
//
// since a function return type can be syntactically identical to a tuple type:
//
//		[int, int] t;
//		[int, int] f( int );
//
// Therefore, it is necessary to look at the token after identifier_or_type_name to know when to reduce
// cfa_abstract_tuple. To make this LR(1), several rules have to be flattened (lengthened) to allow the necessary
// lookahead. To accomplish this, cfa_abstract_declarator has an entry point without tuple, and tuple declarations are
// duplicated when appearing with cfa_function_specifier.

cfa_abstract_declarator_tuple:							// CFA
	cfa_abstract_tuple
	| type_qualifier_list cfa_abstract_tuple
		{ $$ = $2->addQualifiers( $1 ); }
	| cfa_abstract_declarator_no_tuple
	;

cfa_abstract_declarator_no_tuple:						// CFA
	cfa_abstract_ptr
	| cfa_abstract_array
	;

cfa_abstract_ptr:										// CFA
	ptrref_operator type_specifier
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator type_specifier
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_function
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_abstract_function
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_declarator_tuple
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( 0, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_abstract_declarator_tuple
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	;

cfa_abstract_array:										// CFA
		// Only the first dimension can be empty. Empty dimension must be factored out due to shift/reduce conflict with
		// empty (void) function return type.
	'[' ']' type_specifier
		{ $$ = $3->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| '[' ']' multi_array_dimension type_specifier
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| multi_array_dimension type_specifier
		{ $$ = $2->addNewArray( $1 ); }
	| '[' ']' cfa_abstract_ptr
		{ $$ = $3->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| '[' ']' multi_array_dimension cfa_abstract_ptr
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| multi_array_dimension cfa_abstract_ptr
		{ $$ = $2->addNewArray( $1 ); }
	;

cfa_abstract_tuple:										// CFA
	'[' push cfa_abstract_parameter_list pop ']'
		{ $$ = DeclarationNode::newTuple( $3 ); }
	| '[' push type_specifier_nobody ELLIPSIS pop ']'
		{ SemanticError( yylloc, "Tuple array currently unimplemented." ); $$ = nullptr; }
	| '[' push type_specifier_nobody ELLIPSIS constant_expression pop ']'
		{ SemanticError( yylloc, "Tuple array currently unimplemented." ); $$ = nullptr; }
	;

cfa_abstract_function:									// CFA
//	'[' ']' '(' cfa_parameter_ellipsis_list_opt ')'
//		{ $$ = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), $4, nullptr ); }
	cfa_abstract_tuple '(' push cfa_parameter_ellipsis_list_opt pop ')'
		{ $$ = DeclarationNode::newFunction( nullptr, $1, $4, nullptr ); }
	| cfa_function_return '(' push cfa_parameter_ellipsis_list_opt pop ')'
		{ $$ = DeclarationNode::newFunction( nullptr, $1, $4, nullptr ); }
	;

// 1) ISO/IEC 9899:1999 Section 6.7.2(2) : "At least one type specifier shall be given in the declaration specifiers in
//    each declaration, and in the specifier-qualifier list in each structure declaration and type name."
//
// 2) ISO/IEC 9899:1999 Section 6.11.5(1) : "The placement of a storage-class specifier other than at the beginning of
//    the declaration specifiers in a declaration is an obsolescent feature."
//
// 3) ISO/IEC 9899:1999 Section 6.11.6(1) : "The use of function declarators with empty parentheses (not
//    prototype-format parameter type declarators) is an obsolescent feature."
//
// 4) ISO/IEC 9899:1999 Section 6.11.7(1) : "The use of function definitions with separate parameter identifier and
//    declaration lists (not prototype-format parameter type and identifier declarators) is an obsolescent feature.

//************************* MISCELLANEOUS ********************************

comma_opt:												// redundant comma
	// empty
	| ','
	;

default_initializer_opt:
	// empty
		{ $$ = nullptr; }
	| '=' assignment_expression
		{ $$ = $2; }
	;

%%

// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
