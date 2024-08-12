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
// Last Modified On : Sat Aug 10 09:47:05 2024
// Update Count     : 6734
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
#include <sstream>
#include <stack>
using namespace std;

#include "DeclarationNode.hpp"                          // for DeclarationNode, ...
#include "ExpressionNode.hpp"                           // for ExpressionNode, ...
#include "InitializerNode.hpp"                          // for InitializerNode, ...
#include "ParserTypes.hpp"
#include "StatementNode.hpp"                            // for build_...
#include "TypedefTable.hpp"
#include "TypeData.hpp"
#include "AST/Type.hpp"                                 // for BasicType, BasicKind
#include "Common/SemanticError.hpp"                     // error_str
#include "Common/Utility.hpp"                           // for maybeMoveBuild, maybeBuild, CodeLo...

// lex uses __null in a boolean context, it's fine.
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wparentheses-equality"
#endif

extern DeclarationNode * parseTree;
extern ast::Linkage::Spec linkage;
extern TypedefTable typedefTable;

stack<ast::Linkage::Spec> linkageStack;

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

DeclarationNode * distAttr( DeclarationNode * typeSpec, DeclarationNode * declList ) {
	// Distribute type specifier across all declared variables, e.g., static, const, __attribute__.
	assert( declList );

	// Do not distribute attributes for aggregates because the attributes surrounding the aggregate belong it not the
	// variables in the declaration list, e.g.,
	//
	//   struct __attribute__(( aligned(128) )) S { ...
	//   } v1 __attribute__(( aligned(64) )), v2 __attribute__(( aligned(32) )), v3;
	//   struct S v4;
	//
	// v1 => 64, v2 =>32, v3 => 128, v2 => 128
	//
	// Anonymous aggregates are a special case because there is no aggregate to bind the attribute to; hence it floats
	// to the declaration list.
	//
	//   struct __attribute__(( aligned(128) )) /*anonymous */ { ... } v1;
	//
	// v1 => 128

	bool copyattr = ! (typeSpec->type && typeSpec->type->kind == TypeData::Aggregate && ! typeSpec->type->aggregate.anon );

	// addType copies the type information for the aggregate instances from typeSpec into cl's aggInst.aggregate.
	DeclarationNode * cl = (new DeclarationNode)->addType( typeSpec ); // typeSpec IS DELETED!!!

	// Start at second variable in declaration list and clone the type specifiers for each variable.
	for ( DeclarationNode * cur = declList->next ; cur != nullptr; cur = cur->next ) {
		cl->cloneBaseType( cur, copyattr );				// cur is modified
	} // for

	// Add first variable in declaration list with hidden type information in aggInst.aggregate, which is used by
	// extractType to recover the type for the aggregate instances.
	declList->addType( cl, copyattr );					// cl IS DELETED!!!
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration ; iter != nullptr ; iter = iter->next ) {
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
	if ( nullptr == fieldList ) {
		if ( !( typeSpec->type && typeSpec->type->kind == TypeData::Aggregate ) ) {
			stringstream ss;
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr );
	} // if

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distAttr( typeSpec, fieldList ); // mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( yylloc, *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( yylloc, *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)
#define MISSING_ANON_FIELD "illegal syntax, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "illegal syntax, missing low value for ascanding range so index is uninitialized."
#define MISSING_HIGH "illegal syntax, missing high value for descending range so index is uninitialized."

static ForCtrl * makeForCtrl( const CodeLocation & location, DeclarationNode * init, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val( location,
		compop,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val( location,
		// choose += or -= for upto/downto
		compop == OperKinds::LThan || compop == OperKinds::LEThan ? OperKinds::PlusAssn : OperKinds::MinusAssn,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "illegal syntax, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "illegal syntax, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ast::ConstantExpr * constant = dynamic_cast<ast::ConstantExpr *>(type->expr.get());
	if ( constant && (constant->rep == "0" || constant->rep == "1") ) {
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicKind::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

#define MISSING_LOOP_INDEX "illegal syntax, only a single identifier or declaration allowed in initialization, e.g., for ( i; ... ) or for ( int i; ... ). Expression disallowed."

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // forCtrl

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, OperKinds compop, ExpressionNode * range_over_expr, DeclarationNode * type ) {
	assert( compop == OperKinds::LEThan || compop == OperKinds::GEThan );
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl =
			DeclarationNode::newName( new std::string(identifier->name) )->addType( type );
		return new ForCtrl( new StatementNode( indexDecl ), range_over_expr, compop );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // enumRangeCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, adjacent identifiers \"%s\" and \"%s\" are not meaningful in an %s.\n"
				   "Possible cause is misspelled type name or missing generic parameter.",
				   identifier1.c_str(), identifier2.c_str(), kind );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, identifier \"%s\" cannot appear before a %s.\n"
				   "Possible cause is misspelled storage/CV qualifier, misspelled typename, or missing generic parameter.",
				   identifier.c_str(), kind );
} // IdentifierBeforeType

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
	// A raw token can be used.
	Token tok;

	// The general node types hold some generic node or list of nodes.
	DeclarationNode * decl;
	InitializerNode * init;
	ExpressionNode * expr;
	StatementNode * stmt;
	ClauseNode * clause;
	TypeData * type;

	// Special "nodes" containing compound information.
	CondCtl * ifctl;
	ForCtrl * forctl;
	LabelNode * labels;

	// Various flags and single values that become fields later.
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	OperKinds oper;
	bool is_volatile;
	EnumHiding enum_hiding;
	ast::ExceptionKind except_kind;
	// String passes ownership with it.
	std::string * str;

	// Narrower node types are used to avoid constant unwrapping.
	ast::WaitForStmt * wfs;
	ast::WaitUntilStmt::ClauseNode * wucn;
	ast::GenericExpr * genexpr;
}

// ************************ TERMINAL TOKENS ********************************

// keywords
%token TYPEDEF
%token EXTERN STATIC AUTO REGISTER
%token THREADLOCALGCC THREADLOCALC11					// GCC, C11
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
%token SIZEOF TYPEOF VA_LIST VA_ARG AUTO_TYPE COUNTOF	// GCC
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
%token CHOOSE FALLTHRU FALLTHROUGH WITH WHEN WAITFOR WAITUNTIL // CFA
%token CORUN COFOR
%token DISABLE ENABLE TRY THROW THROWRESUME AT			// CFA
%token ASM												// C99, extension ISO/IEC 9899:1999 Section J.5.10(1)
%token ALIGNAS ALIGNOF GENERIC STATICASSERT				// C11

// names and constants: lexer differentiates between identifier and typedef names
%token<tok> IDENTIFIER		TYPEDIMname		TYPEDEFname		TYPEGENname
%token<tok> TIMEOUT			WAND	WOR		CATCH			RECOVER			CATCHRESUME		FIXUP		FINALLY		// CFA
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
%token ATTR ELLIPSIS									// @@	...

%token EXPassign	MULTassign	DIVassign	MODassign	// \=	*=	/=	%=
%token PLUSassign	MINUSassign							// +=	-=
%token LSassign		RSassign							// <<=	>>=
%token ANDassign	ERassign	ORassign				// &=	^=	|=

%token ErangeUp		ErangeUpEq	ErangeDown	ErangeDownEq // +~	+~=/~=	-~	-~=
%token ATassign											// @=

%type<tok> identifier					identifier_at				identifier_or_type_name		attr_name
%type<tok> quasi_keyword
%type<expr> string_literal
%type<str> string_literal_list

%type<enum_hiding> hide_opt				visible_hide_opt

// expressions
%type<expr> constant
%type<expr> tuple						tuple_expression_list
%type<oper> ptrref_operator				unary_operator				assignment_operator			simple_assignment_operator	compound_assignment_operator
%type<expr> primary_expression			postfix_expression			unary_expression
%type<expr> cast_expression_list		cast_expression				exponential_expression		multiplicative_expression	additive_expression
%type<expr> shift_expression			relational_expression		equality_expression
%type<expr> AND_expression				exclusive_OR_expression		inclusive_OR_expression
%type<expr> logical_AND_expression		logical_OR_expression
%type<expr> conditional_expression		constant_expression			assignment_expression		assignment_expression_opt
%type<expr> comma_expression			comma_expression_opt
%type<expr> argument_expression_list_opt argument_expression_list	argument_expression			default_initializer_opt
%type<ifctl> conditional_declaration
%type<forctl> for_control_expression	for_control_expression_list
%type<oper> upupeq updown updowneq downupdowneq
%type<expr> subrange
%type<decl> asm_name_opt
%type<expr> asm_operands_opt			asm_operands_list			asm_operand
%type<labels> label_list
%type<expr> asm_clobbers_list_opt
%type<is_volatile> asm_volatile_opt
%type<expr> handler_predicate_opt
%type<genexpr> generic_association		generic_assoc_list

// statements
%type<stmt> statement					labeled_statement			compound_statement
%type<stmt> statement_decl				statement_decl_list			statement_list_nodecl
%type<stmt> selection_statement
%type<clause> switch_clause_list_opt	switch_clause_list
%type<expr> case_value
%type<clause> case_clause				case_value_list				case_label	case_label_list
%type<stmt> iteration_statement			jump_statement
%type<stmt> expression_statement		asm_statement
%type<stmt> with_statement
%type<expr> with_clause_opt
%type<stmt> corun_statement				cofor_statement
%type<stmt> exception_statement
%type<clause> handler_clause			finally_clause
%type<except_kind> handler_key
%type<stmt> mutex_statement
%type<expr> when_clause					when_clause_opt				waitfor		waituntil		timeout
%type<stmt> waitfor_statement			waituntil_statement
%type<wfs> wor_waitfor_clause
%type<wucn> waituntil_clause			wand_waituntil_clause       wor_waituntil_clause

// declarations
%type<decl> abstract_declarator abstract_ptr abstract_array abstract_function array_dimension multi_array_dimension
%type<decl> abstract_parameter_declarator_opt abstract_parameter_declarator abstract_parameter_ptr abstract_parameter_array abstract_parameter_function array_parameter_dimension array_parameter_1st_dimension
%type<decl> abstract_parameter_declaration

%type<aggKey> aggregate_key aggregate_data aggregate_control
%type<decl> aggregate_type aggregate_type_nobody

%type<decl> assertion assertion_list assertion_list_opt

%type<expr> bit_subrange_size_opt bit_subrange_size

%type<decl> basic_declaration_specifier basic_type_name basic_type_specifier direct_type indirect_type
%type<type> basic_type_name_type
%type<type> vtable vtable_opt default_opt

%type<decl> trait_declaration trait_declaration_list trait_declaring_list trait_specifier

%type<decl> declaration declaration_list declaration_list_opt declaration_qualifier_list
%type<decl> declaration_specifier declaration_specifier_nobody declarator declaring_list

%type<decl> elaborated_type elaborated_type_nobody

%type<decl> enumerator_list enum_type enum_type_nobody enum_key enumerator_type
%type<init> enumerator_value_opt

%type<decl> external_definition external_definition_list external_definition_list_opt

%type<decl> exception_declaration

%type<decl> field_declaration_list_opt field_declaration field_declaring_list_opt field_declarator field_abstract_list_opt field_abstract
%type<expr> field field_name_list field_name fraction_constants_opt

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

%type<decl> cfa_parameter_declaration cfa_parameter_list cfa_parameter_list_ellipsis_opt

%type<decl> cfa_typedef_declaration cfa_variable_declaration cfa_variable_specifier

%type<decl> c_declaration static_assert
%type<decl> KR_function_declarator KR_function_no_ptr KR_function_ptr KR_function_array
%type<decl> KR_parameter_list KR_parameter_list_opt

%type<decl> parameter_declaration parameter_list parameter_list_ellipsis_opt

%type<decl> paren_identifier paren_type

%type<decl> storage_class storage_class_list

%type<decl> sue_declaration_specifier sue_declaration_specifier_nobody sue_type_specifier sue_type_specifier_nobody

%type<tclass> type_class new_type_class
%type<decl> type_declarator type_declarator_name type_declaring_list

%type<decl> type_declaration_specifier type_type_specifier
%type<type> type_name typegen_name
%type<decl> typedef_name typedef_declaration typedef_expression

%type<decl> variable_type_redeclarator variable_type_ptr variable_type_array variable_type_function
%type<decl> general_function_declarator function_type_redeclarator function_type_array function_type_no_ptr function_type_ptr

%type<decl> type_parameter_redeclarator type_parameter_ptr type_parameter_array type_parameter_function

%type<decl> type type_no_function
%type<decl> type_parameter type_parameter_list type_initializer_opt

%type<expr> type_parameters_opt type_list array_type_list // array_dimension_list

%type<decl> type_qualifier forall type_qualifier_list_opt type_qualifier_list
%type<type> type_qualifier_name
%type<decl> type_specifier type_specifier_nobody

%type<decl> variable_declarator variable_ptr variable_array variable_function
%type<decl> variable_abstract_declarator variable_abstract_ptr variable_abstract_array variable_abstract_function

%type<decl> attribute_list_opt attribute_list attribute attribute_name_list attribute_name

// initializers
%type<init>  initializer initializer_list_opt initializer_opt

// designators
%type<expr>  designator designator_list designation


// Handle shift/reduce conflict for dangling else by shifting the ELSE token. For example, this string is ambiguous:
//   .---------.				matches IF '(' comma_expression ')' statement . (reduce)
//   if ( C ) S1 else S2
//   `-----------------'		matches IF '(' comma_expression ')' statement . (shift) ELSE statement */
// Similar issues exit with the waitfor statement.

// Order of these lines matters (low-to-high precedence). THEN is left associative over WAND/WOR/TIMEOUT/ELSE, WAND/WOR
// is left associative over TIMEOUT/ELSE, and TIMEOUT is left associative over ELSE.
%precedence THEN		// rule precedence for IF/WAITFOR statement
%precedence ANDAND		// token precedence for start of WAND in WAITFOR statement
%precedence WAND		// token precedence for start of WAND in WAITFOR statement
%precedence OROR		// token precedence for start of WOR in WAITFOR statement
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
// ************************ Namespace Management ********************************

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
//  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
//      push               pop   push                   pop

push:
		{ typedefTable.enterScope(); }
	;

pop:
		{ typedefTable.leaveScope(); }
	;

// ************************ CONSTANTS ********************************

constant:
		// ENUMERATIONconstant is not included here; it is treated as a variable with type "enumeration constant".
	INTEGERconstant								{ $$ = new ExpressionNode( build_constantInteger( yylloc, *$1 ) ); }
	| FLOATING_DECIMALconstant					{ $$ = new ExpressionNode( build_constantFloat( yylloc, *$1 ) ); }
	| FLOATING_FRACTIONconstant					{ $$ = new ExpressionNode( build_constantFloat( yylloc, *$1 ) ); }
	| FLOATINGconstant							{ $$ = new ExpressionNode( build_constantFloat( yylloc, *$1 ) ); }
	| CHARACTERconstant							{ $$ = new ExpressionNode( build_constantChar( yylloc, *$1 ) ); }
	;

quasi_keyword:											// CFA
	TIMEOUT
	| WAND
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

identifier_or_type_name:
	identifier
	| TYPEDEFname
	| TYPEGENname
	;

string_literal:
	string_literal_list							{ $$ = new ExpressionNode( build_constantStr( yylloc, *$1 ) ); }
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

// ************************ EXPRESSIONS ********************************

primary_expression:
	IDENTIFIER											// typedef name cannot be used as a variable name
		{ $$ = new ExpressionNode( build_varref( yylloc, $1 ) ); }
	| quasi_keyword
		{ $$ = new ExpressionNode( build_varref( yylloc, $1 ) ); }
	| TYPEDIMname										// CFA, generic length argument
		{ $$ = new ExpressionNode( build_dimensionref( yylloc, $1 ) ); }
	| tuple
	| '(' comma_expression ')'
		{ $$ = $2; }
	| '(' compound_statement ')'						// GCC, lambda expression
		{ $$ = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( $2 ) ) ) ); }
	| type_name '.' identifier							// CFA, nested type
		{ $$ = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( $1 ), build_varref( yylloc, $3 ) ) ); }
	| type_name '.' '[' field_name_list ']'				// CFA, nested type / tuple field selector
		{ SemanticError( yylloc, "Qualified name is currently unimplemented." ); $$ = nullptr; }
	| GENERIC '(' assignment_expression ',' generic_assoc_list ')' // C11
		{
			// add the missing control expression to the GenericExpr and return it
			$5->control = maybeMoveBuild( $3 );
			$$ = new ExpressionNode( $5 );
		}
	// | RESUME '(' comma_expression ')'
	//   	{ SemanticError( yylloc, "Resume expression is currently unimplemented." ); $$ = nullptr; }
	// | RESUME '(' comma_expression ')' compound_statement
	//   	{ SemanticError( yylloc, "Resume expression is currently unimplemented." ); $$ = nullptr; }
	| IDENTIFIER IDENTIFIER								// invalid syntax rule
		{ IdentifierBeforeIdentifier( *$1.str, *$2.str, "expression" ); $$ = nullptr; }
	| IDENTIFIER type_qualifier							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type qualifier" ); $$ = nullptr; }
	| IDENTIFIER storage_class							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "storage class" ); $$ = nullptr; }
	| IDENTIFIER basic_type_name						// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	| IDENTIFIER TYPEDEFname							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	| IDENTIFIER TYPEGENname							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	;

generic_assoc_list:										// C11
	generic_association
	| generic_assoc_list ',' generic_association
		{
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == $3->associations.size() );
			$1->associations.push_back( $3->associations.front() );
			delete $3;
			$$ = $1;
		}
	;

generic_association:									// C11
	type_no_function ':' assignment_expression
		{
			// create a GenericExpr wrapper with one association pair
			$$ = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( $1 ), maybeMoveBuild( $3 ) } } );
		}
	| DEFAULT ':' assignment_expression
		{ $$ = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( $3 ) } } ); }
	;

postfix_expression:
	primary_expression
	| postfix_expression '[' assignment_expression ',' tuple_expression_list ']'
		// Historic, transitional: Disallow commas in subscripts.
		// Switching to this behaviour may help check if a C compatibilty case uses comma-exprs in subscripts.
		// Current: Commas in subscripts make tuples.
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, $1, new ExpressionNode( build_tuple( yylloc, $3->set_last( $5 ) ) ) ) ); }
	| postfix_expression '[' assignment_expression ']'
		// CFA, comma_expression disallowed in this context because it results in a common user error: subscripting a
		// matrix with x[i,j] instead of x[i][j]. While this change is not backwards compatible, there seems to be
		// little advantage to this feature and many disadvantages. It is possible to write x[(i,j)] in CFA, which is
		// equivalent to the old x[i,j].
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, $1, $3 ) ); }
	| constant '[' assignment_expression ']'			// 3[a], 'a'[a], 3.5[a]
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, $1, $3 ) ); }
	| string_literal '[' assignment_expression ']'		// "abc"[3], 3["abc"]
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, $1, $3 ) ); }
	| postfix_expression '{' argument_expression_list_opt '}' // CFA, constructor call
		{
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			$$ = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), $1->set_last( $3 ) ) ) );
		}
	| postfix_expression '(' argument_expression_list_opt ')'
		{ $$ = new ExpressionNode( build_func( yylloc, $1, $3 ) ); }
	| VA_ARG '(' primary_expression ',' declaration_specifier_nobody abstract_parameter_declarator_opt ')'
		// { SemanticError( yylloc, "va_arg is currently unimplemented." ); $$ = nullptr; }
		{ $$ = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   $3->set_last( (ExpressionNode *)($6 ? $6->addType( $5 ) : $5) ) ) ); }
	| postfix_expression '`' identifier					// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( $3 ) ) ), $1 ) ); }
	| constant '`' identifier							// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( $3 ) ) ), $1 ) ); }
	| string_literal '`' identifier						// CFA, postfix call
		{ $$ = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( $3 ) ) ), $1 ) ); }

		// SKULLDUGGERY: The typedef table used for parsing does not store fields in structures. To parse a qualified
		// name, it is assumed all name-tokens after the first are identifiers, regardless of how the lexer identifies
    	// them. For example:
		//   
		//   struct S;
		//   forall(T) struct T;
		//   union U;
		//   enum E { S, T, E };
		//   struct Z { int S, T, Z, E, U; };
		//   void fred () {
		//       Z z;
		//       z.S;  // lexer returns S is TYPEDEFname
		//       z.T;  // lexer returns T is TYPEGENname
		//       z.Z;  // lexer returns Z is TYPEDEFname
		//       z.U;  // lexer returns U is TYPEDEFname
		//       z.E;  // lexer returns E is TYPEDEFname
		//   }
	| postfix_expression '.' identifier_or_type_name
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, build_varref( yylloc, $3 ) ) ); }

	| postfix_expression '.' INTEGERconstant			// CFA, tuple index
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, build_constantInteger( yylloc, *$3 ) ) ); }
	| postfix_expression FLOATING_FRACTIONconstant		// CFA, tuple index
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, build_field_name_FLOATING_FRACTIONconstant( yylloc, *$2 ) ) ); }
	| postfix_expression '.' '[' field_name_list ']'	// CFA, tuple field selector
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, build_tuple( yylloc, $4 ) ) ); }
	| postfix_expression '.' aggregate_control
		{ $$ = new ExpressionNode( build_keyword_cast( yylloc, $3, $1 ) ); }
	| postfix_expression ARROW identifier
		{ $$ = new ExpressionNode( build_pfieldSel( yylloc, $1, build_varref( yylloc, $3 ) ) ); }
	| postfix_expression ARROW INTEGERconstant			// CFA, tuple index
		{ $$ = new ExpressionNode( build_pfieldSel( yylloc, $1, build_constantInteger( yylloc, *$3 ) ) ); }
	| postfix_expression ARROW '[' field_name_list ']'	// CFA, tuple field selector
		{ $$ = new ExpressionNode( build_pfieldSel( yylloc, $1, build_tuple( yylloc, $4 ) ) ); }
	| postfix_expression ICR
		{ $$ = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, $1 ) ); }
	| postfix_expression DECR
		{ $$ = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, $1 ) ); }
	| '(' type_no_function ')' '{' initializer_list_opt comma_opt '}' // C99, compound-literal
		{ $$ = new ExpressionNode( build_compoundLiteral( yylloc, $2, new InitializerNode( $5, true ) ) ); }
	| '(' type_no_function ')' '@' '{' initializer_list_opt comma_opt '}' // CFA, explicit C compound-literal
		{ $$ = new ExpressionNode( build_compoundLiteral( yylloc, $2, (new InitializerNode( $6, true ))->set_maybeConstructed( false ) ) ); }
	| '^' primary_expression '{' argument_expression_list_opt '}' // CFA, destructor call
		{
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			$$ = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), $2->set_last( $4 ) ) );
		}
	;

field_name_list:										// CFA, tuple field selector
	field
	| field_name_list ',' field					{ $$ = $1->set_last( $3 ); }
	;

field:													// CFA, tuple field selector
	field_name
	| FLOATING_DECIMALconstant field
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *$1 ) ), maybeMoveBuild( $2 ) ) ); }
	| FLOATING_DECIMALconstant '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *$1 ) ), build_tuple( yylloc, $3 ) ) ); }
	| field_name '.' field
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, maybeMoveBuild( $3 ) ) ); }
	| field_name '.' '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_fieldSel( yylloc, $1, build_tuple( yylloc, $4 ) ) ); }
	| field_name ARROW field
		{ $$ = new ExpressionNode( build_pfieldSel( yylloc, $1, maybeMoveBuild( $3 ) ) ); }
	| field_name ARROW '[' field_name_list ']'
		{ $$ = new ExpressionNode( build_pfieldSel( yylloc, $1, build_tuple( yylloc, $4 ) ) ); }
	;

field_name:
	INTEGERconstant	fraction_constants_opt
		{ $$ = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *$1 ), $2 ) ); }
	| FLOATINGconstant fraction_constants_opt
		{ $$ = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *$1 ), $2 ) ); }
	| identifier_at fraction_constants_opt				// CFA, allow anonymous fields
		{
			$$ = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, $1 ), $2 ) );
		}
	;

fraction_constants_opt:
	// empty
		{ $$ = nullptr; }
	| fraction_constants_opt FLOATING_FRACTIONconstant
		{
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *$2 );
			$$ = $1 != nullptr ? new ExpressionNode( build_fieldSel( yylloc, $1, constant ) ) : new ExpressionNode( constant );
		}
	;

unary_expression:
	postfix_expression
		// first location where constant/string can have operator applied: sizeof 3/sizeof "abc" still requires
		// semantics checks, e.g., ++3, 3--, *3, &&3
	| constant
	| string_literal
		{ $$ = $1; }
	| EXTENSION cast_expression							// GCC
		{ $$ = $2->set_extension( true ); }
		// '*' ('&') is separated from unary_operator because of shift/reduce conflict in:
		//		{ * X; }	 // dereference X
		//		{ * int X; } // CFA declaration of pointer to int
	| ptrref_operator cast_expression					// CFA
		{
			switch ( $1 ) {
			case OperKinds::AddressOf:
				$$ = new ExpressionNode( new ast::AddressExpr( maybeMoveBuild( $2 ) ) );
				break;
			case OperKinds::PointTo:
				$$ = new ExpressionNode( build_unary_val( yylloc, $1, $2 ) );
				break;
			case OperKinds::And:
				$$ = new ExpressionNode( new ast::AddressExpr( new ast::AddressExpr( maybeMoveBuild( $2 ) ) ) );
				break;
			default:
				assert( false );
			}
		}
	| unary_operator cast_expression
		{ $$ = new ExpressionNode( build_unary_val( yylloc, $1, $2 ) ); }
	| ICR unary_expression
		{ $$ = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, $2 ) ); }
	| DECR unary_expression
		{ $$ = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, $2 ) ); }
	| SIZEOF unary_expression
		{ $$ = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( $2 ) ) ); }
	| SIZEOF '(' type_no_function ')'
		{ $$ = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( $3 ) ) ); }
	| ALIGNOF unary_expression							// GCC, variable alignment
		{ $$ = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( $2 ) ) ); }
	| ALIGNOF '(' type_no_function ')'					// GCC, type alignment
		{ $$ = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( $3 ) ) ); }

		// Cannot use rule "type", which includes cfa_abstract_function, for sizeof/alignof, because of S/R problems on
		// look ahead, so the cfa_abstract_function is factored out.
	| SIZEOF '(' cfa_abstract_function ')'
		{ $$ = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( $3 ) ) ); }
	| ALIGNOF '(' cfa_abstract_function ')'				// GCC, type alignment
		{ $$ = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( $3 ) ) ); }

	| OFFSETOF '(' type_no_function ',' identifier ')'
		{ $$ = new ExpressionNode( build_offsetOf( yylloc, $3, build_varref( yylloc, $5 ) ) ); }
	| TYPEID '(' type ')'
		{
			SemanticError( yylloc, "typeid name is currently unimplemented." ); $$ = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
	| COUNTOF unary_expression
		{  $$ = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( $2 ) ) ); }
	| COUNTOF '(' type_no_function ')'
		{ $$ = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( $3 ) ) ); }
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
		{ $$ = new ExpressionNode( build_cast( yylloc, $2, $4 ) ); }
	| '(' aggregate_control '&' ')' cast_expression		// CFA
		{ $$ = new ExpressionNode( build_keyword_cast( yylloc, $2, $5 ) ); }
	| '(' aggregate_control '*' ')' cast_expression		// CFA
		{ $$ = new ExpressionNode( build_keyword_cast( yylloc, $2, $5 ) ); }
	| '(' VIRTUAL ')' cast_expression					// CFA
		{ $$ = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( $4 ), nullptr ) ); }
	| '(' VIRTUAL type_no_function ')' cast_expression	// CFA
		{ $$ = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( $5 ), maybeMoveBuildType( $3 ) ) ); }
	| '(' RETURN type_no_function ')' cast_expression	// CFA
		{ $$ = new ExpressionNode( build_cast( yylloc, $3, $5, ast::CastExpr::Return ) ); }
	| '(' COERCE type_no_function ')' cast_expression	// CFA
		{ SemanticError( yylloc, "Coerce cast is currently unimplemented." ); $$ = nullptr; }
	| '(' qualifier_cast_list ')' cast_expression		// CFA
		{ SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); $$ = nullptr; }
//	| '(' type_no_function ')' tuple
//		{ $$ = new ast::ExpressionNode( build_cast( yylloc, $2, $4 ) ); }
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
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, $1, $3 ) ); }
	;

multiplicative_expression:
	exponential_expression
	| multiplicative_expression '*' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, $1, $3 ) ); }
	| multiplicative_expression '/' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, $1, $3 ) ); }
	| multiplicative_expression '%' exponential_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, $1, $3 ) ); }
	;

additive_expression:
	multiplicative_expression
	| additive_expression '+' multiplicative_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, $1, $3 ) ); }
	| additive_expression '-' multiplicative_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, $1, $3 ) ); }
	;

shift_expression:
	additive_expression
	| shift_expression LS additive_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, $1, $3 ) ); }
	| shift_expression RS additive_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, $1, $3 ) ); }
	;

relational_expression:
	shift_expression
	| relational_expression '<' shift_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, $1, $3 ) ); }
	| relational_expression '>' shift_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, $1, $3 ) ); }
	| relational_expression LE shift_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, $1, $3 ) ); }
	| relational_expression GE shift_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, $1, $3 ) ); }
	;

equality_expression:
	relational_expression
	| equality_expression EQ relational_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, $1, $3 ) ); }
	| equality_expression NE relational_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, $1, $3 ) ); }
	;

AND_expression:
	equality_expression
	| AND_expression '&' equality_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, $1, $3 ) ); }
	;

exclusive_OR_expression:
	AND_expression
	| exclusive_OR_expression '^' AND_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, $1, $3 ) ); }
	;

inclusive_OR_expression:
	exclusive_OR_expression
	| inclusive_OR_expression '|' exclusive_OR_expression
		{ $$ = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, $1, $3 ) ); }
	;

logical_AND_expression:
	inclusive_OR_expression
	| logical_AND_expression ANDAND inclusive_OR_expression
		{ $$ = new ExpressionNode( build_and_or( yylloc, $1, $3, ast::AndExpr ) ); }
	;

logical_OR_expression:
	logical_AND_expression
	| logical_OR_expression OROR logical_AND_expression
		{ $$ = new ExpressionNode( build_and_or( yylloc, $1, $3, ast::OrExpr ) ); }
	;

conditional_expression:
	logical_OR_expression
	| logical_OR_expression '?' comma_expression ':' conditional_expression
		{ $$ = new ExpressionNode( build_cond( yylloc, $1, $3, $5 ) ); }
	| logical_OR_expression '?' /* empty */ ':' conditional_expression // GCC, omitted first operand
		{ $$ = new ExpressionNode( build_cond( yylloc, $1, nullptr, $4 ) ); }
	;

constant_expression:
	conditional_expression
	;

argument_expression_list_opt:
	// empty
		{ $$ = nullptr; }
	| argument_expression_list
	;

argument_expression_list:
	argument_expression
	// | argument_expression_list_opt ',' argument_expression // CFA, allow empty argument
	| argument_expression_list ',' argument_expression	// no empty argument
		{ $$ = $1->set_last( $3 ); }
	;

argument_expression:
	'?'													// CFA, default parameter
		// { SemanticError( yylloc, "Argument to default parameter is currently unimplemented." ); $$ = nullptr; }
		{ $$ = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
	| '?' identifier '=' assignment_expression			// CFA, keyword argument
		// { SemanticError( yylloc, "keyword argument is currently unimplemented." ); $$ = nullptr; }
		{ $$ = $4; }
	| assignment_expression
	;

assignment_expression:
		// CFA, assignment is separated from assignment_operator to ensure no assignment operations for tuples
	conditional_expression
	| unary_expression assignment_operator assignment_expression
		{
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				$$ = new ExpressionNode( build_binary_val( yylloc, $2, $1, $3 ) );
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
		{ $$ = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( $3 ) ) ); }
	| '[' push assignment_expression pop ',' tuple_expression_list ']'
		{ $$ = new ExpressionNode( build_tuple( yylloc, $3->set_last( $6 ) ) ); }
	;

tuple_expression_list:
	assignment_expression
	| '@'												// CFA
		{ SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); $$ = nullptr; }
	| tuple_expression_list ',' assignment_expression
		{ $$ = $1->set_last( $3 ); }
	| tuple_expression_list ',' '@'
		{ SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); $$ = nullptr; }
	;

comma_expression:
	assignment_expression
	| comma_expression ',' assignment_expression
		{ $$ = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( $1 ), maybeMoveBuild( $3 ) ) ); }
	;

comma_expression_opt:
	// empty
		{ $$ = nullptr; }
	| comma_expression
	;

// ************************** STATEMENTS *******************************

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
	| waituntil_statement
	| corun_statement
	| cofor_statement
	| exception_statement
	| enable_disable_statement
		{ SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); $$ = nullptr; }
	| asm_statement
	| DIRECTIVE
		{ $$ = new StatementNode( build_directive( yylloc, $1 ) ); }
	;

labeled_statement:
		// labels cannot be identifiers 0 or 1
	identifier_or_type_name ':' attribute_list_opt statement
		{ $$ = $4->add_label( yylloc, $1, $3 ); }
	| identifier_or_type_name ':' attribute_list_opt error // invalid syntax rule
		{
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", $1.str->c_str() );
			$$ = nullptr;
		}
	;

compound_statement:
	'{' '}'
		{ $$ = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
	| '{' push
	  local_label_declaration_opt						// GCC, local labels appear at start of block
	  statement_decl_list								// C99, intermix declarations and statements
	  pop '}'
		{ $$ = new StatementNode( build_compound( yylloc, $4 ) ); }
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
	| statement_list_nodecl error						// invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); $$ = nullptr; }
	;

expression_statement:
	comma_expression_opt ';'
		{ $$ = new StatementNode( build_expr( yylloc, $1 ) ); }
	;

// "if", "switch", and "choose" require parenthesis around the conditional. See the following ambiguities without
// parenthesis:
//
//   if x + y + z; => if ( x ) + y + z or if ( x + y ) + z
//
//   switch O { }
// 
//     O{} => object-constructor for conditional, switch body ???
//     O{} => O for conditional followed by switch body
// 
//     C++ has this problem, as it has the same constructor syntax.
// 
//   switch sizeof ( T ) { }
// 
//     sizeof ( T ) => sizeof of T for conditional followed by switch body
//     sizeof ( T ) => sizeof of compound literal (T){ }, closing parenthesis ???
// 
//     Note the two grammar rules for sizeof (alignof)
// 
//       | SIZEOF unary_expression
//       | SIZEOF '(' type_no_function ')'
// 
//     where the first DOES NOT require parenthesis! And C++ inherits this problem from C.

selection_statement:
	IF '(' conditional_declaration ')' statement		%prec THEN
		// explicitly deal with the shift/reduce conflict on if/else
		{ $$ = new StatementNode( build_if( yylloc, $3, maybe_build_compound( yylloc, $5 ), nullptr ) ); }
	| IF '(' conditional_declaration ')' statement ELSE statement
		{ $$ = new StatementNode( build_if( yylloc, $3, maybe_build_compound( yylloc, $5 ), maybe_build_compound( yylloc, $7 ) ) ); }
	| SWITCH '(' comma_expression ')' case_clause
		{ $$ = new StatementNode( build_switch( yylloc, true, $3, $5 ) ); }
	| SWITCH '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}' // CFA
		{
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, $3, $8 ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			$$ = $7 ? new StatementNode( build_compound( yylloc, (new StatementNode( $7 ))->set_last( sw ) ) ) : sw;
		}
	| SWITCH '(' comma_expression ')' '{' error '}'		// CFA, invalid syntax rule error
		{ SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); $$ = nullptr; }
	| CHOOSE '(' comma_expression ')' case_clause		// CFA
		{ $$ = new StatementNode( build_switch( yylloc, false, $3, $5 ) ); }
	| CHOOSE '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}' // CFA
		{
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, $3, $8 ) );
			$$ = $7 ? new StatementNode( build_compound( yylloc, (new StatementNode( $7 ))->set_last( sw ) ) ) : sw;
		}
	| CHOOSE '(' comma_expression ')' '{' error '}'		// CFA, invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); $$ = nullptr; }
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
		{ $$ = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( $1 ), maybeMoveBuild( $3 ) ) ); }
	| subrange											// CFA, subrange
	;

case_value_list:										// CFA
	case_value									{ $$ = new ClauseNode( build_case( yylloc, $1 ) ); }
		// convert case list, e.g., "case 1, 3, 5:" into "case 1: case 3: case 5"
	| case_value_list ',' case_value			{ $$ = $1->set_last( new ClauseNode( build_case( yylloc, $3 ) ) ); }
	;

case_label:												// CFA
	CASE error											// invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, case list missing after case." ); $$ = nullptr; }
	| CASE case_value_list ':'					{ $$ = $2; }
	| CASE case_value_list error						// invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, colon missing after case list." ); $$ = nullptr; }
	| DEFAULT ':'								{ $$ = new ClauseNode( build_default( yylloc ) ); }
		// A semantic check is required to ensure only one default clause per switch/choose statement.
	| DEFAULT error										//  invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, colon missing after default." ); $$ = nullptr; }
	;

case_label_list:										// CFA
	case_label
	| case_label_list case_label				{ $$ = $1->set_last( $2 ); }
	;

case_clause:											// CFA
	case_label_list statement					{ $$ = $1->append_last_case( maybe_build_compound( yylloc, $2 ) ); }
	;

switch_clause_list_opt:									// CFA
	// empty
		{ $$ = nullptr; }
	| switch_clause_list
	;

switch_clause_list:										// CFA
	case_label_list statement_list_nodecl
		{ $$ = $1->append_last_case( new StatementNode( build_compound( yylloc, $2 ) ) ); }
	| switch_clause_list case_label_list statement_list_nodecl
		{ $$ = $1->set_last( $2->append_last_case( new StatementNode( build_compound( yylloc, $3 ) ) ) ); }
	;

iteration_statement:
	WHILE '(' ')' statement								%prec THEN // CFA => while ( 1 )
		{ $$ = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, $4 ) ) ); }
	| WHILE '(' ')' statement ELSE statement			// CFA
		{
			$$ = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, $4 ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
	| WHILE '(' conditional_declaration ')' statement	%prec THEN
		{ $$ = new StatementNode( build_while( yylloc, $3, maybe_build_compound( yylloc, $5 ) ) ); }
	| WHILE '(' conditional_declaration ')' statement ELSE statement // CFA
		{ $$ = new StatementNode( build_while( yylloc, $3, maybe_build_compound( yylloc, $5 ), $7 ) ); }
	| DO statement WHILE '(' ')' ';'					// CFA => do while( 1 )
		{ $$ = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, $2 ) ) ); }
	| DO statement WHILE '(' ')' ELSE statement			// CFA
		{
			$$ = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, $2 ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
	| DO statement WHILE '(' comma_expression ')' ';'
		{ $$ = new StatementNode( build_do_while( yylloc, $5, maybe_build_compound( yylloc, $2 ) ) ); }
	| DO statement WHILE '(' comma_expression ')' ELSE statement // CFA
		{ $$ = new StatementNode( build_do_while( yylloc, $5, maybe_build_compound( yylloc, $2 ), $8 ) ); }
	| FOR '(' ')' statement								%prec THEN // CFA => for ( ;; )
		{ $$ = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, $4 ) ) ); }
	| FOR '(' ')' statement ELSE statement				// CFA
		{
			$$ = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, $4 ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
	| FOR '(' for_control_expression_list ')' statement	%prec THEN
		{ $$ = new StatementNode( build_for( yylloc, $3, maybe_build_compound( yylloc, $5 ) ) ); }
	| FOR '(' for_control_expression_list ')' statement ELSE statement // CFA
		{ $$ = new StatementNode( build_for( yylloc, $3, maybe_build_compound( yylloc, $5 ), $7 ) ); }
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
					$1->condition->expr.reset( new ast::LogicalExpr( yylloc, $1->condition->expr.release(), $3->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else $1->condition = $3->condition;
			if ( $1->change ) {
				if ( $3->change ) {
					$1->change->expr.reset( new ast::CommaExpr( yylloc, $1->change->expr.release(), $3->change->expr.release() ) );
				} // if
			} else $1->change = $3->change;
			$$ = $1;
		}
	;

for_control_expression:
	';' comma_expression_opt ';' comma_expression_opt
		{ $$ = new ForCtrl( nullptr, $2, $4 ); }
	| comma_expression ';' comma_expression_opt ';' comma_expression_opt
		{
			$$ = new ForCtrl( $1 ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( $1 ) ) ) : nullptr, $3, $5 );
		}
	| declaration comma_expression_opt ';' comma_expression_opt // C99, declaration has ';'
		{ $$ = new ForCtrl( new StatementNode( $1 ), $2, $4 ); }

	| '@' ';' comma_expression							// CFA, empty loop-index
		{ $$ = new ForCtrl( nullptr, $3, nullptr ); }
	| '@' ';' comma_expression ';' comma_expression		// CFA, empty loop-index
		{ $$ = new ForCtrl( nullptr, $3, $5 ); }

	| comma_expression									// CFA, anonymous loop-index
		{ $$ = forCtrl( yylloc, $1, new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, $1->clone(), NEW_ONE ); }
	| downupdowneq comma_expression						// CFA, anonymous loop-index
		{ $$ = forCtrl( yylloc, $2, new string( DeclarationNode::anonymous.newName() ), UPDOWN( $1, NEW_ZERO, $2->clone() ), $1, UPDOWN( $1, $2->clone(), NEW_ZERO ), NEW_ONE ); }

	| comma_expression updowneq comma_expression		// CFA, anonymous loop-index
		{ $$ = forCtrl( yylloc, $1, new string( DeclarationNode::anonymous.newName() ), UPDOWN( $2, $1->clone(), $3 ), $2, UPDOWN( $2, $3->clone(), $1->clone() ), NEW_ONE ); }
	| '@' updowneq comma_expression						// CFA, anonymous loop-index
		{
			if ( $2 == OperKinds::LThan || $2 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $3, new string( DeclarationNode::anonymous.newName() ), $3->clone(), $2, nullptr, NEW_ONE );
		}
	| comma_expression updowneq '@'						// CFA, anonymous loop-index
		{
			if ( $2 == OperKinds::LThan || $2 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
		}
	| comma_expression updowneq comma_expression '~' comma_expression // CFA, anonymous loop-index
		{ $$ = forCtrl( yylloc, $1, new string( DeclarationNode::anonymous.newName() ), UPDOWN( $2, $1->clone(), $3 ), $2, UPDOWN( $2, $3->clone(), $1->clone() ), $5 ); }
	| '@' updowneq comma_expression '~' comma_expression // CFA, anonymous loop-index
		{
			if ( $2 == OperKinds::LThan || $2 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $3, new string( DeclarationNode::anonymous.newName() ), $3->clone(), $2, nullptr, $5 );
		}
	| comma_expression updowneq '@' '~' comma_expression // CFA, anonymous loop-index
		{
			if ( $2 == OperKinds::LThan || $2 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
		}
	| comma_expression updowneq comma_expression '~' '@' // CFA, invalid syntax rule
		{ SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
	| '@' updowneq '@'									// CFA, invalid syntax rule
		{ SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
	| '@' updowneq comma_expression '~' '@'				// CFA, invalid syntax rule
		{ SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
	| comma_expression updowneq '@' '~' '@'				// CFA, invalid syntax rule
		{ SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }
	| '@' updowneq '@' '~' '@'							// CFA, invalid syntax rule
		{ SemanticError( yylloc, MISSING_ANON_FIELD ); $$ = nullptr; }

		// These rules accept a comma_expression for the initialization, when only an identifier is correct. Being
		// permissive allows for a better error message from forCtrl.
	| comma_expression ';' comma_expression				// CFA
		{ $$ = forCtrl( yylloc, $3, $1, NEW_ZERO, OperKinds::LThan, $3->clone(), NEW_ONE ); }
	| comma_expression ';' downupdowneq comma_expression // CFA
		{ $$ = forCtrl( yylloc, $4, $1, UPDOWN( $3, NEW_ZERO, $4->clone() ), $3, UPDOWN( $3, $4->clone(), NEW_ZERO ), NEW_ONE ); }

	| comma_expression ';' comma_expression updowneq comma_expression // CFA
		{ $$ = forCtrl( yylloc, $3, $1, UPDOWN( $4, $3->clone(), $5 ), $4, UPDOWN( $4, $5->clone(), $3->clone() ), NEW_ONE ); }
	| comma_expression ';' '@' updowneq comma_expression // CFA
		{
			if ( $4 == OperKinds::LThan || $4 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $5, $1, $5->clone(), $4, nullptr, NEW_ONE );
		}
	| comma_expression ';' comma_expression updowneq '@' // CFA
		{
			if ( $4 == OperKinds::GThan || $4 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $4 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $3, $1, $3->clone(), $4, nullptr, NEW_ONE );
		}
	| comma_expression ';' '@' updowneq '@'				// CFA, invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); $$ = nullptr; }

	| comma_expression ';' comma_expression updowneq comma_expression '~' comma_expression // CFA
		{ $$ = forCtrl( yylloc, $3, $1, UPDOWN( $4, $3->clone(), $5 ), $4, UPDOWN( $4, $5->clone(), $3->clone() ), $7 ); }
	| comma_expression ';' '@' updowneq comma_expression '~' comma_expression // CFA, invalid syntax rule
		{
			if ( $4 == OperKinds::LThan || $4 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $5, $1, $5->clone(), $4, nullptr, $7 );
		}
	| comma_expression ';' comma_expression updowneq '@' '~' comma_expression // CFA
		{
			if ( $4 == OperKinds::GThan || $4 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $4 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $3, $1, $3->clone(), $4, nullptr, $7 );
		}
	| comma_expression ';' comma_expression updowneq comma_expression '~' '@' // CFA
		{ $$ = forCtrl( yylloc, $3, $1, UPDOWN( $4, $3->clone(), $5 ), $4, UPDOWN( $4, $5->clone(), $3->clone() ), nullptr ); }
	| comma_expression ';' '@' updowneq comma_expression '~' '@' // CFA, invalid syntax rule
		{
			if ( $4 == OperKinds::LThan || $4 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $5, $1, $5->clone(), $4, nullptr, nullptr );
		}
	| comma_expression ';' comma_expression updowneq '@' '~' '@' // CFA
		{
			if ( $4 == OperKinds::GThan || $4 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $4 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $3, $1, $3->clone(), $4, nullptr, nullptr );
		}
	| comma_expression ';' '@' updowneq '@' '~' '@' // CFA
		{ SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); $$ = nullptr; }

	| declaration comma_expression						// CFA
		{ $$ = forCtrl( yylloc, $1, NEW_ZERO, OperKinds::LThan, $2, NEW_ONE ); }
	| declaration downupdowneq comma_expression			// CFA
		{ $$ = forCtrl( yylloc, $1, UPDOWN( $2, NEW_ZERO, $3 ), $2, UPDOWN( $2, $3->clone(), NEW_ZERO ), NEW_ONE ); }

	| declaration comma_expression updowneq comma_expression // CFA
		{ $$ = forCtrl( yylloc, $1, UPDOWN( $3, $2->clone(), $4 ), $3, UPDOWN( $3, $4->clone(), $2->clone() ), NEW_ONE ); }
	| declaration '@' updowneq comma_expression			// CFA
		{
			if ( $3 == OperKinds::LThan || $3 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $4, $3, nullptr, NEW_ONE );
		}
	| declaration comma_expression updowneq '@'			// CFA
		{
			if ( $3 == OperKinds::GThan || $3 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $3 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $2, $3, nullptr, NEW_ONE );
		}

	| declaration comma_expression updowneq comma_expression '~' comma_expression // CFA
		{ $$ = forCtrl( yylloc, $1, UPDOWN( $3, $2, $4 ), $3, UPDOWN( $3, $4->clone(), $2->clone() ), $6 ); }
	| declaration '@' updowneq comma_expression '~' comma_expression // CFA
		{
			if ( $3 == OperKinds::LThan || $3 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $4, $3, nullptr, $6 );
		}
	| declaration comma_expression updowneq '@' '~' comma_expression // CFA
		{
			if ( $3 == OperKinds::GThan || $3 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $3 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $2, $3, nullptr, $6 );
		}
	| declaration comma_expression updowneq comma_expression '~' '@' // CFA
		{ $$ = forCtrl( yylloc, $1, UPDOWN( $3, $2, $4 ), $3, UPDOWN( $3, $4->clone(), $2->clone() ), nullptr ); }
	| declaration '@' updowneq comma_expression '~' '@' // CFA
		{
			if ( $3 == OperKinds::LThan || $3 == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $4, $3, nullptr, nullptr );
		}
	| declaration comma_expression updowneq '@' '~' '@'	// CFA
		{
			if ( $3 == OperKinds::GThan || $3 == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); $$ = nullptr; }
			else if ( $3 == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); $$ = nullptr; }
			else $$ = forCtrl( yylloc, $1, $2, $3, nullptr, nullptr );
		}
	| declaration '@' updowneq '@' '~' '@'				// CFA, invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); $$ = nullptr; }

	| comma_expression ';' type_type_specifier						// CFA, enum type
		{
			$$ = enumRangeCtrl( $1, OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, $3->clone()->buildType() ) ), $3 );
		}
	| comma_expression ';' downupdowneq enum_key		// CFA, enum type, reverse direction
		{
			if ( $3 == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); $$ = nullptr;
				$3 = OperKinds::GEThan;
			} // if
			$$ = enumRangeCtrl( $1, $3, new ExpressionNode( new ast::TypeExpr( yylloc, $4->clone()->buildType() ) ), $4 );
		}
	;

enum_key:
	type_name
		{	typedefTable.makeTypedef( *$1->symbolic.name, "enum_type_nobody 1" );
			$$ = DeclarationNode::newEnum( $1->symbolic.name, nullptr, false, false ); }
	| ENUM identifier
		{	typedefTable.makeTypedef( *$2, "enum_type_nobody 2" );
			$$ = DeclarationNode::newEnum( $2, nullptr, false, false ); }
	| ENUM type_name
		{	typedefTable.makeTypedef( *$2->symbolic.name, "enum_type_nobody 3" );
			$$ = DeclarationNode::newEnum( $2->symbolic.name, nullptr, false, false ); }
	;

// This rule exists to handle the ambiguity with unary operator '~'. The rule is the same as updowneq minus the '~'.
// Specifically, "for ( ~5 )" means the complement of 5, not loop 0..4. Hence, in this case "for ( ~= 5 )", i.e., 0..5,
// it is not possible to just remove the '='. The entire '~=' must be removed.
downupdowneq:
	ErangeUp
		{ $$ = OperKinds::LThan; }
	| ErangeDown
		{ $$ = OperKinds::GThan; }
	| ErangeUpEq
		{ $$ = OperKinds::LEThan; }
	| ErangeDownEq
		{ $$ = OperKinds::GEThan; }
	;

updown:
	'~'													// shorthand 0 ~ 10 => 0 +~ 10
		{ $$ = OperKinds::LThan; }
	| ErangeUp
		{ $$ = OperKinds::LThan; }
	| ErangeDown
		{ $$ = OperKinds::GThan; }
	;

updowneq:
	updown
	| ErangeUpEq
		{ $$ = OperKinds::LEThan; }
	| ErangeDownEq
		{ $$ = OperKinds::GEThan; }
	;

jump_statement:
	GOTO identifier_or_type_name ';'
		{ $$ = new StatementNode( build_branch( yylloc, $2, ast::BranchStmt::Goto ) ); }
	| GOTO '*' comma_expression ';'						// GCC, computed goto
		// The syntax for the GCC computed goto violates normal expression precedence, e.g., goto *i+3; => goto *(i+3);
		// whereas normal operator precedence yields goto (*i)+3;
		{ $$ = new StatementNode( build_computedgoto( $3 ) ); }
		// A semantic check is required to ensure fallthru appears only in the body of a choose statement.
	| fall_through_name ';'								// CFA
		{ $$ = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
	| fall_through_name identifier_or_type_name ';'		// CFA
		{ $$ = new StatementNode( build_branch( yylloc, $2, ast::BranchStmt::FallThrough ) ); }
	| fall_through_name DEFAULT ';'						// CFA
		{ $$ = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
	| CONTINUE ';'
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement.
		{ $$ = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
	| CONTINUE identifier_or_type_name ';'				// CFA, multi-level continue
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement, and
		// the target of the transfer appears only at the start of an iteration statement.
		{ $$ = new StatementNode( build_branch( yylloc, $2, ast::BranchStmt::Continue ) ); }
	| BREAK ';'
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement.
		{ $$ = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
	| BREAK identifier_or_type_name ';'					// CFA, multi-level exit
		// A semantic check is required to ensure this statement appears only in the body of an iteration statement, and
		// the target of the transfer appears only at the start of an iteration statement.
		{ $$ = new StatementNode( build_branch( yylloc, $2, ast::BranchStmt::Break ) ); }
	| RETURN comma_expression_opt ';'
		{ $$ = new StatementNode( build_return( yylloc, $2 ) ); }
	| RETURN '{' initializer_list_opt comma_opt '}' ';'
		{ SemanticError( yylloc, "Initializer return is currently unimplemented." ); $$ = nullptr; }
	| SUSPEND ';'
		{ $$ = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
	| SUSPEND compound_statement
		{ $$ = new StatementNode( build_suspend( yylloc, $2, ast::SuspendStmt::None ) ); }
	| SUSPEND COROUTINE ';'
		{ $$ = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
	| SUSPEND COROUTINE compound_statement
		{ $$ = new StatementNode( build_suspend( yylloc, $3, ast::SuspendStmt::Coroutine ) ); }
	| SUSPEND GENERATOR ';'
		{ $$ = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
	| SUSPEND GENERATOR compound_statement
		{ $$ = new StatementNode( build_suspend( yylloc, $3, ast::SuspendStmt::Generator ) ); }
	| THROW assignment_expression_opt ';'				// handles rethrow
		{ $$ = new StatementNode( build_throw( yylloc, $2 ) ); }
	| THROWRESUME assignment_expression_opt ';'			// handles reresume
		{ $$ = new StatementNode( build_resume( yylloc, $2 ) ); }
	| THROWRESUME assignment_expression_opt AT assignment_expression ';' // handles reresume
		{ $$ = new StatementNode( build_resume_at( $2, $4 ) ); }
	;

fall_through_name:										// CFA
	FALLTHRU
	| FALLTHROUGH
	;

with_statement:
	WITH '(' type_list ')' statement					// support scoped enumeration
		{ $$ = new StatementNode( build_with( yylloc, $3, $5 ) ); }
	;

// If MUTEX becomes a general qualifier, there are shift/reduce conflicts, so possibly change syntax to "with mutex".
mutex_statement:
	MUTEX '(' argument_expression_list_opt ')' statement
		{
			if ( ! $3 ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); $$ = nullptr; }
			$$ = new StatementNode( build_mutex( yylloc, $3, $5 ) );
		}
	;

when_clause:
	WHEN '(' comma_expression ')'				{ $$ = $3; }
	;

when_clause_opt:
	// empty
		{ $$ = nullptr; }
	| when_clause
	;

cast_expression_list:
	cast_expression
	| cast_expression_list ',' cast_expression
		{ SemanticError( yylloc, "List of mutex member is currently unimplemented." ); $$ = nullptr; }
	;

timeout:
	TIMEOUT '(' comma_expression ')'			{ $$ = $3; }
	;

wor:
	OROR
	| WOR

waitfor:
	WAITFOR '(' cast_expression ')'
		{ $$ = $3; }
	| WAITFOR '(' cast_expression_list ':' argument_expression_list_opt ')'
		{ $$ = $3->set_last( $5 ); }
	;

wor_waitfor_clause:
	when_clause_opt waitfor statement					%prec THEN
		// Called first: create header for WaitForStmt.
		{ $$ = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), $1, $2, maybe_build_compound( yylloc, $3 ) ); }
	| wor_waitfor_clause wor when_clause_opt waitfor statement
		{ $$ = build_waitfor( yylloc, $1, $3, $4, maybe_build_compound( yylloc, $5 ) ); }
	| wor_waitfor_clause wor when_clause_opt ELSE statement
		{ $$ = build_waitfor_else( yylloc, $1, $3, maybe_build_compound( yylloc, $5 ) ); }
	| wor_waitfor_clause wor when_clause_opt timeout statement	%prec THEN
		{ $$ = build_waitfor_timeout( yylloc, $1, $3, $4, maybe_build_compound( yylloc, $5 ) ); }
	// "else" must be conditional after timeout or timeout is never triggered (i.e., it is meaningless)
	| wor_waitfor_clause wor when_clause_opt timeout statement wor ELSE statement // invalid syntax rule
		{ SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); $$ = nullptr; }
	| wor_waitfor_clause wor when_clause_opt timeout statement wor when_clause ELSE statement
		{ $$ = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, $1, $3, $4, maybe_build_compound( yylloc, $5 ) ), $7, maybe_build_compound( yylloc, $9 ) ); }
	;

waitfor_statement:
	wor_waitfor_clause									%prec THEN
		{ $$ = new StatementNode( $1 ); }
	;

wand:
	ANDAND
	| WAND
	;

waituntil:
	WAITUNTIL '(' comma_expression ')'
		{ $$ = $3; }
	;

waituntil_clause:
	when_clause_opt waituntil statement
		{ $$ = build_waituntil_clause( yylloc, $1, $2, maybe_build_compound( yylloc, $3 ) ); }
	| '(' wor_waituntil_clause ')'
		{ $$ = $2; }
	;

wand_waituntil_clause:
	waituntil_clause									%prec THEN
		{ $$ = $1; }
	| waituntil_clause wand wand_waituntil_clause
		{ $$ = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, $1, $3 ); }
	;

wor_waituntil_clause:
	wand_waituntil_clause
		{ $$ = $1; }
	| wor_waituntil_clause wor wand_waituntil_clause
		{ $$ = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, $1, $3 ); }
	| wor_waituntil_clause wor when_clause_opt ELSE statement
		{ $$ = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, $1, build_waituntil_else( yylloc, $3, maybe_build_compound( yylloc, $5 ) ) ); }
	;

waituntil_statement:
	wor_waituntil_clause								%prec THEN
		{ $$ = new StatementNode( build_waituntil_stmt( yylloc, $1 ) );	}
	;

corun_statement:
	CORUN statement
		{ $$ = new StatementNode( build_corun( yylloc, $2 ) ); }
	;

cofor_statement:
	COFOR '(' for_control_expression_list ')' statement
		{ $$ = new StatementNode( build_cofor( yylloc, $3, maybe_build_compound( yylloc, $5 ) ) ); }
	;

exception_statement:
	TRY compound_statement handler_clause					%prec THEN
		{ $$ = new StatementNode( build_try( yylloc, $2, $3, nullptr ) ); }
	| TRY compound_statement finally_clause
		{ $$ = new StatementNode( build_try( yylloc, $2, nullptr, $3 ) ); }
	| TRY compound_statement handler_clause finally_clause
		{ $$ = new StatementNode( build_try( yylloc, $2, $3, $4 ) ); }
	;

handler_clause:
	handler_key '(' push exception_declaration pop handler_predicate_opt ')' compound_statement
		{ $$ = new ClauseNode( build_catch( yylloc, $1, $4, $6, $8 ) ); }
	| handler_clause handler_key '(' push exception_declaration pop handler_predicate_opt ')' compound_statement
		{ $$ = $1->set_last( new ClauseNode( build_catch( yylloc, $2, $5, $7, $9 ) ) ); }
	;

handler_predicate_opt:
	// empty
		{ $$ = nullptr; }
	| ';' conditional_expression				{ $$ = $2; }
	;

handler_key:
	CATCH										{ $$ = ast::Terminate; }
	| RECOVER									{ $$ = ast::Terminate; }
	| CATCHRESUME								{ $$ = ast::Resume; }
	| FIXUP										{ $$ = ast::Resume; }
	;

finally_clause:
	FINALLY compound_statement					{ $$ = new ClauseNode( build_finally( yylloc, $2 ) ); }
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
		{ $$ = new StatementNode( build_asm( yylloc, $2, $4, nullptr ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ')' ';' // remaining GCC
		{ $$ = new StatementNode( build_asm( yylloc, $2, $4, $6 ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ')' ';'
		{ $$ = new StatementNode( build_asm( yylloc, $2, $4, $6, $8 ) ); }
	| ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers_list_opt ')' ';'
		{ $$ = new StatementNode( build_asm( yylloc, $2, $4, $6, $8, $10 ) ); }
	| ASM asm_volatile_opt GOTO '(' string_literal ':' ':' asm_operands_opt ':' asm_clobbers_list_opt ':' label_list ')' ';'
		{ $$ = new StatementNode( build_asm( yylloc, $2, $5, nullptr, $8, $10, $12 ) ); }
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
		{ $$ = $1->set_last( $3 ); }
	;

asm_operand:											// GCC
	string_literal '(' constant_expression ')'
		{ $$ = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( $1 ), maybeMoveBuild( $3 ) ) ); }
	| '[' IDENTIFIER ']' string_literal '(' constant_expression ')'
		{
			$$ = new ExpressionNode( new ast::AsmExpr( yylloc, *$2.str, maybeMoveBuild( $4 ), maybeMoveBuild( $6 ) ) );
			delete $2.str;
		}
	;

asm_clobbers_list_opt:									// GCC
	// empty
		{ $$ = nullptr; }								// use default argument
	| string_literal
		{ $$ = $1; }
	| asm_clobbers_list_opt ',' string_literal
		{ $$ = $1->set_last( $3 ); }
	;

label_list:
	identifier
		{
			$$ = new LabelNode(); $$->labels.emplace_back( yylloc, *$1 );
			delete $1;									// allocated by lexer
		}
	| label_list ',' identifier
		{
			$$ = $1; $1->labels.emplace_back( yylloc, *$3 );
			delete $3;									// allocated by lexer
		}
	;

// ****************************** DECLARATIONS *********************************

declaration_list_opt:									// used at beginning of switch statement
	// empty
		{ $$ = nullptr; }
	| declaration_list
	;

declaration_list:
	declaration
	| declaration_list declaration
		{ $$ = $1->set_last( $2 ); }
	;

KR_parameter_list_opt:									// used to declare parameter types in K&R style functions
	// empty
		{ $$ = nullptr; }
	| KR_parameter_list
	;

KR_parameter_list:
	c_declaration ';'
		{ $$ = $1; }
	| KR_parameter_list c_declaration ';'
		{ $$ = $1->set_last( $2 ); }
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
	| static_assert	';'									// C11
	;

static_assert:
	STATICASSERT '(' constant_expression ',' string_literal ')' // C11
		{ $$ = DeclarationNode::newStaticAssert( $3, maybeMoveBuild( $5 ) ); }
	| STATICASSERT '(' constant_expression ')'			// CFA
		{ $$ = DeclarationNode::newStaticAssert( $3, build_constantStr( yylloc, *new string( "\"\"" ) ) ); }

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
		{ $$ = $1->set_last( $1->cloneType( $5 )->addInitializer( $6 ) ); }
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

		// [ int s, int t ];			// declare s and t
		// [ int, int ] f();
		// [] g( int );
		// [ int x, int y ] = f();		// declare x and y, initialize each from f
		// g( x + y );
	| cfa_function_return asm_name_opt
		{ SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); $$ = nullptr; }
	| type_qualifier_list cfa_function_return asm_name_opt
		{ SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); $$ = nullptr; }
	;

cfa_function_declaration:								// CFA
	cfa_function_specifier
	| type_qualifier_list cfa_function_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| declaration_qualifier_list cfa_function_specifier
		{ $$ = $2->addQualifiers( $1 ); }
	| declaration_qualifier_list type_qualifier_list cfa_function_specifier
		{ $$ = $3->addQualifiers( $1 )->addQualifiers( $2 ); }
	| cfa_function_declaration ',' identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')'
		{
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( $1->type->base );
			$$ = $1->set_last( DeclarationNode::newFunction( $3, ret, $6, nullptr ) );
		}
	;

cfa_function_specifier:									// CFA
	'[' ']' identifier '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt
		{ $$ = DeclarationNode::newFunction( $3,  DeclarationNode::newTuple( nullptr ), $6, nullptr )->addQualifiers( $9 ); }
	| '[' ']' TYPEDEFname '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt
		{ $$ = DeclarationNode::newFunction( $3,  DeclarationNode::newTuple( nullptr ), $6, nullptr )->addQualifiers( $9 ); }
	// | '[' ']' TYPEGENname '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt
	// 	{ $$ = DeclarationNode::newFunction( $3,  DeclarationNode::newTuple( nullptr ), $6, nullptr )->addQualifiers( $9 ); }

		// identifier_or_type_name must be broken apart because of the sequence:
		//
		//   '[' ']' identifier_or_type_name '(' cfa_parameter_list_ellipsis_opt ')'
		//   '[' ']' type_specifier
		//
		// type_specifier can resolve to just TYPEDEFname (e.g., typedef int T; int f( T );). Therefore this must be
		// flattened to allow lookahead to the '(' without having to reduce identifier_or_type_name.
	| cfa_abstract_tuple identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt
		// To obtain LR(1 ), this rule must be factored out from function return type (see cfa_abstract_declarator).
		{ $$ = DeclarationNode::newFunction( $2, $1, $5, nullptr )->addQualifiers( $8 ); }
	| cfa_function_return identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt
		{ $$ = DeclarationNode::newFunction( $2, $1, $5, nullptr )->addQualifiers( $8 ); }
	;

cfa_function_return:									// CFA
	'[' push cfa_parameter_list pop ']'
		{ $$ = DeclarationNode::newTuple( $3 ); }
	| '[' push cfa_parameter_list ',' cfa_abstract_parameter_list pop ']'
		// To obtain LR(1 ), the last cfa_abstract_parameter_list is added into this flattened rule to lookahead to the ']'.
		{ $$ = DeclarationNode::newTuple( $3->set_last( $5 ) ); }
	;

cfa_typedef_declaration:								// CFA
	TYPEDEF cfa_variable_specifier
		{
			typedefTable.addToEnclosingScope( *$2->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			$$ = $2->addTypedef();
		}
	| TYPEDEF cfa_function_specifier
		{
			typedefTable.addToEnclosingScope( *$2->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			$$ = $2->addTypedef();
		}
	| cfa_typedef_declaration ',' identifier
		{
			typedefTable.addToEnclosingScope( *$3, TYPEDEFname, "cfa_typedef_declaration 3" );
			$$ = $1->set_last( $1->cloneType( $3 ) );
		}
	;

// Traditionally typedef is part of storage-class specifier for syntactic convenience only. Here, it is factored out as
// a separate form of declaration, which syntactically precludes storage-class specifiers and initialization.

typedef_declaration:
	TYPEDEF type_specifier declarator
		{
			typedefTable.addToEnclosingScope( *$3->name, TYPEDEFname, "typedef_declaration 1" );
			if ( $2->type->forall || ($2->type->kind == TypeData::Aggregate && $2->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); $$ = nullptr;
			} else $$ = $3->addType( $2 )->addTypedef(); // watchout frees $2 and $3
		}
	| typedef_declaration ',' declarator
		{
			typedefTable.addToEnclosingScope( *$3->name, TYPEDEFname, "typedef_declaration 2" );
			$$ = $1->set_last( $1->cloneBaseType( $3 )->addTypedef() );
		}
	| type_qualifier_list TYPEDEF type_specifier declarator // remaining OBSOLESCENT (see 2 )
		{ SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); $$ = nullptr; }
	| type_specifier TYPEDEF declarator
		{ SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); $$ = nullptr; }
	| type_specifier TYPEDEF type_qualifier_list declarator
		{ SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); $$ = nullptr; }
	;

typedef_expression:
		// deprecated GCC, naming expression type: typedef name = exp; gives a name to the type of an expression
	TYPEDEF identifier '=' assignment_expression
		{
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); $$ = nullptr;
		}
	| typedef_expression ',' identifier '=' assignment_expression
		{
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); $$ = nullptr;
		}
	;

c_declaration:
	declaration_specifier declaring_list
		{ $$ = distAttr( $1, $2 ); }
	| typedef_declaration
	| typedef_expression								// deprecated GCC, naming expression type
	| sue_declaration_specifier
		{
			assert( $1->type );
			if ( $1->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "illegal syntax, useless type qualifier(s) in empty declaration." ); $$ = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( $1->type->kind == TypeData::AggregateInst && $1->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "illegal syntax, useless storage qualifier(s) in empty aggregate declaration." ); $$ = nullptr;
			}
		}
	;

declaring_list:
		// A semantic check is required to ensure asm_name only appears on declarations with implicit or explicit static
		// storage-class
	variable_declarator asm_name_opt initializer_opt
		{ $$ = $1->addAsmName( $2 )->addInitializer( $3 ); }
	| variable_type_redeclarator asm_name_opt initializer_opt
		{ $$ = $1->addAsmName( $2 )->addInitializer( $3 ); }

	| general_function_declarator asm_name_opt
		{ $$ = $1->addAsmName( $2 )->addInitializer( nullptr ); }
	| general_function_declarator asm_name_opt '=' VOID
		{ $$ = $1->addAsmName( $2 )->addInitializer( new InitializerNode( true ) ); }

	| declaring_list ',' attribute_list_opt declarator asm_name_opt initializer_opt
		{ $$ = $1->set_last( $4->addQualifiers( $3 )->addAsmName( $5 )->addInitializer( $6 ) ); }
	;

general_function_declarator:
	function_type_redeclarator
	| function_declarator
	;

declaration_specifier:									// type specifier + storage class
	basic_declaration_specifier
	| type_declaration_specifier
	| sue_declaration_specifier
	| sue_declaration_specifier invalid_types			// invalid syntax rule
		{
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( $1->type->aggregate.kind ) );
			$$ = nullptr;
		}
	;

invalid_types:
	aggregate_key
	| basic_type_name
	| indirect_type
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
		{ $$ = DeclarationNode::newFromTypeData( $1 ); }
	| attribute											// trick handles most attribute locations
	;

type_qualifier_name:
	CONST
		{ $$ = build_type_qualifier( ast::CV::Const ); }
	| RESTRICT
		{ $$ = build_type_qualifier( ast::CV::Restrict ); }
	| VOLATILE
		{ $$ = build_type_qualifier( ast::CV::Volatile ); }
	| ATOMIC
		{ $$ = build_type_qualifier( ast::CV::Atomic ); }

		// forall is a CV qualifier because it can appear in places where SC qualifiers are disallowed.
		//
		//   void foo( forall( T ) T (*)( T ) ); // forward declaration
		//   void bar( static int ); // static disallowed (gcc/CFA)
	| forall
		{ $$ = build_forall( $1 ); }
	;

forall:
	FORALL '(' type_parameter_list ')'					// CFA
		{ $$ = $3; }
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
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
	| STATIC
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::Static ); }
	| AUTO
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
	| REGISTER
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::Register ); }
	| THREADLOCALGCC										// GCC
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
	| THREADLOCALC11										// C11
		{ $$ = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
		// Put function specifiers here to simplify parsing rules, but separate them semantically.
	| INLINE											// C99
		{ $$ = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
	| FORTRAN											// C99
		{ $$ = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
	| NORETURN											// C11
		{ $$ = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
	;

basic_type_name:
	basic_type_name_type
		{ $$ = DeclarationNode::newFromTypeData( $1 ); }
	;

// Just an intermediate value for conversion.
basic_type_name_type:
	VOID
		{ $$ = build_basic_type( TypeData::Void ); }
	| BOOL												// C99
		{ $$ = build_basic_type( TypeData::Bool ); }
	| CHAR
		{ $$ = build_basic_type( TypeData::Char ); }
	| INT
		{ $$ = build_basic_type( TypeData::Int ); }
	| INT128
		{ $$ = build_basic_type( TypeData::Int128 ); }
	| UINT128
		{ $$ = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
	| FLOAT
		{ $$ = build_basic_type( TypeData::Float ); }
	| DOUBLE
		{ $$ = build_basic_type( TypeData::Double ); }
	| uuFLOAT80
		{ $$ = build_basic_type( TypeData::uuFloat80 ); }
	| uuFLOAT128
		{ $$ = build_basic_type( TypeData::uuFloat128 ); }
	| uFLOAT16
		{ $$ = build_basic_type( TypeData::uFloat16 ); }
	| uFLOAT32
		{ $$ = build_basic_type( TypeData::uFloat32 ); }
	| uFLOAT32X
		{ $$ = build_basic_type( TypeData::uFloat32x ); }
	| uFLOAT64
		{ $$ = build_basic_type( TypeData::uFloat64 ); }
	| uFLOAT64X
		{ $$ = build_basic_type( TypeData::uFloat64x ); }
	| uFLOAT128
		{ $$ = build_basic_type( TypeData::uFloat128 ); }
	| DECIMAL32
		{ SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); $$ = nullptr; }
	| DECIMAL64
		{ SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); $$ = nullptr; }
	| DECIMAL128
		{ SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); $$ = nullptr; }
	| COMPLEX											// C99
		{ $$ = build_complex_type( TypeData::Complex ); }
	| IMAGINARY											// C99
		{ $$ = build_complex_type( TypeData::Imaginary ); }
	| SIGNED
		{ $$ = build_signedness( TypeData::Signed ); }
	| UNSIGNED
		{ $$ = build_signedness( TypeData::Unsigned ); }
	| SHORT
		{ $$ = build_length( TypeData::Short ); }
	| LONG
		{ $$ = build_length( TypeData::Long ); }
	| VA_LIST											// GCC, __builtin_va_list
		{ $$ = build_builtin_type( TypeData::Valist ); }
	| AUTO_TYPE
		{ $$ = build_builtin_type( TypeData::AutoType ); }
	| vtable
	;

vtable_opt:
	// empty
		{ $$ = nullptr; }
	| vtable
	;

vtable:
	VTABLE '(' type_name ')' default_opt
		{ $$ = build_vtable_type( $3 ); }
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
		{ $$ = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $3 ) ) ), true ); }
	| BASETYPEOF '(' comma_expression ')'				// CFA: basetypeof( a+b ) y;
		{ $$ = DeclarationNode::newTypeof( $3, true ); }
	| ZERO_T											// CFA
		{ $$ = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
	| ONE_T												// CFA
		{ $$ = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
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
		{ $$ = DeclarationNode::newFromTypeData( $1 ); }
	| type_qualifier_list type_name
		{ $$ = DeclarationNode::newFromTypeData( $2 )->addQualifiers( $1 ); }
	| type_type_specifier type_qualifier
		{ $$ = $1->addQualifiers( $2 ); }
	;

type_name:
	TYPEDEFname
		{ $$ = build_typedef( $1 ); }
	| '.' TYPEDEFname
		{ $$ = build_qualified_type( build_global_scope(), build_typedef( $2 ) ); }
	| type_name '.' TYPEDEFname
		{ $$ = build_qualified_type( $1, build_typedef( $3 ) ); }
	| typegen_name
	| '.' typegen_name
		{ $$ = build_qualified_type( build_global_scope(), $2 ); }
	| type_name '.' typegen_name
		{ $$ = build_qualified_type( $1, $3 ); }
	;

typegen_name:											// CFA
	TYPEGENname
		{ $$ = build_type_gen( $1, nullptr ); }
	| TYPEGENname '(' ')'
		{ $$ = build_type_gen( $1, nullptr ); }
	| TYPEGENname '(' type_list ')'
		{ $$ = build_type_gen( $1, $3 ); }
	;

elaborated_type:										// struct, union, enum
	aggregate_type
	| enum_type
	;

elaborated_type_nobody:									// struct, union, enum - {...}
	aggregate_type_nobody
	| enum_type_nobody
	;

// ************************** AGGREGATE *******************************

aggregate_type:											// struct, union
	aggregate_key attribute_list_opt
		{ forall = false; }								// reset
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{ $$ = DeclarationNode::newAggregate( $1, nullptr, $7, $5, true )->addQualifiers( $2 ); }
	| aggregate_key attribute_list_opt identifier
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{
			$$ = DeclarationNode::newAggregate( $1, $3, $8, $6, true )->addQualifiers( $2 );
		}
	| aggregate_key attribute_list_opt TYPEDEFname		// unqualified type name
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{
			DeclarationNode::newFromTypeData( build_typedef( $3 ) );
			$$ = DeclarationNode::newAggregate( $1, $3, $8, $6, true )->addQualifiers( $2 );
		}
	| aggregate_key attribute_list_opt TYPEGENname		// unqualified type name
		{
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
	  '{' field_declaration_list_opt '}' type_parameters_opt
		{
			DeclarationNode::newFromTypeData( build_type_gen( $3, nullptr ) );
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
			typedefTable.makeTypedef( *$3, forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			$$ = DeclarationNode::newAggregate( $1, $3, nullptr, nullptr, false )->addQualifiers( $2 );
		}
	| aggregate_key attribute_list_opt type_name
		{
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( $3->kind == TypeData::SymbolicInst && ! $3->symbolic.isTypedef ) {
				$$ = DeclarationNode::newFromTypeData( $3 )->addQualifiers( $2 );
			} else {
				$$ = DeclarationNode::newAggregate( $1, $3->symbolic.name, $3->symbolic.actuals, nullptr, false )->addQualifiers( $2 );
				$3->symbolic.name = nullptr;			// copied to $$
				$3->symbolic.actuals = nullptr;
				delete $3;
			}
		}
	;

aggregate_key:
	aggregate_data
	| aggregate_control
	;

aggregate_data:
	STRUCT vtable_opt
		{ $$ = ast::AggregateDecl::Struct; }
	| UNION
		{ $$ = ast::AggregateDecl::Union; }
	| EXCEPTION											// CFA
		{ $$ = ast::AggregateDecl::Exception; }
	;

aggregate_control:										// CFA
	MONITOR
		{ $$ = ast::AggregateDecl::Monitor; }
	| MUTEX STRUCT
		{ $$ = ast::AggregateDecl::Monitor; }
	| GENERATOR
		{ $$ = ast::AggregateDecl::Generator; }
	| MUTEX GENERATOR
		{
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			$$ = ast::AggregateDecl::NoAggregate;
		}
	| COROUTINE
		{ $$ = ast::AggregateDecl::Coroutine; }
	| MUTEX COROUTINE
		{
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			$$ = ast::AggregateDecl::NoAggregate;
		}
	| THREAD
		{ $$ = ast::AggregateDecl::Thread; }
	| MUTEX THREAD
		{
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			$$ = ast::AggregateDecl::NoAggregate;
		}
	;

field_declaration_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_declaration_list_opt field_declaration
		{ $$ = $1 ? $1->set_last( $2 ) : $2; }
	;

field_declaration:
	type_specifier field_declaring_list_opt ';'
		{
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			$$ = fieldDecl( $1, $2 );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
	| type_specifier field_declaring_list_opt '}'		// invalid syntax rule
		{
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			$$ = nullptr;
		}
	| EXTENSION type_specifier field_declaring_list_opt ';'	// GCC
		{ $$ = fieldDecl( $2, $3 ); distExt( $$ ); }
	| STATIC type_specifier field_declaring_list_opt ';' // CFA
		{ SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); $$ = nullptr; }
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
	| static_assert ';'									// C11
	;

field_declaring_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_declarator
	| field_declaring_list_opt ',' attribute_list_opt field_declarator
		{ $$ = $1->set_last( $4->addQualifiers( $3 ) ); }
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
	| function_type_redeclarator bit_subrange_size_opt
		// A semantic check is required to ensure bit_subrange only appears on integral types.
		{ $$ = $1->addBitfield( $2 ); }
	;

field_abstract_list_opt:
	// empty
		{ $$ = nullptr; }
	| field_abstract
	| field_abstract_list_opt ',' attribute_list_opt field_abstract
		{ $$ = $1->set_last( $4->addQualifiers( $3 ) ); }
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
		{ $$ = $1->set_last( $1->cloneType( $3 ) ); }
	;

cfa_field_abstract_list:								// CFA, new style field declaration
	// bit-fields are handled by C declarations
	cfa_abstract_declarator_tuple
	| cfa_field_abstract_list ','
		{ $$ = $1->set_last( $1->cloneType( 0 ) ); }
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

// ************************** ENUMERATION *******************************

enum_type:
		// anonymous, no type name 
	ENUM attribute_list_opt hide_opt '{' enumerator_list comma_opt '}'
		{
			if ( $3 == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); $$ = nullptr;
			} // if
			$$ = DeclarationNode::newEnum( nullptr, $5, true, false )->addQualifiers( $2 );
		}
	| ENUM enumerator_type attribute_list_opt hide_opt '{' enumerator_list comma_opt '}'
		{
			if ( $2 && ($2->storageClasses.val != 0 || $2->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( $4 == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); $$ = nullptr;
			} // if
			$$ = DeclarationNode::newEnum( nullptr, $6, true, true, $2 )->addQualifiers( $3 );
		}

		// named type
	| ENUM attribute_list_opt identifier
		{ typedefTable.makeTypedef( *$3, "enum_type 1" ); }
	  hide_opt '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( $3, $7, true, false, nullptr, $5 )->addQualifiers( $2 ); }
	| ENUM attribute_list_opt typedef_name hide_opt '{' enumerator_list comma_opt '}' // unqualified type name
		{ $$ = DeclarationNode::newEnum( $3->name, $6, true, false, nullptr, $4 )->addQualifiers( $2 ); }
	| ENUM enumerator_type attribute_list_opt identifier attribute_list_opt
		{
			if ( $2 && ($2->storageClasses.any() || $2->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *$4, "enum_type 2" );
		}
	  hide_opt '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( $4, $9, true, true, $2, $7 )->addQualifiers( $3 )->addQualifiers( $5 ); }
	| ENUM enumerator_type attribute_list_opt typedef_name attribute_list_opt hide_opt '{' enumerator_list comma_opt '}'
		{ $$ = DeclarationNode::newEnum( $4->name, $8, true, true, $2, $6 )->addQualifiers( $3 )->addQualifiers( $5 ); }

		// forward declaration
	| enum_type_nobody
	;

enumerator_type:
	'(' ')'												// pure enumeration
		{ $$ = nullptr; }
	| '(' cfa_abstract_parameter_declaration ')'		// typed enumeration
		{ $$ = $2; }
	;

hide_opt:
	// empty
		{ $$ = EnumHiding::Visible; }
	| '!'
		{ $$ = EnumHiding::Hide; }
	;

enum_type_nobody:										// enum - {...}
	ENUM attribute_list_opt identifier
		{
			typedefTable.makeTypedef( *$3, "enum_type_nobody 1" );
			$$ = DeclarationNode::newEnum( $3, nullptr, false, false )->addQualifiers( $2 );
		}
	| ENUM attribute_list_opt type_name
		{
			typedefTable.makeTypedef( *$3->symbolic.name, "enum_type_nobody 2" );
			$$ = DeclarationNode::newEnum( $3->symbolic.name, nullptr, false, false )->addQualifiers( $2 );
		}
	;

enumerator_list:
	// empty
		{ SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  $$ = nullptr; }
	| visible_hide_opt identifier_or_type_name enumerator_value_opt
		{ $$ = DeclarationNode::newEnumValueGeneric( $2, $3 ); }
	| INLINE type_name
		{
			$$ = DeclarationNode::newEnumInLine( $2->symbolic.name );
			$2->symbolic.name = nullptr;
			delete $2;
		}
	| enumerator_list ',' visible_hide_opt identifier_or_type_name enumerator_value_opt
		{ $$ = $1->set_last( DeclarationNode::newEnumValueGeneric( $4, $5 ) ); }
	| enumerator_list ',' INLINE type_name
		{ $$ = $1->set_last( DeclarationNode::newEnumInLine( $4->symbolic.name )  ); }
	;

visible_hide_opt:
	hide_opt
	| '^'
		{ $$ = EnumHiding::Visible; }
	;

enumerator_value_opt:
	// empty
		{ $$ = nullptr; }
	| '=' constant_expression					{ $$ = new InitializerNode( $2 ); }
	| '=' '{' initializer_list_opt comma_opt '}' { $$ = new InitializerNode( $3, true ); }
	// | simple_assignment_operator initializer
	// 	{ $$ = $1 == OperKinds::Assign ? $2 : $2->set_maybeConstructed( false ); }
	;

// ************************** FUNCTION PARAMETERS *******************************

parameter_list_ellipsis_opt:
	// empty
		{ $$ = nullptr; }
	| ELLIPSIS
		{ $$ = nullptr; }
	| parameter_list
	| parameter_list ',' ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	;

parameter_list:											// abstract + real
	parameter_declaration
	| abstract_parameter_declaration
	| parameter_list ',' parameter_declaration
		{ $$ = $1->set_last( $3 ); }
	| parameter_list ',' abstract_parameter_declaration
		{ $$ = $1->set_last( $3 ); }
	;

cfa_parameter_list_ellipsis_opt:						// CFA, abstract + real
	// empty
		{ $$ = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
	| ELLIPSIS
		{ $$ = nullptr; }
	| cfa_parameter_list
	| cfa_abstract_parameter_list
	| cfa_parameter_list ',' cfa_abstract_parameter_list
		{ $$ = $1->set_last( $3 ); }
	| cfa_parameter_list ',' ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	| cfa_abstract_parameter_list ',' ELLIPSIS
		{ $$ = $1->addVarArgs(); }
	;

cfa_parameter_list:										// CFA
		// To obtain LR(1) between cfa_parameter_list and cfa_abstract_tuple, the last cfa_abstract_parameter_list is
		// factored out from cfa_parameter_list, flattening the rules to get lookahead to the ']'.
	cfa_parameter_declaration
	| cfa_abstract_parameter_list ',' cfa_parameter_declaration
		{ $$ = $1->set_last( $3 ); }
	| cfa_parameter_list ',' cfa_parameter_declaration
		{ $$ = $1->set_last( $3 ); }
	| cfa_parameter_list ',' cfa_abstract_parameter_list ',' cfa_parameter_declaration
		{ $$ = $1->set_last( $3 )->set_last( $5 ); }
	;

cfa_abstract_parameter_list:							// CFA, new & old style abstract
	cfa_abstract_parameter_declaration
	| cfa_abstract_parameter_list ',' cfa_abstract_parameter_declaration
		{ $$ = $1->set_last( $3 ); }
	;

// Provides optional identifier names (abstract_declarator/variable_declarator), no initialization, different semantics
// for typedef name by using type_parameter_redeclarator instead of typedef_redeclarator, and function prototypes.

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

cfa_parameter_declaration:								// CFA, new & old style parameter declaration
	parameter_declaration
	| cfa_identifier_parameter_declarator_no_tuple identifier_or_type_name default_initializer_opt
		{ $$ = $1->addName( $2 ); }
	| cfa_abstract_tuple identifier_or_type_name default_initializer_opt
		// To obtain LR(1), these rules must be duplicated here (see cfa_abstract_declarator).
		{ $$ = $1->addName( $2 ); }
	| type_qualifier_list cfa_abstract_tuple identifier_or_type_name default_initializer_opt
		{ $$ = $2->addName( $3 )->addQualifiers( $1 ); }
	| cfa_function_specifier							// int f( "int fp()" );
	;

cfa_abstract_parameter_declaration:						// CFA, new & old style parameter declaration
	abstract_parameter_declaration
	| cfa_identifier_parameter_declarator_no_tuple
	| cfa_abstract_tuple
		// To obtain LR(1), these rules must be duplicated here (see cfa_abstract_declarator).
	| type_qualifier_list cfa_abstract_tuple
		{ $$ = $2->addQualifiers( $1 ); }
	| cfa_abstract_function								// int f( "int ()" );
	;

// ISO/IEC 9899:1999 Section 6.9.1(6) : "An identifier declared as a typedef name shall not be redeclared as a
// parameter." Because the scope of the K&R-style parameter-list sees the typedef first, the following is based only on
// identifiers.  The ANSI-style parameter-list can redefine a typedef name.

identifier_list:										// K&R-style parameter list => no types
	identifier
		{ $$ = DeclarationNode::newName( $1 ); }
	| identifier_list ',' identifier
		{ $$ = $1->set_last( DeclarationNode::newName( $3 ) ); }
	;

type_no_function:										// sizeof, alignof, cast (constructor)
	cfa_abstract_declarator_tuple						// CFA
	| type_specifier									// cannot be type_specifier_nobody, e.g., (struct S {}){} is a thing
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
	| initializer_list_opt ',' initializer		{ $$ = $1->set_last( $3 ); }
	| initializer_list_opt ',' designation initializer { $$ = $1->set_last( $4->set_designators( $3 ) ); }
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
		{ $$ = new ExpressionNode( build_varref( yylloc, $1 ) ); }
	;

designator_list:										// C99
	designator
	| designator_list designator
		{ $$ = $1->set_last( $2 ); }
	//| designator_list designator						{ $$ = new ExpressionNode( $1, $2 ); }
	;

designator:
	'.' identifier_at									// C99, field name
		{ $$ = new ExpressionNode( build_varref( yylloc, $2 ) ); }
	| '[' push assignment_expression pop ']'			// C99, single array element
		// assignment_expression used instead of constant_expression because of shift/reduce conflicts with tuple.
		{ $$ = $3; }
	| '[' push subrange pop ']'							// CFA, multiple array elements
		{ $$ = $3; }
	| '[' push constant_expression ELLIPSIS constant_expression pop ']' // GCC, multiple array elements
		{ $$ = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( $3 ), maybeMoveBuild( $5 ) ) ); }
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
		{ $$ = $1->set_last( $3 ); }
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
			typedefTable.addToScope( *$2, TYPEDEFname, "type_parameter 1" );
			if ( $1 == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( $1 == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( $1 == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
	  type_initializer_opt assertion_list_opt
		{ $$ = DeclarationNode::newTypeParam( $1, $2 )->addTypeInitializer( $4 )->addAssertions( $5 ); }
	| identifier_or_type_name new_type_class
		{ typedefTable.addToScope( *$1, TYPEDEFname, "type_parameter 2" ); }
	  type_initializer_opt assertion_list_opt
		{ $$ = DeclarationNode::newTypeParam( $2, $1 )->addTypeInitializer( $4 )->addAssertions( $5 ); }
	| '[' identifier_or_type_name ']'
		{
			typedefTable.addToScope( *$2, TYPEDIMname, "type_parameter 3" );
			$$ = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, $2 );
		}
	// | type_specifier identifier_parameter_declarator
	| assertion_list
		{ $$ = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( $1 ); }
	| ENUM '(' identifier_or_type_name ')' identifier_or_type_name new_type_class type_initializer_opt assertion_list_opt
		{	
			typedefTable.addToScope( *$3, TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *$5, TYPEDIMname, "type_parameter 5" );
			$$ = DeclarationNode::newTypeParam( $6, $5 )->addTypeInitializer( $7 )->addAssertions( $8 );
		}
	;

new_type_class:											// CFA
	// empty
		{ $$ = ast::TypeDecl::Otype; }
	| '&'
		{ $$ = ast::TypeDecl::Dtype; }
	| '*'
		{ $$ = ast::TypeDecl::DStype; }					// Dtype + sized
	// | '(' '*' ')'									// Gregor made me do it
	//  	{ $$ = ast::TypeDecl::Ftype; }
	| ELLIPSIS
		{ $$ = ast::TypeDecl::Ttype; }
	;

type_class:												// CFA
	OTYPE
		{ $$ = ast::TypeDecl::Otype; }
	| DTYPE
		{ $$ = ast::TypeDecl::Dtype; }
	| FTYPE
		{ $$ = ast::TypeDecl::Ftype; }
	| TTYPE
		{ $$ = ast::TypeDecl::Ttype; }
	;

assertion_list_opt:										// CFA
	// empty
		{ $$ = nullptr; }
	| assertion_list
	;

assertion_list:											// CFA
	assertion
	| assertion_list assertion
		{ $$ = $1->set_last( $2 ); }
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
		{ $$ = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $1 ) ) ); }
	| assignment_expression
	| type_list ',' type
		{ $$ = $1->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $3 ) ) ) ); }
	| type_list ',' assignment_expression
		{ $$ = $1->set_last( $3 ); }
	;

type_declaring_list:									// CFA
	OTYPE type_declarator
		{ $$ = $2; }
	| storage_class_list OTYPE type_declarator
		{ $$ = $3->addQualifiers( $1 ); }
	| type_declaring_list ',' type_declarator
		{ $$ = $1->set_last( $3->copySpecifiers( $1 ) ); }
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
			typedefTable.addToEnclosingScope( *$1, TYPEDEFname, "type_declarator_name 1" );
			$$ = DeclarationNode::newTypeDecl( $1, nullptr );
		}
	| identifier_or_type_name '(' type_parameter_list ')'
		{
			typedefTable.addToEnclosingScope( *$1, TYPEGENname, "type_declarator_name 2" );
			$$ = DeclarationNode::newTypeDecl( $1, $3 );
		}
	;

trait_specifier:										// CFA
	TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' '}'
		{
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			$$ = DeclarationNode::newTrait( $2, $4, nullptr );
		}
	| forall TRAIT identifier_or_type_name '{' '}'		// alternate
		{ $$ = DeclarationNode::newTrait( $3, $1, nullptr ); }
	| TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' push trait_declaration_list pop '}'
		{
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			$$ = DeclarationNode::newTrait( $2, $4, $8 );
		}
	| forall TRAIT identifier_or_type_name '{' push trait_declaration_list pop '}' // alternate
		{ $$ = DeclarationNode::newTrait( $3, $1, $6 ); }
	;

trait_declaration_list:									// CFA
	trait_declaration
	| trait_declaration_list pop push trait_declaration
		{ $$ = $1->set_last( $4 ); }
	;

trait_declaration:										// CFA
	cfa_trait_declaring_list ';'
	| trait_declaring_list ';'
	;

cfa_trait_declaring_list:								// CFA
	cfa_variable_specifier
	| cfa_function_specifier
	| cfa_trait_declaring_list pop ',' push identifier_or_type_name
		{ $$ = $1->set_last( $1->cloneType( $5 ) ); }
	;

trait_declaring_list:									// CFA
	type_specifier declarator
		{ $$ = $2->addType( $1 ); }
	| trait_declaring_list pop ',' push declarator
		{ $$ = $1->set_last( $1->cloneBaseType( $5 ) ); }
	;

// **************************** EXTERNAL DEFINITIONS *****************************

translation_unit:
	// empty, input file
	| external_definition_list
		{ parseTree = parseTree ? parseTree->set_last( $1 ) : $1; }
	;

external_definition_list:
	push external_definition pop
		{ $$ = $2; }
	| external_definition_list push external_definition pop
		{ $$ = $1 ? $1->set_last( $3 ) : $3; }
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
		{ $$ = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, $1 ) ) ); }
	| declaration
		{
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( $1->linkage == ast::Linkage::Cforall && ! $1->storageClasses.is_static &&
				 $1->type && $1->type->kind == TypeData::AggregateInst ) {
				if ( $1->type->aggInst.aggregate->aggregate.anon ) {
					SemanticError( yylloc, "extern anonymous aggregate is currently unimplemented." ); $$ = nullptr;
				}
			}
		}
	| IDENTIFIER IDENTIFIER
		{ IdentifierBeforeIdentifier( *$1.str, *$2.str, " declaration" ); $$ = nullptr; }
	| IDENTIFIER type_qualifier							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type qualifier" ); $$ = nullptr; }
	| IDENTIFIER storage_class							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "storage class" ); $$ = nullptr; }
	| IDENTIFIER basic_type_name						// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	| IDENTIFIER TYPEDEFname							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	| IDENTIFIER TYPEGENname							// invalid syntax rule
		{ IdentifierBeforeType( *$1.str, "type" ); $$ = nullptr; }
	| external_function_definition
	| EXTENSION external_definition						// GCC, multiple __extension__ allowed, meaning unknown
		{
			distExt( $2 );								// mark all fields in list
			$$ = $2;
		}
	| ASM '(' string_literal ')' ';'					// GCC, global assembler statement
		{ $$ = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, $3, nullptr ) ) ); }
	| EXTERN STRINGliteral
		{
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, $2 );
		}
	  up external_definition down
		{
			linkage = linkageStack.top();
			linkageStack.pop();
			$$ = $5;
		}
	| EXTERN STRINGliteral								// C++-style linkage specifier
		{
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, $2 );
		}
	  '{' up external_definition_list_opt down '}'
		{
			linkage = linkageStack.top();
			linkageStack.pop();
			$$ = $6;
		}
		// global distribution
	| type_qualifier_list
		{
			if ( $1->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
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
			if ( $1->type && $1->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
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
			if ( ($1->type && $1->type->qualifiers.any()) || ($2->type && $2->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ($1->type && $1->type->forall) || ($2->type && $2->type->forall) ) forall = true; // remember generic type
		}
	  '{' up external_definition_list_opt down '}'		// CFA, namespace
		{
			distQual( $6, $1->addQualifiers( $2 ) );
			forall = false;
			$$ = $6;
		}
	| ';'												// empty declaration
		{ $$ = nullptr; }
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
	| WITH '(' type_list ')' attribute_list_opt			// support scoped enumeration
		{
			$$ = $3; forall = false;
			if ( $5 ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				$$ = nullptr;
			} // if
		}
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
	| declaration_specifier function_type_redeclarator with_clause_opt compound_statement
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
	| function_type_redeclarator
	;

subrange:
	constant_expression '~' constant_expression			// CFA, integer subrange
		{ $$ = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( $1 ), maybeMoveBuild( $3 ) ) ); }
	;

// **************************** ASM *****************************

asm_name_opt:											// GCC
	// empty
		{ $$ = nullptr; }
	| ASM '(' string_literal ')' attribute_list_opt
		{
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( $3 );
			$$ = name->addQualifiers( $5 );
		}
	;

// **************************** ATTRIBUTE *****************************

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
	| ATTRIBUTE '(' attribute_name_list ')'				// CFA
		{ $$ = $3; }
	| ATTR '(' attribute_name_list ')'					// CFA
		{ $$ = $3; }
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
	identifier_or_type_name
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
	| '?' identifier
		// { SemanticError( yylloc, "keyword parameter is currently unimplemented." ); $$ = nullptr; }
		{ $$ = DeclarationNode::newName( $2 ); }
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
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	| '(' variable_array ')' multi_array_dimension		// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list variable_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' variable_array ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list variable_array ')'				// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

variable_function:
	'(' variable_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
	| '(' attribute_list variable_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $3->addQualifiers( $2 )->addParamList( $6 ); }
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
	paren_identifier '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $3 ); }
	| '(' function_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $2->addParamList( $5 ); }
	| '(' attribute_list function_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $3->addQualifiers( $2 )->addParamList( $6 ); }
	| '(' function_no_ptr ')'							// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list function_no_ptr ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

function_ptr:
	ptrref_operator function_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	| '(' KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $2->addParamList( $5 ); }
	| '(' attribute_list KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $3->addQualifiers( $2 )->addParamList( $6 ); }
	| '(' KR_function_no_ptr ')'						// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list KR_function_no_ptr ')'			// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

KR_function_ptr:
	ptrref_operator KR_function_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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

// This pattern parses a declaration for a variable that redefines a type name, e.g.:
//
//		typedef int foo;
//		{
//		   int foo; // redefine typedef name in new scope
//		}

paren_type:
	typedef_name
		{
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *$1->name, IDENTIFIER, "paren_type" );
		}
	| '(' paren_type ')'
		{ $$ = $2; }
	;

variable_type_redeclarator:
	paren_type attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_type_ptr
	| variable_type_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_type_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

variable_type_ptr:
	ptrref_operator variable_type_redeclarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| ptrref_operator type_qualifier_list variable_type_redeclarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' variable_type_ptr ')' attribute_list_opt		// redundant parenthesis
		{ $$ = $2->addQualifiers( $4 ); }
	| '(' attribute_list variable_type_ptr ')' attribute_list_opt // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addQualifiers( $5 ); }
	;

variable_type_array:
	paren_type array_dimension
		{ $$ = $1->addArray( $2 ); }
	| '(' variable_type_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list variable_type_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' variable_type_array ')' multi_array_dimension	// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list variable_type_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' variable_type_array ')'						// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list variable_type_array ')'		// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

variable_type_function:
	'(' variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
	| '(' attribute_list variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $3->addQualifiers( $2 )->addParamList( $6 ); }
	| '(' variable_type_function ')'					// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list variable_type_function ')'		// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

// This pattern parses a declaration for a function prototype that redefines a type name.  It precludes declaring an
// array of functions versus a pointer to an array of functions, and returning arrays and functions versus pointers to
// arrays and functions.

function_type_redeclarator:
	function_type_no_ptr attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| function_type_ptr
	| function_type_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

function_type_no_ptr:
	paren_type '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $3 ); }
	| '(' function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $2->addParamList( $5 ); }
	| '(' attribute_list function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'
		{ $$ = $3->addQualifiers( $2 )->addParamList( $6 ); }
	| '(' function_type_no_ptr ')'						// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list function_type_no_ptr ')'		// redundant parenthesis
		{ $$ = $3->addQualifiers( $2 ); }
	;

function_type_ptr:
	ptrref_operator function_type_redeclarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| ptrref_operator type_qualifier_list function_type_redeclarator
		{ $$ = $3->addPointer( DeclarationNode::newPointer( $2, $1 ) ); }
	| '(' function_type_ptr ')' attribute_list_opt
		{ $$ = $2->addQualifiers( $4 ); }
	| '(' attribute_list function_type_ptr ')' attribute_list_opt
		{ $$ = $3->addQualifiers( $2 )->addQualifiers( $5 ); }
	;

function_type_array:
	'(' function_type_ptr ')' array_dimension
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list function_type_ptr ')' array_dimension
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' function_type_array ')' multi_array_dimension	// redundant parenthesis
		{ $$ = $2->addArray( $4 ); }
	| '(' attribute_list function_type_array ')' multi_array_dimension // redundant parenthesis
		{ $$ = $3->addQualifiers( $2 )->addArray( $5 ); }
	| '(' function_type_array ')'						// redundant parenthesis
		{ $$ = $2; }
	| '(' attribute_list function_type_array ')'		// redundant parenthesis
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
		{ $$ = $3->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( $4 ); }
	| identifier_parameter_ptr
	| identifier_parameter_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| identifier_parameter_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

identifier_parameter_ptr:
	ptrref_operator identifier_parameter_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	paren_identifier '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $3 ); }
	| '(' identifier_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
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
		{ $$ = $3->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( $4 ); }
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
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	typedef_name '(' parameter_list_ellipsis_opt ')'	// empty parameter list OBSOLESCENT (see 3)
		{ $$ = $1->addParamList( $3 ); }
	| '(' type_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
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
		{ $$ = DeclarationNode::newPointer( nullptr, $1 ); }
	| ptrref_operator type_qualifier_list
		{ $$ = DeclarationNode::newPointer( $2, $1 ); }
	| ptrref_operator abstract_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	'(' parameter_list_ellipsis_opt ')'					// empty parameter list OBSOLESCENT (see 3)
		{ $$ = DeclarationNode::newFunction( nullptr, nullptr, $2, nullptr ); }
	| '(' abstract_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
	| '(' abstract_function ')'							// redundant parenthesis
		{ $$ = $2; }
	;

array_dimension:
		// Only the first dimension can be empty.
	'[' ']'
		{ $$ = DeclarationNode::newArray( nullptr, nullptr, false ); }
	| '[' ']' multi_array_dimension
		{ $$ = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( $3 ); }
		// Cannot use constant_expression because of tuples => semantic check
	| '[' push assignment_expression pop ',' comma_expression ']' // CFA
		{ $$ = DeclarationNode::newArray( $3, nullptr, false )->addArray( DeclarationNode::newArray( $6, nullptr, false ) ); }
		// { SemanticError( yylloc, "New array dimension is currently unimplemented." ); $$ = nullptr; }

		// If needed, the following parses and does not use comma_expression, so the array structure can be built.
	// | '[' push assignment_expression pop ',' push array_dimension_list pop ']' // CFA

	| '[' push array_type_list pop ']'					// CFA
		{ $$ = DeclarationNode::newArray( $3, nullptr, false ); }
	| multi_array_dimension
	;

// array_dimension_list:
// 	assignment_expression
// 	| array_dimension_list ',' assignment_expression
// 	;

array_type_list:
	basic_type_name
		{ $$ = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $1 ) ) ); }
	| type_name
		{ $$ = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $1 ) ) ); }
	| assignment_expression upupeq assignment_expression
	| array_type_list ',' basic_type_name
		{ $$ = $1->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $3 ) ) ) ); }
	| array_type_list ',' type_name
		{ $$ = $1->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( $3 ) ) ) ); }
	| array_type_list ',' assignment_expression upupeq assignment_expression
	;

upupeq:
	'~'
		{ $$ = OperKinds::LThan; }
	| ErangeUpEq
		{ $$ = OperKinds::LEThan; }
	;

multi_array_dimension:
	'[' push assignment_expression pop ']'
		{ $$ = DeclarationNode::newArray( $3, nullptr, false ); }
	| '[' push '*' pop ']'								// C99
		{ $$ = DeclarationNode::newVarArray( 0 ); }
	| multi_array_dimension '[' push assignment_expression pop ']'
		{ $$ = $1->addArray( DeclarationNode::newArray( $4, nullptr, false ) ); }
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
// and functions versus pointers to arrays and functions. In addition, the pattern handles the special meaning of
// parenthesis around a typedef name:
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

abstract_parameter_declarator_opt:
	// empty
		{ $$ = nullptr; }
	| abstract_parameter_declarator
	;

abstract_parameter_declarator:
	abstract_parameter_ptr
	| '&' MUTEX attribute_list_opt
		{ $$ = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( $3 ); }
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
	'(' parameter_list_ellipsis_opt ')'					// empty parameter list OBSOLESCENT (see 3)
		{ $$ = DeclarationNode::newFunction( nullptr, nullptr, $2, nullptr ); }
	| '(' abstract_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
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
		{ $$ = DeclarationNode::newArray( nullptr, nullptr, false ); }
		// multi_array_dimension handles the '[' '*' ']' case
	| '[' push type_qualifier_list '*' pop ']'			// remaining C99
		{ $$ = DeclarationNode::newVarArray( $3 ); }
	| '[' push type_qualifier_list pop ']'
		{ $$ = DeclarationNode::newArray( nullptr, $3, false ); }
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
//   struct S {
//       int;
//       int *;
//       int [10];
//       int (*)();
//   };

variable_abstract_declarator:
	variable_abstract_ptr
	| variable_abstract_array attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	| variable_abstract_function attribute_list_opt
		{ $$ = $1->addQualifiers( $2 ); }
	;

variable_abstract_ptr:
	ptrref_operator
		{ $$ = DeclarationNode::newPointer( nullptr, $1 ); }
	| ptrref_operator type_qualifier_list
		{ $$ = DeclarationNode::newPointer( $2, $1 ); }
	| ptrref_operator variable_abstract_declarator
		{ $$ = $2->addPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	'(' variable_abstract_ptr ')' '(' parameter_list_ellipsis_opt ')' // empty parameter list OBSOLESCENT (see 3)
		{ $$ = $2->addParamList( $5 ); }
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
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| type_qualifier_list ptrref_operator type_specifier_nobody
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_function
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_abstract_function
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_identifier_parameter_declarator_tuple
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_identifier_parameter_declarator_tuple
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	;

cfa_identifier_parameter_array:							// CFA
		// Only the first dimension can be empty or have qualifiers. Empty dimension must be factored out due to
		// shift/reduce conflict with new-style empty (void) function return type.
	'[' ']' type_specifier_nobody
		{ $$ = $3->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| cfa_array_parameter_1st_dimension type_specifier_nobody
		{ $$ = $2->addNewArray( $1 ); }
	| '[' ']' multi_array_dimension type_specifier_nobody
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| cfa_array_parameter_1st_dimension multi_array_dimension type_specifier_nobody
		{ $$ = $3->addNewArray( $2 )->addNewArray( $1 ); }
	| multi_array_dimension type_specifier_nobody
		{ $$ = $2->addNewArray( $1 ); }

	| '[' ']' cfa_identifier_parameter_ptr
		{ $$ = $3->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
	| cfa_array_parameter_1st_dimension cfa_identifier_parameter_ptr
		{ $$ = $2->addNewArray( $1 ); }
	| '[' ']' multi_array_dimension cfa_identifier_parameter_ptr
		{ $$ = $4->addNewArray( $3 )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
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
//		'[' cfa_parameter_list ']' identifier_or_type_name '(' cfa_parameter_list_ellipsis_opt ')'
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
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| type_qualifier_list ptrref_operator type_specifier
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_function
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
	| type_qualifier_list ptrref_operator cfa_abstract_function
		{ $$ = $3->addNewPointer( DeclarationNode::newPointer( $1, $2 ) ); }
	| ptrref_operator cfa_abstract_declarator_tuple
		{ $$ = $2->addNewPointer( DeclarationNode::newPointer( nullptr, $1 ) ); }
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
	'[' ']' '(' cfa_parameter_list_ellipsis_opt ')'
		{ $$ = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), $4, nullptr ); }
	| cfa_abstract_tuple '(' push cfa_parameter_list_ellipsis_opt pop ')'
		{ $$ = DeclarationNode::newFunction( nullptr, $1, $4, nullptr ); }
	| cfa_function_return '(' push cfa_parameter_list_ellipsis_opt pop ')'
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

// ************************ MISCELLANEOUS ********************************

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
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
