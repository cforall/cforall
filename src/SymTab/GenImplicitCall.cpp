//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenImplicitCall.hpp --
//
// Author           : Andrew Beach
// Created On       : Fri Apr 14 14:38:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Fri Apr 14 14:46:00 2023
// Update Count     : 0
//

#include "GenImplicitCall.hpp"

#include "AST/Copy.hpp"                  // for deepCopy
#include "AST/Decl.hpp"                  // for ObjectDecl
#include "AST/Expr.hpp"                  // for ConstantExpr, UntypedExpr,...
#include "AST/Init.hpp"                  // for SingleInit
#include "AST/Inspect.hpp"               // for isUnnamedBitfield
#include "AST/Stmt.hpp"                  // for ExprStmt
#include "AST/Type.hpp"                  // for ArrayType, BasicType, ...
#include "CodeGen/OperatorTable.h"       // for isCtorDtor
#include "Common/UniqueName.h"           // for UniqueName

namespace SymTab {

namespace {

template< typename OutIter >
ast::ptr< ast::Stmt > genCall(
	InitTweak::InitExpander & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, const std::string & fname, OutIter && out,
	const ast::Type * type, const ast::Type * addCast, LoopDirection forward = LoopForward );

/// inserts into out a generated call expression to function fname with arguments dstParam and
/// srcParam. Should only be called with non-array types.
/// optionally returns a statement which must be inserted prior to the containing loop, if
/// there is one
template< typename OutIter >
ast::ptr< ast::Stmt > genScalarCall(
	InitTweak::InitExpander & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, std::string fname, OutIter && out, const ast::Type * type,
	const ast::Type * addCast = nullptr
) {
	bool isReferenceCtorDtor = false;
	if ( dynamic_cast< const ast::ReferenceType * >( type ) && CodeGen::isCtorDtor( fname ) ) {
		// reference constructors are essentially application of the rebind operator.
		// apply & to both arguments, do not need a cast
		fname = "?=?";
		dstParam = new ast::AddressExpr( dstParam );
		addCast = nullptr;
		isReferenceCtorDtor = true;
	}

	// want to be able to generate assignment, ctor, and dtor generically, so fname is one of
	// "?=?", "?{}", or "^?{}"
	ast::UntypedExpr * fExpr = new ast::UntypedExpr( loc, new ast::NameExpr( loc, fname ) );

	if ( addCast ) {
		// cast to T& with qualifiers removed, so that qualified objects can be constructed and
		// destructed with the same functions as non-qualified objects. Unfortunately, lvalue
		// is considered a qualifier - for AddressExpr to resolve, its argument must have an
		// lvalue-qualified type, so remove all qualifiers except lvalue.
		// xxx -- old code actually removed lvalue too...
		ast::ptr< ast::Type > guard = addCast;  // prevent castType from mutating addCast
		ast::ptr< ast::Type > castType = addCast;
		ast::remove_qualifiers(
			castType,
			ast::CV::Const | ast::CV::Volatile | ast::CV::Restrict | ast::CV::Atomic );
			dstParam = new ast::CastExpr{ dstParam, new ast::ReferenceType{ castType } };
	}
	fExpr->args.emplace_back( dstParam );

	ast::ptr<ast::Stmt> listInit = srcParam.buildListInit( fExpr );

	// fetch next set of arguments
	++srcParam;

	// return if adding reference fails -- will happen on default ctor and dtor
	if ( isReferenceCtorDtor && ! srcParam.addReference() ) return listInit;

	std::vector< ast::ptr< ast::Expr > > args = *srcParam;
	splice( fExpr->args, args );

	*out++ = new ast::ExprStmt{ loc, fExpr };

	srcParam.clearArrayIndices();

	return listInit;
}


/// Store in out a loop which calls fname on each element of the array with srcParam and
/// dstParam as arguments. If forward is true, loop goes from 0 to N-1, else N-1 to 0
template< typename OutIter >
void genArrayCall(
	InitTweak::InitExpander & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, const std::string & fname, OutIter && out,
	const ast::ArrayType * array, const ast::Type * addCast = nullptr,
	LoopDirection forward = LoopForward
) {
	static UniqueName indexName( "_index" );

	// for a flexible array member nothing is done -- user must define own assignment
	if ( ! array->dimension ) return;

	if ( addCast ) {
		// peel off array layer from cast
		addCast = strict_dynamic_cast< const ast::ArrayType * >( addCast )->base;
	}

	ast::ptr< ast::Expr > begin, end;
	std::string cmp, update;

	const ast::Expr * dimension = deepCopy( array->dimension );
	if ( forward ) {
		// generate: for ( int i = 0; i < N; ++i )
		begin = ast::ConstantExpr::from_int( loc, 0 );
		end = dimension;
		cmp = "?<?";
		update = "++?";
	} else {
		// generate: for ( int i = N-1; i >= 0; --i )
		begin = ast::UntypedExpr::createCall( loc, "?-?",
			{ dimension, ast::ConstantExpr::from_int( loc, 1 ) } );
		end = ast::ConstantExpr::from_int( loc, 0 );
		cmp = "?>=?";
		update = "--?";
	}

	ast::ptr< ast::DeclWithType > index = new ast::ObjectDecl(
		loc, indexName.newName(), new ast::BasicType( ast::BasicType::SignedInt ),
		new ast::SingleInit( loc, begin ) );
	ast::ptr< ast::Expr > indexVar = new ast::VariableExpr( loc, index );

	ast::ptr< ast::Expr > cond = ast::UntypedExpr::createCall(
		loc, cmp, { indexVar, end } );

	ast::ptr< ast::Expr > inc = ast::UntypedExpr::createCall(
		loc, update, { indexVar } );

	ast::ptr< ast::Expr > dstIndex = ast::UntypedExpr::createCall(
		loc, "?[?]", { dstParam, indexVar } );

	// srcParam must keep track of the array indices to build the source parameter and/or
	// array list initializer
	srcParam.addArrayIndex( indexVar, array->dimension );

	// for stmt's body, eventually containing call
	ast::CompoundStmt * body = new ast::CompoundStmt( loc );
	ast::ptr< ast::Stmt > listInit = genCall(
		srcParam, dstIndex, loc, fname, std::back_inserter( body->kids ), array->base, addCast,
		forward );

	// block containing the stmt and index variable
	ast::CompoundStmt * block = new ast::CompoundStmt( loc );
	block->push_back( new ast::DeclStmt( loc, index ) );
	if ( listInit ) { block->push_back( listInit ); }
	block->push_back( new ast::ForStmt( loc, {}, cond, inc, body ) );

	*out++ = block;
}

template< typename OutIter >
ast::ptr< ast::Stmt > genCall(
	InitTweak::InitExpander & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, const std::string & fname, OutIter && out,
	const ast::Type * type, const ast::Type * addCast, LoopDirection forward
) {
	if ( auto at = dynamic_cast< const ast::ArrayType * >( type ) ) {
		genArrayCall(
			srcParam, dstParam, loc, fname, std::forward< OutIter >(out), at, addCast,
			forward );
		return {};
	} else {
		return genScalarCall(
			srcParam, dstParam, loc, fname, std::forward< OutIter >( out ), type, addCast );
	}
}

} // namespace

ast::ptr< ast::Stmt > genImplicitCall(
	InitTweak::InitExpander & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * obj,
	LoopDirection forward
) {
	// unnamed bit fields are not copied as they cannot be accessed
	if ( isUnnamedBitfield( obj ) ) return {};

	ast::ptr< ast::Type > addCast;
	if ( (fname == "?{}" || fname == "^?{}") && ( ! obj || ( obj && ! obj->bitfieldWidth ) ) ) {
		assert( dstParam->result );
		addCast = dstParam->result;
	}

	std::vector< ast::ptr< ast::Stmt > > stmts;
	genCall(
		srcParam, dstParam, loc, fname, back_inserter( stmts ), obj->type, addCast, forward );

	if ( stmts.empty() ) {
		return {};
	} else if ( stmts.size() == 1 ) {
		const ast::Stmt * callStmt = stmts.front();
		if ( addCast ) {
			// implicitly generated ctor/dtor calls should be wrapped so that later passes are
			// aware they were generated.
			callStmt = new ast::ImplicitCtorDtorStmt( callStmt->location, callStmt );
		}
		return callStmt;
	} else {
		assert( false );
		return {};
	}
}

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //


