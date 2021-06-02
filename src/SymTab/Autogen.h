//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Autogen.h --
//
// Author           : Rob Schluntz
// Created On       : Sun May 17 21:53:34 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 16:38:06 2019
// Update Count     : 16
//

#pragma once

#include <cassert>                // for assert
#include <iterator>               // for back_inserter
#include <string>                 // for string

#include "AST/Decl.hpp"
#include "AST/Eval.hpp"
#include "AST/Expr.hpp"
#include "AST/Init.hpp"
#include "AST/Node.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/UniqueName.h"    // for UniqueName
#include "Common/utility.h"       // for splice
#include "InitTweak/InitTweak.h"  // for InitExpander
#include "SynTree/Constant.h"     // for Constant
#include "SynTree/Declaration.h"  // for DeclarationWithType, ObjectDecl
#include "SynTree/Expression.h"   // for NameExpr, ConstantExpr, UntypedExpr...
#include "SynTree/Type.h"         // for Type, ArrayType, Type::Qualifiers
#include "SynTree/Statement.h"    // for CompoundStmt, DeclStmt, ExprStmt

class CompoundStmt;
class Statement;

namespace SymTab {
	/// Generates assignment operators, constructors, and destructor for aggregate types as required
	void autogenerateRoutines( std::list< Declaration * > &translationUnit );

	/// returns true if obj's name is the empty string and it has a bitfield width
	bool isUnnamedBitfield( ObjectDecl * obj );
	bool isUnnamedBitfield( const ast::ObjectDecl * obj );

	/// generate the type of an assignment function for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genAssignType( Type * paramType, bool maybePolymorphic = true );

	/// generate the type of a default constructor or destructor for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genDefaultType( Type * paramType, bool maybePolymorphic = true );

	ast::FunctionDecl * genDefaultFunc(const CodeLocation loc, const std::string fname, const ast::Type * paramType, bool maybePolymorphic = true);

	/// generate the type of a copy constructor for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genCopyType( Type * paramType, bool maybePolymorphic = true );

	/// Enum for loop direction
	enum LoopDirection { LoopBackward, LoopForward };

	/// inserts into out a generated call expression to function fname with arguments dstParam and srcParam. Intended to be used with generated ?=?, ?{}, and ^?{} calls.
	template< typename OutputIterator >
	Statement * genCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, const std::string & fname, OutputIterator out, Type * type, Type * addCast = nullptr, bool forward = true );

	template< typename OutIter >
	ast::ptr< ast::Stmt > genCall(
		InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam, 
		const CodeLocation & loc, const std::string & fname, OutIter && out, 
		const ast::Type * type, const ast::Type * addCast, LoopDirection forward = LoopForward );

	/// inserts into out a generated call expression to function fname with arguments dstParam and srcParam. Should only be called with non-array types.
	/// optionally returns a statement which must be inserted prior to the containing loop, if there is one
	template< typename OutputIterator >
	Statement * genScalarCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, std::string fname, OutputIterator out, Type * type, Type * addCast = nullptr ) {
		bool isReferenceCtorDtor = false;
		if ( dynamic_cast< ReferenceType * >( type ) && CodeGen::isCtorDtor( fname ) ) {
			// reference constructors are essentially application of the rebind operator.
			// apply & to both arguments, do not need a cast
			fname = "?=?";
			dstParam = new AddressExpr( dstParam );
			addCast = nullptr;
			isReferenceCtorDtor = true;
		}

		// want to be able to generate assignment, ctor, and dtor generically,
		// so fname is either ?=?, ?{}, or ^?{}
		UntypedExpr * fExpr = new UntypedExpr( new NameExpr( fname ) );

		if ( addCast ) {
			// cast to T& with qualifiers removed, so that qualified objects can be constructed
			// and destructed with the same functions as non-qualified objects.
			// unfortunately, lvalue is considered a qualifier. For AddressExpr to resolve, its argument
			// must have an lvalue qualified type, so remove all qualifiers except lvalue. If we ever
			// remove lvalue as a qualifier, this can change to
			//   type->get_qualifiers() = Type::Qualifiers();
			Type * castType = addCast->clone();
			castType->get_qualifiers() -= Type::Qualifiers( Type::Const | Type::Volatile | Type::Restrict | Type::Atomic );
			// castType->set_lvalue( true ); // xxx - might not need this
			dstParam = new CastExpr( dstParam, new ReferenceType( Type::Qualifiers(), castType ) );
		}
		fExpr->args.push_back( dstParam );

		Statement * listInit = srcParam.buildListInit( fExpr );

		// fetch next set of arguments
		++srcParam;

		// return if adding reference fails - will happen on default constructor and destructor
		if ( isReferenceCtorDtor && ! srcParam.addReference() ) {
			delete fExpr;
			return listInit;
		}

		std::list< Expression * > args = *srcParam;
		fExpr->args.splice( fExpr->args.end(), args );

		*out++ = new ExprStmt( fExpr );

		srcParam.clearArrayIndices();

		return listInit;
	}

	/// inserts into out a generated call expression to function fname with arguments dstParam and 
	/// srcParam. Should only be called with non-array types.
	/// optionally returns a statement which must be inserted prior to the containing loop, if 
	/// there is one
	template< typename OutIter >
	ast::ptr< ast::Stmt > genScalarCall( 
		InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam, 
		const CodeLocation & loc, std::string fname, OutIter && out, const ast::Type * type, 
		const ast::Type * addCast = nullptr
	) {
		bool isReferenceCtorDtor = false;
		if ( dynamic_cast< const ast::ReferenceType * >( type ) && CodeGen::isCtorDtor( fname ) ) {
			// reference constructors are essentially application of the rebind operator.
			// apply & to both arguments, do not need a cast
			fname = "?=?";
			dstParam = new ast::AddressExpr{ dstParam };
			addCast = nullptr;
			isReferenceCtorDtor = true;
		}

		// want to be able to generate assignment, ctor, and dtor generically, so fname is one of
		// "?=?", "?{}", or "^?{}"
		ast::UntypedExpr * fExpr = new ast::UntypedExpr{ loc, new ast::NameExpr{ loc, fname } };

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

	/// Store in out a loop which calls fname on each element of the array with srcParam and dstParam as arguments.
	/// If forward is true, loop goes from 0 to N-1, else N-1 to 0
	template< typename OutputIterator >
	void genArrayCall( InitTweak::InitExpander_old & srcParam, Expression *dstParam, const std::string & fname, OutputIterator out, ArrayType *array, Type * addCast = nullptr, bool forward = true ) {
		static UniqueName indexName( "_index" );

		// for a flexible array member nothing is done -- user must define own assignment
		if ( ! array->get_dimension() ) return;

		if ( addCast ) {
			// peel off array layer from cast
			ArrayType * at = strict_dynamic_cast< ArrayType * >( addCast );
			addCast = at->base;
		}

		Expression * begin, * end, * update, * cmp;
		if ( forward ) {
			// generate: for ( int i = 0; i < N; ++i )
			begin = new ConstantExpr( Constant::from_int( 0 ) );
			end = array->dimension->clone();
			cmp = new NameExpr( "?<?" );
			update = new NameExpr( "++?" );
		} else {
			// generate: for ( int i = N-1; i >= 0; --i )
			begin = new UntypedExpr( new NameExpr( "?-?" ) );
			((UntypedExpr*)begin)->args.push_back( array->dimension->clone() );
			((UntypedExpr*)begin)->args.push_back( new ConstantExpr( Constant::from_int( 1 ) ) );
			end = new ConstantExpr( Constant::from_int( 0 ) );
			cmp = new NameExpr( "?>=?" );
			update = new NameExpr( "--?" );
		}

		ObjectDecl *index = new ObjectDecl( indexName.newName(), Type::StorageClasses(), LinkageSpec::C, 0, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), new SingleInit( begin ) );

		UntypedExpr *cond = new UntypedExpr( cmp );
		cond->args.push_back( new VariableExpr( index ) );
		cond->args.push_back( end );

		UntypedExpr *inc = new UntypedExpr( update );
		inc->args.push_back( new VariableExpr( index ) );

		UntypedExpr *dstIndex = new UntypedExpr( new NameExpr( "?[?]" ) );
		dstIndex->args.push_back( dstParam );
		dstIndex->args.push_back( new VariableExpr( index ) );
		dstParam = dstIndex;

		// srcParam must keep track of the array indices to build the
		// source parameter and/or array list initializer
		srcParam.addArrayIndex( new VariableExpr( index ), array->dimension->clone() );

		// for stmt's body, eventually containing call
		CompoundStmt * body = new CompoundStmt();
		Statement * listInit = genCall( srcParam, dstParam, fname, back_inserter( body->kids ), array->base, addCast, forward );

		// block containing for stmt and index variable
		std::list<Statement *> initList;
		CompoundStmt * block = new CompoundStmt();
		block->push_back( new DeclStmt( index ) );
		if ( listInit ) block->get_kids().push_back( listInit );
		block->push_back( new ForStmt( initList, cond, inc, body ) );

		*out++ = block;
	}

	/// Store in out a loop which calls fname on each element of the array with srcParam and 
	/// dstParam as arguments. If forward is true, loop goes from 0 to N-1, else N-1 to 0
	template< typename OutIter >
	void genArrayCall(
		InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam, 
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

		if ( forward ) {
			// generate: for ( int i = 0; i < N; ++i )
			begin = ast::ConstantExpr::from_int( loc, 0 );
			end = array->dimension;
			cmp = "?<?";
			update = "++?";
		} else {
			// generate: for ( int i = N-1; i >= 0; --i )
			begin = ast::call( 
				loc, "?-?", array->dimension, ast::ConstantExpr::from_int( loc, 1 ) );
			end = ast::ConstantExpr::from_int( loc, 0 );
			cmp = "?>=?";
			update = "--?";
		}

		ast::ptr< ast::DeclWithType > index = new ast::ObjectDecl{ 
			loc, indexName.newName(), new ast::BasicType{ ast::BasicType::SignedInt }, 
			new ast::SingleInit{ loc, begin } };
		ast::ptr< ast::Expr > indexVar = new ast::VariableExpr{ loc, index };
		
		ast::ptr< ast::Expr > cond = ast::call( loc, cmp, indexVar, end );
		
		ast::ptr< ast::Expr > inc = ast::call( loc, update, indexVar );
		
		ast::ptr< ast::Expr > dstIndex = ast::call( loc, "?[?]", dstParam, indexVar );
		
		// srcParam must keep track of the array indices to build the source parameter and/or 
		// array list initializer
		srcParam.addArrayIndex( indexVar, array->dimension );

		// for stmt's body, eventually containing call
		ast::CompoundStmt * body = new ast::CompoundStmt{ loc };
		ast::ptr< ast::Stmt > listInit = genCall( 
			srcParam, dstIndex, loc, fname, std::back_inserter( body->kids ), array->base, addCast, 
			forward );
		
		// block containing the stmt and index variable
		ast::CompoundStmt * block = new ast::CompoundStmt{ loc };
		block->push_back( new ast::DeclStmt{ loc, index } );
		if ( listInit ) { block->push_back( listInit ); }
		block->push_back( new ast::ForStmt{ loc, {}, cond, inc, body } );

		*out++ = block;
	}

	template< typename OutputIterator >
	Statement * genCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, const std::string & fname, OutputIterator out, Type * type, Type * addCast, bool forward ) {
		if ( ArrayType * at = dynamic_cast< ArrayType * >( type ) ) {
			genArrayCall( srcParam, dstParam, fname, out, at, addCast, forward );
			return 0;
		} else {
			return genScalarCall( srcParam, dstParam, fname, out, type, addCast );
		}
	}

	template< typename OutIter >
	ast::ptr< ast::Stmt > genCall(
		InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam, 
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

	/// inserts into out a generated call expression to function fname with arguments dstParam
	/// and srcParam. Intended to be used with generated ?=?, ?{}, and ^?{} calls. decl is the
	/// object being constructed. The function wraps constructor and destructor calls in an
	/// ImplicitCtorDtorStmt node.
	template< typename OutputIterator >
	void genImplicitCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, const std::string & fname, OutputIterator out, DeclarationWithType * decl, bool forward = true ) {
		ObjectDecl *obj = dynamic_cast<ObjectDecl *>( decl );
		assert( obj );
		// unnamed bit fields are not copied as they cannot be accessed
		if ( isUnnamedBitfield( obj ) ) return;

		Type * addCast = nullptr;
		if ( (fname == "?{}" || fname == "^?{}") && ( !obj || ( obj && ! obj->get_bitfieldWidth() ) ) ) {
			assert( dstParam->result );
			addCast = dstParam->result;
		}
		std::list< Statement * > stmts;
		genCall( srcParam, dstParam, fname, back_inserter( stmts ), obj->type, addCast, forward );

		// currently genCall should produce at most one element, but if that changes then the next line needs to be updated to grab the statement which contains the call
		assert( stmts.size() <= 1 );
		if ( stmts.size() == 1 ) {
			Statement * callStmt = stmts.front();
			if ( addCast ) {
				// implicitly generated ctor/dtor calls should be wrapped
				// so that later passes are aware they were generated.
				// xxx - don't mark as an implicit ctor/dtor if obj is a bitfield,
				// because this causes the address to be taken at codegen, which is illegal in C.
				callStmt = new ImplicitCtorDtorStmt( callStmt );
			}
			*out++ = callStmt;
		}
	}

	static inline ast::ptr< ast::Stmt > genImplicitCall( 
		InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam, 
		const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * obj, 
		LoopDirection forward = LoopForward 
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
				callStmt = new ast::ImplicitCtorDtorStmt{ callStmt->location, callStmt };
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

