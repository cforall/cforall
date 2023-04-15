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
// Last Modified By : Andrew Beach
// Last Modified On : Fri Apr 14 15:06:00 2023
// Update Count     : 17
//

#pragma once

#include <cassert>                // for assert
#include <iterator>               // for back_inserter
#include <string>                 // for string

#include "AST/Decl.hpp"
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

	/// generate the type of an assignment function for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genAssignType( Type * paramType, bool maybePolymorphic = true );

	/// generate the type of a default constructor or destructor for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genDefaultType( Type * paramType, bool maybePolymorphic = true );

	/// generate the type of a copy constructor for paramType.
	/// maybePolymorphic is true if the resulting FunctionType is allowed to be polymorphic
	FunctionType * genCopyType( Type * paramType, bool maybePolymorphic = true );

	/// Enum for loop direction
	enum LoopDirection { LoopBackward, LoopForward };

	/// inserts into out a generated call expression to function fname with arguments dstParam and srcParam. Intended to be used with generated ?=?, ?{}, and ^?{} calls.
	template< typename OutputIterator >
	Statement * genCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, const std::string & fname, OutputIterator out, Type * type, Type * addCast = nullptr, bool forward = true );

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

	template< typename OutputIterator >
	Statement * genCall( InitTweak::InitExpander_old & srcParam, Expression * dstParam, const std::string & fname, OutputIterator out, Type * type, Type * addCast, bool forward ) {
		if ( ArrayType * at = dynamic_cast< ArrayType * >( type ) ) {
			genArrayCall( srcParam, dstParam, fname, out, at, addCast, forward );
			return 0;
		} else {
			return genScalarCall( srcParam, dstParam, fname, out, type, addCast );
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

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
