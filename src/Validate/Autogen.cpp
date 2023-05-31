//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Autogen.cpp -- Generate automatic routines for data types.
//
// Author           : Andrew Beach
// Created On       : Thu Dec  2 13:44:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 20 16:00:00 2022
// Update Count     : 2
//

#include "Autogen.hpp"

#include <algorithm>               // for count_if
#include <cassert>                 // for strict_dynamic_cast, assert, assertf
#include <iterator>                // for back_insert_iterator, back_inserter
#include <list>                    // for list, _List_iterator, list<>::iter...
#include <set>                     // for set, _Rb_tree_const_iterator
#include <utility>                 // for pair
#include <vector>                  // for vector

#include "AST/Attribute.hpp"
#include "AST/Copy.hpp"
#include "AST/Create.hpp"
#include "AST/Decl.hpp"
#include "AST/DeclReplacer.hpp"
#include "AST/Expr.hpp"
#include "AST/Inspect.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/SymbolTable.hpp"
#include "CodeGen/OperatorTable.h" // for isCtorDtor, isCtorDtorAssign
#include "Common/ScopedMap.h"      // for ScopedMap<>::const_iterator, Scope...
#include "Common/utility.h"        // for cloneAll, operator+
#include "GenPoly/ScopedSet.h"     // for ScopedSet, ScopedSet<>::iterator
#include "InitTweak/GenInit.h"     // for fixReturnStatements
#include "InitTweak/InitTweak.h"   // for isAssignment, isCopyConstructor
#include "SymTab/GenImplicitCall.hpp"  // for genImplicitCall
#include "SymTab/Mangler.h"        // for Mangler
#include "CompilationState.h"

namespace Validate {

namespace {

// --------------------------------------------------------------------------
struct AutogenerateRoutines_new final :
		public ast::WithDeclsToAdd<>,
		public ast::WithShortCircuiting {
	void previsit( const ast::EnumDecl * enumDecl );
	void previsit( const ast::StructDecl * structDecl );
	void previsit( const ast::UnionDecl * structDecl );
	void previsit( const ast::TypeDecl * typeDecl );
	void previsit( const ast::TraitDecl * traitDecl );
	void previsit( const ast::FunctionDecl * functionDecl );
	void postvisit( const ast::FunctionDecl * functionDecl );

private:
	// Current level of nested functions.
	unsigned int functionNesting = 0;
};

// --------------------------------------------------------------------------
/// Class used to generate functions for a particular declaration.
/// Note it isn't really stored, it is just a class for organization and to
/// help pass around some of the common arguments.
class FuncGenerator {
public:
	std::list<ast::ptr<ast::Decl>> forwards;
	std::list<ast::ptr<ast::Decl>> definitions;

	FuncGenerator( const ast::Type * type, unsigned int functionNesting ) :
		type( type ), functionNesting( functionNesting )
	{}

	/// Generate functions (and forward decls.) and append them to the list.
	void generateAndAppendFunctions( std::list<ast::ptr<ast::Decl>> & );

	virtual bool shouldAutogen() const = 0;
protected:
	const ast::Type * type;
	unsigned int functionNesting;
	ast::Linkage::Spec proto_linkage = ast::Linkage::AutoGen;

	// Internal helpers:
	void genStandardFuncs();
	void produceDecl( const ast::FunctionDecl * decl );
	void produceForwardDecl( const ast::FunctionDecl * decl );

	const CodeLocation& getLocation() const { return getDecl()->location; }
	ast::FunctionDecl * genProto( std::string&& name,
		std::vector<ast::ptr<ast::DeclWithType>>&& params,
		std::vector<ast::ptr<ast::DeclWithType>>&& returns ) const;

	ast::ObjectDecl * dstParam() const;
	ast::ObjectDecl * srcParam() const;
	ast::FunctionDecl * genCtorProto() const;
	ast::FunctionDecl * genCopyProto() const;
	ast::FunctionDecl * genDtorProto() const;
	ast::FunctionDecl * genAssignProto() const;
	ast::FunctionDecl * genFieldCtorProto( unsigned int fields ) const;

	// Internal Hooks:
	virtual void genFuncBody( ast::FunctionDecl * decl ) = 0;
	virtual void genFieldCtors() = 0;
	virtual bool isConcurrentType() const { return false; }
	virtual const ast::Decl * getDecl() const = 0;
};

class StructFuncGenerator final : public FuncGenerator {
	const ast::StructDecl * decl;
public:
	StructFuncGenerator( const ast::StructDecl * decl,
			const ast::StructInstType * type,
			unsigned int functionNesting ) :
		FuncGenerator( type, functionNesting ), decl( decl )
	{}

	// Built-ins do not use autogeneration.
	bool shouldAutogen() const final { return !decl->linkage.is_builtin && !structHasFlexibleArray(decl); }
private:
	void genFuncBody( ast::FunctionDecl * decl ) final;
	void genFieldCtors() final;
	bool isConcurrentType() const final {
		return decl->is_thread() || decl->is_monitor();
	}
	virtual const ast::Decl * getDecl() const final { return decl; }

	/// Generates a single struct member operation.
	/// (constructor call, destructor call, assignment call)
	// This is managed because it uses another helper that returns a ast::ptr.
	ast::ptr<ast::Stmt> makeMemberOp(
		const CodeLocation& location,
		const ast::ObjectDecl * dstParam, const ast::Expr * src,
		const ast::ObjectDecl * field, ast::FunctionDecl * func,
		SymTab::LoopDirection direction );

	/// Generates the body of a struct function by iterating the struct members
	/// (via parameters). Generates default constructor, copy constructor,
	/// copy assignment, and destructor bodies. No field constructor bodies.
	template<typename Iterator>
	void makeFunctionBody( Iterator member, Iterator end,
			ast::FunctionDecl * func, SymTab::LoopDirection direction );

	/// Generate the body of a constructor which takes parameters that match
	/// fields. (With arguments for one to all of the fields.)
	template<typename Iterator>
	void makeFieldCtorBody( Iterator member, Iterator end,
			ast::FunctionDecl * func );
};

class UnionFuncGenerator final : public FuncGenerator {
	const ast::UnionDecl * decl;
public:
	UnionFuncGenerator( const ast::UnionDecl * decl,
			const ast::UnionInstType * type,
			unsigned int functionNesting ) :
		FuncGenerator( type, functionNesting ), decl( decl )
	{}

	// Built-ins do not use autogeneration.
	bool shouldAutogen() const final { return !decl->linkage.is_builtin; }
private:
	void genFuncBody( ast::FunctionDecl * decl ) final;
	void genFieldCtors() final;
	const ast::Decl * getDecl() const final { return decl; }

	/// Generate a single union assignment expression (using memcpy).
	ast::ExprStmt * makeAssignOp( const CodeLocation& location,
		const ast::ObjectDecl * dstParam, const ast::ObjectDecl * srcParam );
};

class EnumFuncGenerator final : public FuncGenerator {
	const ast::EnumDecl * decl;
public:
	EnumFuncGenerator( const ast::EnumDecl * decl,
			const ast::EnumInstType * type,
			unsigned int functionNesting ) :
		FuncGenerator( type, functionNesting ), decl( decl )
	{
		// TODO: These functions are somewhere between instrinsic and autogen,
		// could possibly use a new linkage type. For now we just make the
		// basic ones intrinsic to code-gen them as C assignments.
		const auto & real_type = decl->base;
		const auto & basic = real_type.as<ast::BasicType>();
		if(!real_type || (basic && basic->isInteger())) proto_linkage = ast::Linkage::Intrinsic;
	}

	bool shouldAutogen() const final { return true; }
private:
	void genFuncBody( ast::FunctionDecl * decl ) final;
	void genFieldCtors() final;
	const ast::Decl * getDecl() const final { return decl; }
};

class TypeFuncGenerator final : public FuncGenerator {
	const ast::TypeDecl * decl;
public:
	TypeFuncGenerator( const ast::TypeDecl * decl,
			ast::TypeInstType * type,
			unsigned int functionNesting ) :
		FuncGenerator( type, functionNesting ), decl( decl )
	{}

	bool shouldAutogen() const final { return true; }
	void genFieldCtors() final;
private:
	void genFuncBody( ast::FunctionDecl * decl ) final;
	const ast::Decl * getDecl() const final { return decl; }
};

// --------------------------------------------------------------------------
const std::vector<ast::ptr<ast::TypeDecl>>& getGenericParams( const ast::Type * t ) {
	if ( auto inst = dynamic_cast< const ast::StructInstType * >( t ) ) {
		return inst->base->params;
	} else if ( auto inst = dynamic_cast< const ast::UnionInstType * >( t ) ) {
		return inst->base->params;
	}
	static std::vector<ast::ptr<ast::TypeDecl>> const empty;
	return empty;
}

/// Changes the node inside a pointer so that it has the unused attribute.
void addUnusedAttribute( ast::ptr<ast::DeclWithType> & declPtr ) {
	ast::DeclWithType * decl = declPtr.get_and_mutate();
	decl->attributes.push_back( new ast::Attribute( "unused" ) );
}

// --------------------------------------------------------------------------
void AutogenerateRoutines_new::previsit( const ast::EnumDecl * enumDecl ) {
	// Must visit children (enum constants) to add them to the symbol table.
	if ( !enumDecl->body ) return;

	// if ( auto enumBaseType = enumDecl->base ) {
	// 	if ( auto enumBaseTypeAsStructInst = dynamic_cast<const ast::StructInstType *>(enumBaseType.get()) ) {
	// 		const ast::StructDecl * structDecl = enumBaseTypeAsStructInst->base.get();
	// 		this->previsit( structDecl );
	// 	}
	// }

	ast::EnumInstType enumInst( enumDecl->name );
	enumInst.base = enumDecl;
	EnumFuncGenerator gen( enumDecl, &enumInst, functionNesting );
	gen.generateAndAppendFunctions( declsToAddAfter );
}

void AutogenerateRoutines_new::previsit( const ast::StructDecl * structDecl ) {
	visit_children = false;
	if ( !structDecl->body ) return;

	ast::StructInstType structInst( structDecl->name );
	structInst.base = structDecl;
	for ( const ast::TypeDecl * typeDecl : structDecl->params ) {
		structInst.params.push_back( new ast::TypeExpr(
			typeDecl->location,
			new ast::TypeInstType( typeDecl )
		) );
	}
	StructFuncGenerator gen( structDecl, &structInst, functionNesting );
	gen.generateAndAppendFunctions( declsToAddAfter );
}

void AutogenerateRoutines_new::previsit( const ast::UnionDecl * unionDecl ) {
	visit_children = false;
	if ( !unionDecl->body ) return;

	ast::UnionInstType unionInst( unionDecl->name );
	unionInst.base = unionDecl;
	for ( const ast::TypeDecl * typeDecl : unionDecl->params ) {
		unionInst.params.push_back( new ast::TypeExpr(
			unionDecl->location,
			new ast::TypeInstType( typeDecl )
		) );
	}
	UnionFuncGenerator gen( unionDecl, &unionInst, functionNesting );
	gen.generateAndAppendFunctions( declsToAddAfter );
}

/// Generate ctor/dtors/assign for typedecls, e.g., otype T = int *;
void AutogenerateRoutines_new::previsit( const ast::TypeDecl * typeDecl ) {
	if ( !typeDecl->base ) return;

	ast::TypeInstType refType( typeDecl->name, typeDecl );
	TypeFuncGenerator gen( typeDecl, &refType, functionNesting );
	gen.generateAndAppendFunctions( declsToAddAfter );
}

void AutogenerateRoutines_new::previsit( const ast::TraitDecl * ) {
	// Ensure that we don't add assignment ops for types defined as part of the trait
	visit_children = false;
}

void AutogenerateRoutines_new::previsit( const ast::FunctionDecl * ) {
	// Track whether we're currently in a function.
	// Can ignore function type idiosyncrasies, because function type can never
	// declare a new type.
	functionNesting += 1;
}

void AutogenerateRoutines_new::postvisit( const ast::FunctionDecl * ) {
	functionNesting -= 1;
}

void FuncGenerator::generateAndAppendFunctions(
		std::list<ast::ptr<ast::Decl>> & decls ) {
	if ( !shouldAutogen() ) return;

	// Generate the functions (they go into forwards and definitions).
	genStandardFuncs();
	genFieldCtors();

	// Now export the lists contents.
	decls.splice( decls.end(), forwards );
	decls.splice( decls.end(), definitions );
}

void FuncGenerator::produceDecl( const ast::FunctionDecl * decl ) {
	assert( nullptr != decl->stmts );

	definitions.push_back( decl );
}

/// Make a forward declaration of the decl and add it to forwards.
void FuncGenerator::produceForwardDecl( const ast::FunctionDecl * decl ) {
	if (0 != functionNesting) return;
	ast::FunctionDecl * fwd =
		( decl->stmts ) ? ast::asForward( decl ) : ast::deepCopy( decl ) ;
	fwd->fixUniqueId();
	forwards.push_back( fwd );
}

void replaceAll( std::vector<ast::ptr<ast::DeclWithType>> & dwts,
		const ast::DeclReplacer::TypeMap & map ) {
	for ( auto & dwt : dwts ) {
		dwt = strict_dynamic_cast<const ast::DeclWithType *>(
				ast::DeclReplacer::replace( dwt, map ) );
	}
}

/// Generates a basic prototype function declaration.
ast::FunctionDecl * FuncGenerator::genProto( std::string&& name,
		std::vector<ast::ptr<ast::DeclWithType>>&& params,
		std::vector<ast::ptr<ast::DeclWithType>>&& returns ) const {

	// Handle generic prameters and assertions, if any.
	auto const & old_type_params = getGenericParams( type );
	ast::DeclReplacer::TypeMap oldToNew;
	std::vector<ast::ptr<ast::TypeDecl>> type_params;
	std::vector<ast::ptr<ast::DeclWithType>> assertions;
	for ( auto & old_param : old_type_params ) {
		ast::TypeDecl * decl = ast::deepCopy( old_param );
		decl->init = nullptr;
		splice( assertions, decl->assertions );
		oldToNew.emplace( std::make_pair( old_param, decl ) );
		type_params.push_back( decl );
	}
	replaceAll( params, oldToNew );
	replaceAll( returns, oldToNew );
	replaceAll( assertions, oldToNew );

	ast::FunctionDecl * decl = new ast::FunctionDecl(
		// Auto-generated routines use the type declaration's location.
		getLocation(),
		std::move( name ),
		std::move( type_params ),
		std::move( assertions ),
		std::move( params ),
		std::move( returns ),
		// Only a prototype, no body.
		nullptr,
		// Use static storage if we are at the top level.
		(0 < functionNesting) ? ast::Storage::Classes() : ast::Storage::Static,
		proto_linkage,
		std::vector<ast::ptr<ast::Attribute>>(),
		// Auto-generated routines are inline to avoid conflicts.
		ast::Function::Specs( ast::Function::Inline ) );
	decl->fixUniqueId();
	return decl;
}

ast::ObjectDecl * FuncGenerator::dstParam() const {
	return new ast::ObjectDecl( getLocation(), "_dst",
		new ast::ReferenceType( ast::deepCopy( type ) ) );
}

ast::ObjectDecl * FuncGenerator::srcParam() const {
	return new ast::ObjectDecl( getLocation(), "_src",
		ast::deepCopy( type ) );
}

/// Use the current type T to create `void ?{}(T & _dst)`.
ast::FunctionDecl * FuncGenerator::genCtorProto() const {
	return genProto( "?{}", { dstParam() }, {} );
}

/// Use the current type T to create `void ?{}(T & _dst, T _src)`.
ast::FunctionDecl * FuncGenerator::genCopyProto() const {
	return genProto( "?{}", { dstParam(), srcParam() }, {} );
}

/// Use the current type T to create `void ?{}(T & _dst)`.
ast::FunctionDecl * FuncGenerator::genDtorProto() const {
	// The destructor must be mutex on a concurrent type.
	auto dst = dstParam();
	if ( isConcurrentType() ) {
		add_qualifiers( dst->type, ast::CV::Qualifiers( ast::CV::Mutex ) );
	}
	return genProto( "^?{}", { dst }, {} );
}

/// Use the current type T to create `T ?{}(T & _dst, T _src)`.
ast::FunctionDecl * FuncGenerator::genAssignProto() const {
	// Only the name is different, so just reuse the generation function.
	auto retval = srcParam();
	retval->name = "_ret";
	return genProto( "?=?", { dstParam(), srcParam() }, { retval } );
}

// This one can return null if the last field is an unnamed bitfield.
ast::FunctionDecl * FuncGenerator::genFieldCtorProto(
		unsigned int fields ) const {
	assert( 0 < fields );
	auto aggr = strict_dynamic_cast<const ast::AggregateDecl *>( getDecl() );

	std::vector<ast::ptr<ast::DeclWithType>> params = { dstParam() };
	for ( unsigned int index = 0 ; index < fields ; ++index ) {
		auto member = aggr->members[index].strict_as<ast::DeclWithType>();
		if ( ast::isUnnamedBitfield(
				dynamic_cast<const ast::ObjectDecl *>( member ) ) ) {
			if ( index == fields - 1 ) {
				return nullptr;
			}
			continue;
		}

		auto * paramType = ast::deepCopy( member->get_type() );
		paramType->attributes.clear();
		ast::ObjectDecl * param = new ast::ObjectDecl(
			getLocation(), member->name, paramType );
		for ( auto & attr : member->attributes ) {
			if ( attr->isValidOnFuncParam() ) {
				param->attributes.push_back( attr );
			}
		}
		params.emplace_back( param );
	}
	return genProto( "?{}", std::move( params ), {} );
}

void appendReturnThis( ast::FunctionDecl * decl ) {
	assert( 1 <= decl->params.size() );
	assert( 1 == decl->returns.size() );
	assert( decl->stmts );

	const CodeLocation& location = (decl->stmts->kids.empty())
		? decl->stmts->location : decl->stmts->kids.back()->location;
	const ast::DeclWithType * thisParam = decl->params.front();
	decl->stmts.get_and_mutate()->push_back(
		new ast::ReturnStmt( location,
			new ast::VariableExpr( location, thisParam )
		)
	);
}

void FuncGenerator::genStandardFuncs() {
	// The order here determines the order that these functions are generated.
	// Assignment should come last since it uses copy constructor in return.
	ast::FunctionDecl *(FuncGenerator::*standardProtos[4])() const = {
			&FuncGenerator::genCtorProto, &FuncGenerator::genCopyProto,
			&FuncGenerator::genDtorProto, &FuncGenerator::genAssignProto };
	for ( auto & generator : standardProtos ) {
		ast::FunctionDecl * decl = (this->*generator)();
		produceForwardDecl( decl );
		genFuncBody( decl );
		if ( CodeGen::isAssignment( decl->name ) ) {
			appendReturnThis( decl );
		}
		produceDecl( decl );
	}
}

void StructFuncGenerator::genFieldCtors() {
	// The field constructors are only generated if the default constructor
	// and copy constructor are both generated, since the need both.
	unsigned numCtors = std::count_if( definitions.begin(), definitions.end(),
		[](const ast::Decl * decl){ return CodeGen::isConstructor( decl->name ); }
	);
	if ( 2 != numCtors ) return;

	for ( unsigned int num = 1 ; num <= decl->members.size() ; ++num ) {
		ast::FunctionDecl * ctor = genFieldCtorProto( num );
		if ( nullptr == ctor ) {
			continue;
		}
		produceForwardDecl( ctor );
		makeFieldCtorBody( decl->members.begin(), decl->members.end(), ctor );
		produceDecl( ctor );
	}
}

void StructFuncGenerator::genFuncBody( ast::FunctionDecl * functionDecl ) {
	// Generate appropriate calls to member constructors and assignment.
	// Destructor needs to do everything in reverse,
	// so pass "forward" based on whether the function is a destructor
	if ( CodeGen::isDestructor( functionDecl->name ) ) {
		makeFunctionBody( decl->members.rbegin(), decl->members.rend(),
			functionDecl, SymTab::LoopBackward );
	} else {
		makeFunctionBody( decl->members.begin(), decl->members.end(),
			functionDecl, SymTab::LoopForward );
	}
}

ast::ptr<ast::Stmt> StructFuncGenerator::makeMemberOp(
		const CodeLocation& location, const ast::ObjectDecl * dstParam,
		const ast::Expr * src, const ast::ObjectDecl * field,
		ast::FunctionDecl * func, SymTab::LoopDirection direction ) {
	InitTweak::InitExpander_new srcParam( src );
	// Assign to destination.
	ast::Expr * dstSelect = new ast::MemberExpr(
		location,
		field,
		new ast::CastExpr(
			location,
			new ast::VariableExpr( location, dstParam ),
			dstParam->type.strict_as<ast::ReferenceType>()->base
		)
	);
	return genImplicitCall(
		srcParam, dstSelect, location, func->name,
		field, direction
	);
}

template<typename Iterator>
void StructFuncGenerator::makeFunctionBody( Iterator current, Iterator end,
		ast::FunctionDecl * func, SymTab::LoopDirection direction ) {
	// Trying to get the best code location. Should probably use a helper or
	// just figure out what that would be given where this is called.
	assert( nullptr == func->stmts );
	const CodeLocation& location = func->location;

	ast::CompoundStmt * stmts = new ast::CompoundStmt( location );

	for ( ; current != end ; ++current ) {
		const ast::ptr<ast::Decl> & member = *current;
		auto field = member.as<ast::ObjectDecl>();
		if ( nullptr == field ) {
			continue;
		}

		// Don't assign to constant members (but do construct/destruct them).
		if ( CodeGen::isAssignment( func->name ) ) {
			// For array types we need to strip off the array layers.
			const ast::Type * type = field->get_type();
			while ( auto at = dynamic_cast<const ast::ArrayType *>( type ) ) {
				type = at->base;
			}
			if ( type->is_const() ) {
				continue;
			}
		}

		assert( !func->params.empty() );
		const ast::ObjectDecl * dstParam =
			func->params.front().strict_as<ast::ObjectDecl>();
		const ast::ObjectDecl * srcParam = nullptr;
		if ( 2 == func->params.size() ) {
			srcParam = func->params.back().strict_as<ast::ObjectDecl>();
		}

		ast::Expr * srcSelect = (srcParam) ? new ast::MemberExpr(
			location, field, new ast::VariableExpr( location, srcParam )
		) : nullptr;
		ast::ptr<ast::Stmt> stmt =
			makeMemberOp( location, dstParam, srcSelect, field, func, direction );

		if ( nullptr != stmt ) {
			stmts->kids.push_back( stmt );
		}
	}

	func->stmts = stmts;
}

template<typename Iterator>
void StructFuncGenerator::makeFieldCtorBody( Iterator current, Iterator end,
		ast::FunctionDecl * func ) {
	const CodeLocation& location = func->location;
	auto & params = func->params;
	// Need at least the constructed parameter and one field parameter.
	assert( 2 <= params.size() );

	ast::CompoundStmt * stmts = new ast::CompoundStmt( location );

	auto dstParam = params.front().strict_as<ast::ObjectDecl>();
	// Skip over the 'this' parameter.
	for ( auto param = params.begin() + 1 ; current != end ; ++current ) {
		const ast::ptr<ast::Decl> & member = *current;
		ast::ptr<ast::Stmt> stmt = nullptr;
		auto field = member.as<ast::ObjectDecl>();
		// Not sure why it could be null.
		// Don't make a function for a parameter that is an unnamed bitfield.
		if ( nullptr == field || ast::isUnnamedBitfield( field ) ) {
			continue;
		// Matching Parameter: Initialize the field by copy.
		} else if ( params.end() != param ) {
			const ast::Expr *srcSelect = new ast::VariableExpr(
				func->location, param->get() );
			stmt = makeMemberOp( location, dstParam, srcSelect, field, func, SymTab::LoopForward );
			++param;
		// No Matching Parameter: Initialize the field by default constructor.
		} else {
			stmt = makeMemberOp( location, dstParam, nullptr, field, func, SymTab::LoopForward );
		}

		if ( nullptr != stmt ) {
			stmts->kids.push_back( stmt );
		}
	}
	func->stmts = stmts;
}

void UnionFuncGenerator::genFieldCtors() {
	// Field constructors are only generated if default and copy constructor
	// are generated, since they need access to both
	unsigned numCtors = std::count_if( definitions.begin(), definitions.end(),
		[]( const ast::Decl * d ){ return CodeGen::isConstructor( d->name ); }
	);
	if ( 2 != numCtors ) {
		return;
	}

	// Create a constructor which takes the first member type as a
	// parameter. For example for `union A { int x; char y; };` generate
	// a function with signature `void ?{}(A *, int)`. This mimics C's
	// behaviour which initializes the first member of the union.

	// Still, there must be some members.
	if ( !decl->members.empty() ) {
		ast::FunctionDecl * ctor = genFieldCtorProto( 1 );
		if ( nullptr == ctor ) {
			return;
		}
		produceForwardDecl( ctor );
		auto params = ctor->params;
		auto dstParam = params.front().strict_as<ast::ObjectDecl>();
		auto srcParam = params.back().strict_as<ast::ObjectDecl>();
		ctor->stmts = new ast::CompoundStmt( getLocation(),
			{ makeAssignOp( getLocation(), dstParam, srcParam ) }
		);
		produceDecl( ctor );
	}
}

void UnionFuncGenerator::genFuncBody( ast::FunctionDecl * functionDecl ) {
	const CodeLocation& location = functionDecl->location;
	auto & params = functionDecl->params;
	if ( InitTweak::isCopyConstructor( functionDecl )
			|| InitTweak::isAssignment( functionDecl ) ) {
		assert( 2 == params.size() );
		auto dstParam = params.front().strict_as<ast::ObjectDecl>();
		auto srcParam = params.back().strict_as<ast::ObjectDecl>();
		functionDecl->stmts = new ast::CompoundStmt( location,
			{ makeAssignOp( location, dstParam, srcParam ) }
		);
	} else {
		assert( 1 == params.size() );
		// Default constructor and destructor is empty.
		functionDecl->stmts = new ast::CompoundStmt( location );
		// Add unused attribute to parameter to silence warnings.
		addUnusedAttribute( params.front() );

		// Just an extra step to make the forward and declaration match.
		if ( forwards.empty() ) return;
		ast::FunctionDecl * fwd = strict_dynamic_cast<ast::FunctionDecl *>(
			forwards.back().get_and_mutate() );
		addUnusedAttribute( fwd->params.front() );
	}
}

ast::ExprStmt * UnionFuncGenerator::makeAssignOp( const CodeLocation& location,
		const ast::ObjectDecl * dstParam, const ast::ObjectDecl * srcParam ) {
	return new ast::ExprStmt( location, new ast::UntypedExpr(
		location,
		new ast::NameExpr( location, "__builtin_memcpy" ),
		{
			new ast::AddressExpr( location,
				new ast::VariableExpr( location, dstParam ) ),
			new ast::AddressExpr( location,
				new ast::VariableExpr( location, srcParam ) ),
			new ast::SizeofExpr( location, srcParam->type ),
		} ) );
}

void EnumFuncGenerator::genFieldCtors() {
	// Enumerations to not have field constructors.
}

void EnumFuncGenerator::genFuncBody( ast::FunctionDecl * functionDecl ) {
	const CodeLocation& location = functionDecl->location;
	auto & params = functionDecl->params;
	if ( InitTweak::isCopyConstructor( functionDecl )
			|| InitTweak::isAssignment( functionDecl ) ) {
		assert( 2 == params.size() );
		auto dstParam = params.front().strict_as<ast::ObjectDecl>();
		auto srcParam = params.back().strict_as<ast::ObjectDecl>();

		/* This looks like a recursive call, but code-gen will turn it into
		 * a C-style assignment.
		 *
		 * This is still before function pointer type conversion,
		 * so this will have to do it manually.
		 *
		 * It will also reference the parent function declaration, creating
		 * a cycle for references. This also means that the ref-counts are
		 * now non-zero and the declaration will be deleted if it ever
		 * returns to zero.
		 */
		auto callExpr = new ast::ApplicationExpr( location,
			ast::VariableExpr::functionPointer( location, functionDecl ),
			{
				new ast::VariableExpr( location, dstParam ),
				new ast::VariableExpr( location, srcParam ),
			}
		);
		functionDecl->stmts = new ast::CompoundStmt( location,
			{ new ast::ExprStmt( location, callExpr ) }
		);
	} else {
		assert( 1 == params.size() );
		// Default constructor and destructor is empty.
		functionDecl->stmts = new ast::CompoundStmt( location );
		// Just add unused attribute to parameter to silence warnings.
		addUnusedAttribute( params.front() );

		// Just an extra step to make the forward and declaration match.
		if ( forwards.empty() ) return;
		ast::FunctionDecl * fwd = strict_dynamic_cast<ast::FunctionDecl *>(
			forwards.back().get_and_mutate() );
		addUnusedAttribute( fwd->params.front() );
	}
}

void TypeFuncGenerator::genFieldCtors() {
	// Opaque types do not have field constructors.
}

void TypeFuncGenerator::genFuncBody( ast::FunctionDecl * functionDecl ) {
	const CodeLocation& location = functionDecl->location;
	auto & params = functionDecl->type->params;
	assertf( 1 == params.size() || 2 == params.size(),
		"Incorrect number of parameters in autogenerated typedecl function: %zd",
		params.size() );
	auto dstParam = params.front().strict_as<ast::ObjectDecl>();
	auto srcParam = (2 == params.size())
		? params.back().strict_as<ast::ObjectDecl>() : nullptr;
	// Generate appropriate calls to member constructor and assignment.
	ast::UntypedExpr * expr = new ast::UntypedExpr( location,
		new ast::NameExpr( location, functionDecl->name )
	);
	expr->args.push_back( new ast::CastExpr( location,
		new ast::VariableExpr( location, dstParam ),
		new ast::ReferenceType( decl->base )
	) );
	if ( srcParam ) {
		expr->args.push_back( new ast::CastExpr( location,
			new ast::VariableExpr( location, srcParam ),
			decl->base
		) );
	}
	functionDecl->stmts = new ast::CompoundStmt( location,
		{ new ast::ExprStmt( location, expr ) }
	);
}

} // namespace

void autogenerateRoutines( ast::TranslationUnit & translationUnit ) {
	ast::Pass<AutogenerateRoutines_new>::run( translationUnit );
}

} // Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
