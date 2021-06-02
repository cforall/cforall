//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenPoly.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:22:57 2017
// Update Count     : 7
//

#pragma once

#include <iostream>               // for ostream
#include <string>                 // for string, allocator, operator+, basic...

#include "ErasableScopedMap.h"    // for ErasableScopedMap
#include "AST/Fwd.hpp"
#include "SymTab/Mangler.h"       // for Mangler
#include "SynTree/Declaration.h"  // for TypeDecl::Data, AggregateDecl, Type...
#include "SynTree/SynTree.h"      // for Visitor Nodes

namespace GenPoly {

	typedef ErasableScopedMap< std::string, TypeDecl::Data > TyVarMap;
	/// Replaces a TypeInstType by its referrent in the environment, if applicable
	Type* replaceTypeInst( Type* type, const TypeSubstitution* env );

	/// returns polymorphic type if is polymorphic type, NULL otherwise; will look up substitution in env if provided
	Type *isPolyType( Type *type, const TypeSubstitution *env = 0 );
	const ast::Type * isPolyType(const ast::Type * type, const ast::TypeSubstitution * env = nullptr);

	/// returns polymorphic type if is polymorphic type in tyVars, NULL otherwise; will look up substitution in env if provided
	Type *isPolyType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env = 0 );
	const ast::Type * isPolyType(const ast::Type * type, const TyVarMap & tyVars, const ast::TypeSubstitution * env = nullptr);

	/// returns dynamic-layout type if is dynamic-layout type in tyVars, NULL otherwise; will look up substitution in env if provided
	ReferenceToType *isDynType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env = 0 );

	/// true iff function has dynamic-layout return type under the given type variable map
	ReferenceToType *isDynRet( FunctionType *function, const TyVarMap &tyVars );

	/// true iff function has dynamic-layout return type under the type variable map generated from its forall-parameters
	ReferenceToType *isDynRet( FunctionType *function );

	/// A function needs an adapter if it returns a dynamic-layout value or if any of its parameters have dynamic-layout type
	bool needsAdapter( FunctionType *adaptee, const TyVarMap &tyVarr );

	/// returns polymorphic type if is pointer to polymorphic type, NULL otherwise; will look up substitution in env if provided
	Type *isPolyPtr( Type *type, const TypeSubstitution *env = 0 );

	/// returns polymorphic type if is pointer to polymorphic type in tyVars, NULL otherwise; will look up substitution in env if provided
	Type *isPolyPtr( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env = 0 );

	/// if the base type (after dereferencing N >= 0 pointers) is a polymorphic type, returns the base type, NULL otherwise;
	/// N will be stored in levels, if provided, will look up substitution in env if provided
	Type *hasPolyBase( Type *type, int *levels = 0, const TypeSubstitution *env = 0 );

	/// if the base type (after dereferencing N >= 0 pointers) is a polymorphic type in tyVars, returns the base type, NULL otherwise;
	/// N will be stored in levels, if provided, will look up substitution in env if provided
	Type *hasPolyBase( Type *type, const TyVarMap &tyVars, int *levels = 0, const TypeSubstitution *env = 0 );

	/// true iff this type or some base of this type after dereferencing pointers is either polymorphic or a generic type with at least one
	/// polymorphic parameter; will look up substitution in env if provided.
	bool includesPolyType( Type *type, const TypeSubstitution *env = 0 );

	/// true iff this type or some base of this type after dereferencing pointers is either polymorphic in tyVars, or a generic type with
	/// at least one polymorphic parameter in tyVars; will look up substitution in env if provided.
	bool includesPolyType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env = 0 );

	/// Returns a pointer to the base FunctionType if ty is the type of a function (or pointer to one), NULL otherwise
	FunctionType *getFunctionType( Type *ty );
	const ast::FunctionType * getFunctionType( const ast::Type * ty );

	/// If expr (after dereferencing N >= 0 pointers) is a variable expression, returns the variable expression, NULL otherwise;
	/// N will be stored in levels, if provided
	VariableExpr *getBaseVar( Expression *expr, int *levels = 0 );

	/// true iff types are structurally identical, where TypeInstType's match any type.
	bool typesPolyCompatible( Type *aty, Type *bty );

	/// true if arg requires boxing given exprTyVars
	bool needsBoxing( Type * param, Type * arg, const TyVarMap &exprTyVars, const TypeSubstitution * env );
	bool needsBoxing( const ast::Type * param, const ast::Type * arg, const TyVarMap &exprTyVars, const ast::TypeSubstitution * env);

	/// true if arg requires boxing in the call to appExpr
	bool needsBoxing( Type * param, Type * arg, ApplicationExpr * appExpr, const TypeSubstitution * env );
	bool needsBoxing( const ast::Type * param, const ast::Type * arg, const ast::ApplicationExpr * appExpr, const ast::TypeSubstitution * env);

	/// Adds the type variable `tyVar` to `tyVarMap`
	void addToTyVarMap( TypeDecl * tyVar, TyVarMap &tyVarMap );

	/// Adds the declarations in the forall list of type (and its pointed-to type if it's a pointer type) to `tyVarMap`
	void makeTyVarMap( Type *type, TyVarMap &tyVarMap );
	void makeTyVarMap(const ast::Type * type, TyVarMap & tyVarMap);

	/// Prints type variable map
	void printTyVarMap( std::ostream &os, const TyVarMap &tyVarMap );

	/// Gets the mangled name of this type; alias for SymTab::Mangler::mangleType().
	inline std::string mangleType( Type *ty ) { return SymTab::Mangler::mangleType( ty ); }

	/// Gets the name of the sizeof parameter for the type, given its mangled name
	inline std::string sizeofName( const std::string &name ) { return std::string( "_sizeof_" ) + name; }

	/// Gets the name of the alignof parameter for the type, given its mangled name
	inline std::string alignofName( const std::string &name ) { return std::string( "_alignof_" ) + name; }

	/// Gets the name of the offsetof parameter for the type, given its mangled name
	inline std::string offsetofName( const std::string &name ) { return std::string( "_offsetof_" ) + name; }

	/// Gets the name of the layout function for a given aggregate type, given its declaration
	inline std::string layoutofName( AggregateDecl *decl ) { return std::string( "_layoutof_" ) + decl->get_name(); }

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
