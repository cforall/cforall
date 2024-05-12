//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tables.hpp --
//
// Author           : Andrew Beach
// Created On       : Mon Aug 31 11:07:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wec Dec  8 16:58:00 2021
// Update Count     : 3
//

#include <list>  // for list

#include <string>
#include "AST/Fwd.hpp"

namespace Virtual {

std::string typeIdType( std::string const & type_name );
std::string typeIdName( std::string const & type_name );
std::string vtableTypeName( std::string const & type_name );
std::string instanceName( std::string const & vtable_name );
std::string vtableInstanceName( std::string const & type_name );
std::string concurrentDefaultVTableName();
bool isVTableInstanceName( std::string const & name );

/* Create a forward declaration of a vtable of the given type.
 * vtableType node is consumed.
 */
ast::ObjectDecl * makeVtableForward(
	CodeLocation const & location, std::string const & name,
	ast::StructInstType const * vtableType );

/* Create an initialized definition of a vtable.
 * vtableType and init (if provided) nodes are consumed.
 */
ast::ObjectDecl * makeVtableInstance(
	CodeLocation const & location,
	std::string const & name,
	ast::StructInstType const * vtableType,
	ast::Type const * objectType,
	ast::Init const * init = nullptr );

// Some special code for how exceptions interact with virtual tables.

/* Create a forward declaration of the exception virtual function
 * linking the vtableType to the exceptType. Both nodes are consumed.
 */
ast::FunctionDecl * makeGetExceptionForward(
	CodeLocation const & location,
	ast::Type const * vtableType,
	ast::Type const * exceptType );

/* Create the definition of the exception virtual function.
 * exceptType node is consumed.
 */
ast::FunctionDecl * makeGetExceptionFunction(
	CodeLocation const & location,
	ast::ObjectDecl const * vtableInstance, ast::Type const * exceptType );

/* Build an instance of the type-id from the type of the type-id.
 * TODO: Should take the parent type. Currently locked to the exception_t.
 */
ast::ObjectDecl * makeTypeIdInstance(
	const CodeLocation & location, ast::StructInstType const * typeIdType );

}
