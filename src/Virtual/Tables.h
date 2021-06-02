//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tables.h --
//
// Author           : Andrew Beach
// Created On       : Mon Aug 31 11:07:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 21 10:30:00 2021
// Update Count     : 2
//

#include <list>  // for list

class Declaration;
class StructDecl;
class Expression;

namespace Virtual {

std::string typeIdType( std::string const & type_name );
std::string typeIdName( std::string const & type_name );
std::string vtableTypeName( std::string const & type_name );
std::string instanceName( std::string const & vtable_name );
std::string vtableInstanceName( std::string const & type_name );
std::string concurrentDefaultVTableName();
bool isVTableInstanceName( std::string const & name );

ObjectDecl * makeVtableForward(
	std::string const & name, StructInstType * vtableType );
/* Create a forward declaration of a vtable of the given type.
 * vtableType node is consumed.
 */

ObjectDecl * makeVtableInstance(
	std::string const & name,
	StructInstType * vtableType, Type * objectType,
	Initializer * init = nullptr );
/* Create an initialized definition of a vtable.
 * vtableType and init (if provided) nodes are consumed.
 */

// Some special code for how exceptions interact with virtual tables.
FunctionDecl * makeGetExceptionForward( Type * vtableType, Type * exceptType );
/* Create a forward declaration of the exception virtual function
 * linking the vtableType to the exceptType. Both nodes are consumed.
 */

FunctionDecl * makeGetExceptionFunction(
	ObjectDecl * vtableInstance, Type * exceptType );
/* Create the definition of the exception virtual function.
 * exceptType node is consumed.
 */

ObjectDecl * makeTypeIdInstance( StructInstType const * typeIdType );
/* Build an instance of the type-id from the type of the type-id.
 * TODO: Should take the parent type. Currently locked to the exception_t.
 */

}
