//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptDecl.cc -- Handles declarations of exception types.
//
// Author           : Henry Xue
// Created On       : Tue Jul 20 04:10:50 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed May 25 16:43:00 2022
// Update Count     : 5
//

#include "ExceptDecl.h"

#include <cassert>               // for assert
#include <string>                // for string
#include <sstream>               // for stringstream

#include "Common/PassVisitor.h"  // for PassVisitor
#include "Common/utility.h"      // for cloneAll
#include "SynTree/Mutator.h"     // for mutateAll
#include "Virtual/Tables.h"      // for helpers

namespace ControlStruct {

const std::list< Expression *> & makeParameters(
	const std::list< TypeDecl *> & forallClause
) {
	auto parameters = new std::list< Expression *>();
	for ( auto it = forallClause.begin(); it != forallClause.end(); it++ ) {
		parameters->emplace_back( new TypeExpr(
			new TypeInstType( noQualifiers, ( *it )->get_name(), false )
		) );
	}
	return *parameters;
}

StructInstType * makeExceptInstType(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	StructInstType * exceptInstType = new StructInstType(
		noQualifiers,
		exceptionName
	);
	cloneAll( parameters, exceptInstType->parameters );
	return exceptInstType;
}

// void (*copy)(exception_name parameters * this, exception_name parameters * that);
FunctionType * makeCopyFnType(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	FunctionType * copyFnType = new FunctionType( noQualifiers, false );
	copyFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeExceptInstType( exceptionName, parameters ) ),
		nullptr
	) );
	copyFnType->get_parameters().push_back( new ObjectDecl(
		"that",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeExceptInstType( exceptionName, parameters ) ),
		nullptr
	) );
	copyFnType->get_returnVals().push_back( new ObjectDecl(
		"",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new VoidType( noQualifiers ),
		nullptr
	) );
	return copyFnType;
}

// void (*^?{})(exception_name parameters & this);
FunctionType * makeDtorFnType(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	FunctionType * dtorFnType = new FunctionType( noQualifiers, false );
	dtorFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new ReferenceType( noQualifiers,
			makeExceptInstType( exceptionName, parameters ) ),
		nullptr
	) );
	dtorFnType->get_returnVals().push_back( new ObjectDecl(
		"",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new VoidType( noQualifiers ),
		nullptr
	) );
	return dtorFnType;
}

// const char * (*msg)(exception_name parameters * this);
FunctionType * makeMsgFnType(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	FunctionType * msgFnType = new FunctionType( noQualifiers, false );
	msgFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeExceptInstType( exceptionName, parameters ) ),
		nullptr
	) );
	msgFnType->get_returnVals().push_back( new ObjectDecl(
		"",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new BasicType( Type::Const, BasicType::Char ) ),
		nullptr
	) );
	return msgFnType;
}

StructDecl * ehmTypeIdStruct(
	const std::string & exceptionName,
	const std::list< TypeDecl *> & forallClause
) {
	StructDecl * structDecl = new StructDecl( Virtual::typeIdType( exceptionName ) );
	structDecl->members.push_back( new ObjectDecl(
		"parent",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new StructInstType( Type::Const, "__cfavir_type_info" ) ),
		nullptr
	) );
	structDecl->set_body( true );
	cloneAll( forallClause, structDecl->parameters );
	return structDecl;
}

ObjectDecl * ehmTypeIdValue(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	StructInstType * typeIdType = new StructInstType(
		Type::Const,
		Virtual::typeIdType( exceptionName )
	);
	cloneAll( parameters, typeIdType->parameters );
	return new ObjectDecl(
		Virtual::typeIdName( exceptionName ),
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		typeIdType,
		new ListInit( { new SingleInit(
			new AddressExpr( new NameExpr( "__cfatid_exception_t" ) )
			) }, {}, true ),
		{ new Attribute( "cfa_linkonce" ) }
	);
}

StructDecl * ehmExceptionStructDecl(
	const std::string & exceptionName,
	const std::list< TypeDecl *> & forallClause
) {
	StructDecl * structDecl = new StructDecl( exceptionName );
	cloneAll( forallClause, structDecl->parameters );
	return structDecl;
}

StructDecl * ehmVirtualTableStruct(
	const std::string & exceptionName,
	const std::list< TypeDecl *> & forallClause,
	const std::list< Expression *> & parameters
) {
	StructInstType * typeIdType = new StructInstType(
		Type::Const,
		Virtual::typeIdType( exceptionName )
	);
	cloneAll( parameters, typeIdType->parameters );
	ObjectDecl * typeId = new ObjectDecl(
		"__cfavir_typeid",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, typeIdType ),
		nullptr
	);
	ObjectDecl * size = new ObjectDecl(
		"size",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new TypeInstType( noQualifiers, "size_t", false ),
		nullptr
	);
	ObjectDecl * copy = new ObjectDecl(
		"copy",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeCopyFnType( exceptionName, parameters ) ),
		nullptr
	);
	ObjectDecl * dtor = new ObjectDecl(
		"^?{}",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeDtorFnType( exceptionName, parameters ) ),
		nullptr
	);
	ObjectDecl * msg = new ObjectDecl(
		"msg",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			makeMsgFnType( exceptionName, parameters ) ),
		nullptr
	);
	StructDecl * structDecl = new StructDecl( Virtual::vtableTypeName( exceptionName ) );
	structDecl->members.push_back( typeId );
	structDecl->members.push_back( size );
	structDecl->members.push_back( copy );
	structDecl->members.push_back( dtor );
	structDecl->members.push_back( msg );
	structDecl->set_body( true );
	cloneAll( forallClause, structDecl->parameters );
	return structDecl;
}

StructDecl * ehmExceptionStruct(
	const std::string & exceptionName,
	const std::list< TypeDecl *> & forallClause,
	const std::list< Expression *> & parameters,
	const std::list< Declaration *> & members
) {
	StructInstType * vtableType = new StructInstType(
		Type::Const,
		Virtual::vtableTypeName( exceptionName )
	);
	cloneAll( parameters, vtableType->parameters );
	StructDecl * structDecl = new StructDecl( exceptionName );
	structDecl->members = members;
	structDecl->members.push_front( new ObjectDecl(
		"virtual_table",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, vtableType ),
		nullptr
	) );
	structDecl->set_body( true );
	cloneAll( forallClause, structDecl->parameters );
	return structDecl;
}

ObjectDecl * ehmTypeIdExtern(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	StructInstType * typeIdType = new StructInstType(
		Type::Const,
		Virtual::typeIdType( exceptionName )
	);
	cloneAll( parameters, typeIdType->parameters );
	return new ObjectDecl(
		Virtual::typeIdName( exceptionName ),
		Type::Extern,
		LinkageSpec::Cforall,
		nullptr,
		typeIdType,
		nullptr,
		{ new Attribute( "cfa_linkonce" ) }
	);
}

ObjectDecl * ehmExternVtable(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters,
	const std::string & tableName
) {
	StructInstType * vtableType = new StructInstType(
		Type::Const,
		Virtual::vtableTypeName( exceptionName )
	);
	cloneAll( parameters, vtableType->parameters );
	return new ObjectDecl(
		tableName,
		Type::Extern,
		LinkageSpec::Cforall,
		nullptr,
		vtableType,
		nullptr
	);
}

FunctionDecl * ehmDefineCopy(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	return new FunctionDecl(
		"copy",
		noStorageClasses,
		LinkageSpec::Cforall,
		makeCopyFnType( exceptionName, parameters ),
		new CompoundStmt( {
			new ExprStmt( new UntypedExpr( new NameExpr( "?=?" ), {
				new UntypedExpr( new NameExpr( "*?" ), { new NameExpr( "this" ) } ),
				new UntypedExpr( new NameExpr( "*?" ), { new NameExpr( "that" ) } )
			} ) )
		} )
	);
}

FunctionDecl * ehmDefineMsg(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters
) {
	std::stringstream msg;
	msg << exceptionName;
	if ( !parameters.empty() ) { // forall variant, add parameters
		msg << "(";
		for ( auto it = parameters.begin(); it != parameters.end(); it++ ) {
			( *it )->print( msg );
			if ( it + 1 == parameters.end() ) {
				msg << ")"; // end of list, close bracket
			} else {
				msg << ", "; // otherwise use comma as separator
			}
		}
	}
	return new FunctionDecl(
		"msg",
		noStorageClasses,
		LinkageSpec::Cforall,
		makeMsgFnType( exceptionName, parameters ),
		new CompoundStmt( {
			new ReturnStmt( new ConstantExpr( Constant::from_string( msg.str() ) ) )
		} )
	);
}

ObjectDecl * ehmVirtualTable(
	const std::string & exceptionName,
	const std::list< Expression *> & parameters,
	const std::string & tableName
) {
	StructInstType * sizeofType = new StructInstType( noQualifiers, exceptionName );
	cloneAll( parameters, sizeofType->parameters );
	std::list< Initializer *> inits {
		new SingleInit( new AddressExpr(
			new NameExpr( Virtual::typeIdName( exceptionName ) ) ) ),
		new SingleInit( new SizeofExpr( sizeofType ) ),
		new SingleInit( new NameExpr( "copy" ) ),
		new SingleInit( new NameExpr( "^?{}" ) ),
		new SingleInit( new NameExpr( "msg" ) )
	};
	std::list< Designation *> desig {
		new Designation( { new NameExpr( "__cfavir_typeid" ) } ),
		new Designation( { new NameExpr( "size" ) } ),
		new Designation( { new NameExpr( "copy" ) } ),
		new Designation( { new NameExpr( "^?{}" ) } ),
		new Designation( { new NameExpr( "msg" ) } )
	};
	StructInstType * vtableType = new StructInstType(
		Type::Const,
		Virtual::vtableTypeName( exceptionName )
	);
	cloneAll( parameters, vtableType->parameters );
	return new ObjectDecl(
		tableName,
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		vtableType,
		new ListInit( inits, desig )
	);
}

class ExceptDeclCore : public WithDeclsToAdd {
public:
	// translates exception decls
	Declaration * postmutate( StructDecl * structDecl );

	// translates vtable decls
	DeclarationWithType * postmutate( ObjectDecl * objectDecl );
};

Declaration * ExceptDeclCore::postmutate( StructDecl * structDecl ) {
	if ( structDecl->is_exception() ) {
		const std::string & exceptionName = structDecl->get_name();
		const std::list< TypeDecl *> & forallClause = structDecl->get_parameters();
		const std::list< Expression *> & parameters = makeParameters( forallClause );
		const std::list< Declaration *> & members = structDecl->get_members();

		declsToAddBefore.push_back( ehmTypeIdStruct( exceptionName, forallClause ) );
		if ( forallClause.empty() ) { // non-forall variant
			declsToAddBefore.push_back( ehmTypeIdValue( exceptionName, parameters ) );
		}
		declsToAddBefore.push_back( ehmExceptionStructDecl( exceptionName, forallClause ) );
		declsToAddBefore.push_back( ehmVirtualTableStruct( exceptionName, forallClause, parameters ) );
		return ehmExceptionStruct( exceptionName, forallClause, parameters, members );
	}
	return structDecl;
}

DeclarationWithType * ExceptDeclCore::postmutate( ObjectDecl * objectDecl ) {
	// Check if it is VTableType
	VTableType * vtableType = dynamic_cast< VTableType *>( objectDecl->get_type() );
	if ( vtableType ) {
		TypeInstType * base = dynamic_cast< TypeInstType *>( vtableType->get_base() );
		assert( base ); // should be a TypeInstType
		const std::string & exceptionName = base->get_name();
		const std::string & tableName = objectDecl->get_name();
		const std::list< Expression *> parameters = base->get_parameters();

		if ( objectDecl->get_storageClasses().is_extern ) { // if extern
			if ( !parameters.empty() ) { // forall variant
				declsToAddBefore.push_back( ehmTypeIdExtern( exceptionName, parameters ) );
			}
			return ehmExternVtable( exceptionName, parameters, tableName );
		}
		// else, non-extern
		if ( !parameters.empty() ) { // forall variant
			declsToAddBefore.push_back( ehmTypeIdValue( exceptionName, parameters ) );
		}
		declsToAddBefore.push_back( ehmDefineCopy( exceptionName, parameters ) );
		declsToAddBefore.push_back( ehmDefineMsg( exceptionName, parameters ) );
		return ehmVirtualTable( exceptionName, parameters, tableName );
	}
	return objectDecl;
}

class VTableCore : public WithDeclsToAdd {
public:
	// Remove any remaining vtable type nodes in the tree.
	Type * postmutate( VTableType * vtableType );
};

Type * VTableCore::postmutate( VTableType * vtableType ) {
	auto inst = strict_dynamic_cast<ReferenceToType *>( vtableType->base );

	std::string vtableName = Virtual::vtableTypeName( inst->name );
	StructInstType * newType = new StructInstType( noQualifiers, vtableName );
	cloneAll( inst->parameters, newType->parameters );

	delete vtableType;
	return newType;
}

void translateExcept( std::list< Declaration *> & translationUnit ) {
	PassVisitor<ExceptDeclCore> translator;
	mutateAll( translationUnit, translator );
	PassVisitor<VTableCore> typeTranslator;
	mutateAll( translationUnit, typeTranslator );
}

}
