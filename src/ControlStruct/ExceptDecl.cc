//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptDecl.cc --
//
// Author           : Henry Xue
// Created On       : Tue Jul 20 04:10:50 2021
// Last Modified By : Henry Xue
// Last Modified On : Tue Jul 20 04:10:50 2021
// Update Count     : 1
//

#include "ExceptDecl.h"

#include "Common/PassVisitor.h"  // for PassVisitor
#include "SynTree/Mutator.h"     // for mutateAll
#include "Virtual/Tables.h"      // for helpers

namespace ControlStruct {

StructDecl * ehmTypeIdStruct( const std::string & exceptionName, std::list< TypeDecl *> * parameters ) {
	StructDecl * structDecl = new StructDecl( Virtual::typeIdType( exceptionName ) );
	structDecl->members.push_back( new ObjectDecl(
		"parent",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new TypeInstType( Type::Const, "__cfavir_type_info", false ) ),
		nullptr
	) );
	structDecl->set_body( true );
	if ( parameters ) {
		structDecl->parameters = *parameters;
	}
	return structDecl;
}

ObjectDecl * ehmTypeIdValue( const std::string & exceptionName, std::list< Expression *> * parameters ) {
	StructInstType * structInstType = new StructInstType( Type::Const, Virtual::typeIdType( exceptionName ) );
	if ( parameters ) {
		structInstType->parameters = *parameters;
	}
	return new ObjectDecl(
		Virtual::typeIdName( exceptionName ),
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		structInstType,
		new ListInit( { new SingleInit(
			new AddressExpr( new NameExpr( "__cfatid_exception_t" ) )
			) }, {}, true ),
		{ new Attribute( "cfa_linkonce" ) }
	);
}

StructDecl * ehmExceptionStructDecl( const std::string & exceptionName, std::list< TypeDecl *> * parameters ) {
	StructDecl * structDecl = new StructDecl( exceptionName );
	if ( parameters ) {
		structDecl->parameters = *parameters;
	}
	return structDecl;
}

StructDecl * ehmVirtualTableStruct( const std::string & exceptionName, std::list< TypeDecl *> * parameters ) {
	// _EHM_TYPE_ID_TYPE(exception_name) parameters const * __cfavir_typeid;
	ObjectDecl * typeId = new ObjectDecl(
		"__cfavir_typeid",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new StructInstType( Type::Const, Virtual::typeIdType( exceptionName ) ) ),
		nullptr
	);

	// size_t size;
	ObjectDecl * size = new ObjectDecl(
		"size",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new TypeInstType( noQualifiers, "size_t", false ),
		nullptr
	);
	
	// void (*copy)(exception_name parameters * this, exception_name parameters * other);
	FunctionType * copyFnType = new FunctionType( noQualifiers, false );
	copyFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new TypeInstType( noQualifiers, exceptionName, false ) ),
		nullptr
	) );
	copyFnType->get_parameters().push_back( new ObjectDecl(
		"other",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new TypeInstType( noQualifiers, exceptionName, false ) ),
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
	ObjectDecl * copy = new ObjectDecl(
		"copy",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, copyFnType ),
		nullptr
	);

	// void (*^?{})(exception_name parameters & this);
	FunctionType * dtorFnType = new FunctionType( noQualifiers, false );
	dtorFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new ReferenceType( noQualifiers,
			new TypeInstType( noQualifiers, exceptionName, false ) ),
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
	ObjectDecl * dtor = new ObjectDecl(
		"^?{}",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, dtorFnType ),
		nullptr
	);

	// const char * (*msg)(exception_name parameters * this);
	FunctionType * msgFnType = new FunctionType( noQualifiers, false );
	msgFnType->get_parameters().push_back( new ObjectDecl(
		"this",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new TypeInstType( noQualifiers, exceptionName, false ) ),
		nullptr
	) );
	msgFnType->get_returnVals().push_back( new ObjectDecl(
		"",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, new BasicType( Type::Const, BasicType::Char ) ),
		nullptr
	) );
	ObjectDecl * msg = new ObjectDecl(
		"msg",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, msgFnType ),
		nullptr
	);

	StructDecl * structDecl = new StructDecl( Virtual::vtableTypeName( exceptionName ) );
	structDecl->members.push_back( typeId );
	structDecl->members.push_back( size );
	structDecl->members.push_back( copy );
	structDecl->members.push_back( dtor );
	structDecl->members.push_back( msg );
	structDecl->set_body( true );
	if ( parameters ) {
		structDecl->parameters = *parameters;
	}
	return structDecl;
}

StructDecl * ehmExceptionStruct( const std::string & exceptionName, const std::list<Declaration*> & members,
								 std::list<TypeDecl *> * parameters ) {
	StructDecl * structDecl = new StructDecl( exceptionName );
	structDecl->members = members;
	structDecl->members.push_front( new ObjectDecl(
		"virtual_table",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers,
			new StructInstType( Type::Const, Virtual::vtableTypeName( exceptionName ) ) ),
		nullptr
	) );
	structDecl->set_body( true );
	if ( parameters ) {
		structDecl->parameters = *parameters;
	}
	return structDecl;
}

class ExceptDeclCore : public WithDeclsToAdd {
public:
	Declaration * postmutate( StructDecl * structDecl );
};

Declaration * ExceptDeclCore::postmutate( StructDecl * structDecl ) {
	if ( structDecl->is_exception() ) {
		const std::string & exceptionName = structDecl->name;
		declsToAddBefore.push_back( ehmTypeIdStruct( exceptionName, nullptr ) );
		declsToAddBefore.push_back( ehmTypeIdValue( exceptionName, nullptr ) );
		declsToAddBefore.push_back( ehmExceptionStructDecl( exceptionName, nullptr ) );
		declsToAddBefore.push_back( ehmVirtualTableStruct( exceptionName, nullptr ) );
		return ehmExceptionStruct( exceptionName, structDecl->get_members(), nullptr );
	}
	return structDecl;
}

void translateExcept( std::list< Declaration *> & translationUnit ) {
	PassVisitor<ExceptDeclCore> translator;
	mutateAll( translationUnit, translator );
}

}
