//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptDeclNew.cpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jul 12 15:50:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul 18 11:01:00 2022
// Update Count     : 0
//

#include "ExceptDecl.h"

#include <sstream>

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Type.hpp"
#include "Virtual/Tables.h"

namespace ControlStruct {

namespace {

std::vector<ast::ptr<ast::Expr>> forallToParams(
		std::vector<ast::ptr<ast::TypeDecl>> const & forall ) {
	return map_range<std::vector<ast::ptr<ast::Expr>>>( forall,
		[]( ast::ptr<ast::TypeDecl> const & decl ) {
			return new ast::TypeExpr( decl->location,
				new ast::TypeInstType( decl->name, decl->kind ) );
		}
	);
}

// A slightly argumented extra constructor, adds a deepCopy.
ast::StructInstType * namedStructInstType(
		std::string const & name, ast::CV::Qualifiers qualifiers,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::StructInstType * type = new ast::StructInstType( name, qualifiers );
	for ( ast::ptr<ast::Expr> const & param : params ) {
		type->params.push_back( ast::deepCopy( param ) );
	}
	return type;
}

ast::StructInstType * createExceptionInstType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	return namedStructInstType( exceptionName, ast::CV::Qualifiers(), params );
}

ast::StructInstType * createVTableInstType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	std::string name = Virtual::vtableTypeName( exceptionName );
	return namedStructInstType( name, ast::CV::Const, params );
}

ast::StructInstType * createTypeIdInstType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	std::string name = Virtual::typeIdType( exceptionName );
	return namedStructInstType( name, ast::CV::Const, params );
}

ast::FunctionType const * createCopyFuncType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::FunctionType * type = new ast::FunctionType( ast::FixedArgs );
	type->params.push_back( new ast::PointerType(
		createExceptionInstType( exceptionName, params ) ) );
	type->params.push_back( new ast::PointerType(
		createExceptionInstType( exceptionName, params ) ) );
	type->returns.push_back( new ast::VoidType() );
	return type;
}

ast::FunctionType const * createDtorFuncType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::FunctionType * type = new ast::FunctionType( ast::FixedArgs );
	type->params.push_back( new ast::ReferenceType(
		createExceptionInstType( exceptionName, params ) ) );
	type->returns.push_back( new ast::VoidType() );
	return type;
}

ast::FunctionType const * createMsgFuncType(
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::FunctionType * type = new ast::FunctionType( ast::FixedArgs );
	type->params.push_back( new ast::PointerType(
		createExceptionInstType( exceptionName, params ) ) );
	type->returns.push_back( new ast::PointerType(
		new ast::BasicType( ast::BasicType::Char, ast::CV::Const ) ) );
	return type;
}

ast::StructDecl const * createTypeIdStruct(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::TypeDecl>> const & forallClause ) {
	ast::StructDecl * decl = new ast::StructDecl( location,
			Virtual::typeIdType( exceptionName ) );
	decl->members.push_back( new ast::ObjectDecl(
		location,
		"parent",
		new ast::PointerType(
			new ast::StructInstType( "__cfavir_type_info", ast::CV::Const ) )
	) );
	decl->body = true;
	for ( ast::ptr<ast::TypeDecl> const & param : forallClause ) {
		decl->params.push_back( ast::deepCopy( param ) );
	}
	return decl;
}

ast::ObjectDecl const * createTypeIdValue(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::StructInstType * typeIdType =
		createTypeIdInstType( exceptionName, params );
	return new ast::ObjectDecl(
		location,
		Virtual::typeIdName( exceptionName ),
		typeIdType,
		new ast::ListInit( location, {
			new ast::SingleInit( location,
				new ast::AddressExpr( location,
					new ast::NameExpr( location, "__cfatid_exception_t" ) ),
				ast::MaybeConstruct ),
		}, {}, ast::MaybeConstruct ),
		ast::Storage::Classes(),
		ast::Linkage::Cforall,
		nullptr,
		{ new ast::Attribute( "cfa_linkonce" ) }
	);
}

ast::StructDecl const * createExceptionStructForward(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::TypeDecl>> const & forall ) {
	ast::StructDecl * decl = new ast::StructDecl( location, exceptionName );
	for ( ast::ptr<ast::TypeDecl> const & param : forall ) {
		decl->params.push_back( ast::deepCopy( param ) );
	}
	return decl;
}

ast::StructDecl const * createVirtualTableStruct(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::TypeDecl>> const & forall,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	ast::StructInstType * typeIdType =
		createTypeIdInstType( exceptionName, params );
	ast::ObjectDecl * typeId = new ast::ObjectDecl(
		location,
		"__cfavir_typeid",
		new ast::PointerType( typeIdType )
	);
	ast::ObjectDecl * size = new ast::ObjectDecl(
		location,
		"size",
		new ast::TypeInstType( "size_t", ast::TypeDecl::Dtype )
	);
	ast::ObjectDecl * copy = new ast::ObjectDecl(
		location,
		"copy",
		new ast::PointerType( createCopyFuncType( exceptionName, params ) )
	);
	ast::ObjectDecl * dtor = new ast::ObjectDecl(
		location,
		"^?{}",
		new ast::PointerType( createDtorFuncType( exceptionName, params ) )
	);
	ast::ObjectDecl * msg = new ast::ObjectDecl(
		location,
		"msg",
		new ast::PointerType( createMsgFuncType( exceptionName, params ) )
	);
	ast::StructDecl * decl = new ast::StructDecl(
		location,
		Virtual::vtableTypeName( exceptionName ) );
	decl->members.push_back( typeId );
	decl->members.push_back( size );
	decl->members.push_back( copy );
	decl->members.push_back( dtor );
	decl->members.push_back( msg );
	decl->body = true;
	for ( ast::ptr<ast::TypeDecl> const & param : forall ) {
		decl->params.push_back( param );
	}
	return decl;
}

ast::StructDecl const * createExceptionStruct(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::TypeDecl>> const & forallClause,
		std::vector<ast::ptr<ast::Expr>> const & params,
		std::vector<ast::ptr<ast::Decl>> const & members ) {
	ast::StructDecl * decl = new ast::StructDecl( location, exceptionName );
	decl->members.push_back( new ast::ObjectDecl(
		location,
		"virtual_table",
		new ast::PointerType(
			createVTableInstType( exceptionName, params ) )
	) );
	for ( ast::ptr<ast::Decl> const & member : members ) {
		decl->members.push_back( ast::deepCopy( member ) );
	}
	decl->body = true;
	for ( ast::ptr<ast::TypeDecl> const & param : forallClause ) {
		decl->params.push_back( ast::deepCopy( param ) );
	}
	return decl;
}

ast::ObjectDecl const * createExternTypeId(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	return new ast::ObjectDecl(
		location,
		Virtual::typeIdName( exceptionName ),
		createVTableInstType( exceptionName, params ),
		nullptr,
		ast::Storage::Extern,
		ast::Linkage::Cforall,
		nullptr,
		{ new ast::Attribute( "cfa_linkonce" ) }
	);
}

ast::ObjectDecl * createExternVTable(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params,
		std::string const & tableName ) {
	return new ast::ObjectDecl(
		location,
		tableName,
		createVTableInstType( exceptionName, params ),
		nullptr,
		ast::Storage::Extern,
		ast::Linkage::Cforall
	);
}

ast::FunctionDecl const * createCopy(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	return new ast::FunctionDecl(
		location,
		"copy",
		{/* forall */},
		{/* assertions */},
		{
			new ast::ObjectDecl(
				location,
				"this",
				new ast::PointerType(
					createExceptionInstType( exceptionName, params ) )
			),
			new ast::ObjectDecl(
				location,
				"that",
				new ast::PointerType(
					createExceptionInstType( exceptionName, params ) )
			),
		},
		{
			new ast::ObjectDecl( location, "", new ast::VoidType() ),
		},
		new ast::CompoundStmt( location, {
			new ast::ExprStmt( location,
				new ast::UntypedExpr( location,
					new ast::NameExpr( location, "?=?" ),
					{
						new ast::UntypedExpr( location,
							new ast::NameExpr( location, "*?" ),
							{ new ast::NameExpr( location, "this" ) } ),
						new ast::UntypedExpr( location,
							new ast::NameExpr( location, "*?" ),
							{ new ast::NameExpr( location, "that" ) } ),
					}
				)
			),
		} ),
        ast::Storage::Classes(),
		ast::Linkage::Cforall,
        { new ast::Attribute( "cfa_linkonce" ) }
	);
}

ast::FunctionDecl const * createMsg(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params ) {
	std::stringstream msg;
	msg << exceptionName;
	// The forall variant, add parameters to the string.
	if ( !params.empty() ) {
		msg << "(";
		bool first = true;
		for ( auto & param : params ) {
			// Seperator Logic: A comma proceeds all but the first object.
			if ( first ) {
				first = false;
			} else {
				msg << ", ";
			}

			ast::print( msg, param.get() );
		}
		msg << ")";
	}
	return new ast::FunctionDecl(
		location,
		"msg",
		{/* forall */},
		{/* assertions */},
		{
			new ast::ObjectDecl(
				location,
				"this",
				new ast::PointerType(
					createExceptionInstType( exceptionName, params ) )
			),
		},
		{
			new ast::ObjectDecl(
				location,
				"",
				new ast::PointerType(
					new ast::BasicType( ast::BasicType::Char, ast::CV::Const ) )
			),
		},
		new ast::CompoundStmt( location, {
			new ast::ReturnStmt( location,
				ast::ConstantExpr::from_string( location, msg.str() )
			),
		} ),
		ast::Storage::Classes(),
		ast::Linkage::Cforall,
        { new ast::Attribute( "cfa_linkonce" ) }
	);
}

ast::ObjectDecl * createVirtualTable(
		CodeLocation const & location,
		std::string const & exceptionName,
		std::vector<ast::ptr<ast::Expr>> const & params,
		std::string const & tableName ) {
	ast::StructInstType * sizeType = new ast::StructInstType( exceptionName );
	for ( ast::ptr<ast::Expr> const & param : params ) {
		sizeType->params.push_back( ast::deepCopy( param ) );
	}
	std::vector<ast::ptr<ast::Init>> inits {
		new ast::SingleInit( location,
			new ast::AddressExpr( location,
				new ast::NameExpr( location,
					Virtual::typeIdName( exceptionName ) ) ) ),
		new ast::SingleInit( location,
			new ast::SizeofExpr( location, sizeType )  ),
		new ast::SingleInit( location,
			new ast::NameExpr( location, "copy" ) ),
		new ast::SingleInit( location,
			new ast::NameExpr( location, "^?{}" ) ),
		new ast::SingleInit( location,
			new ast::NameExpr( location, "msg" ) ),
	};
	std::vector<ast::ptr<ast::Designation>> dsigs {
		new ast::Designation( location, {
			new ast::NameExpr( location, "__cfavir_typeid" ) } ),
		new ast::Designation( location, {
			new ast::NameExpr( location, "size" ) } ),
		new ast::Designation( location, {
			new ast::NameExpr( location, "copy" ) } ),
		new ast::Designation( location, {
			new ast::NameExpr( location, "^?{}" ) } ),
		new ast::Designation( location, {
			new ast::NameExpr( location, "msg" ) } ),
	};
	return new ast::ObjectDecl(
		location,
		tableName,
		createVTableInstType( exceptionName, params ),
		new ast::ListInit( location, std::move( inits ), std::move( dsigs ) )
	);
}

struct ExceptDeclCore : public ast::WithDeclsToAdd<> {
	ast::StructDecl const * transformExcept( ast::StructDecl const * decl );
	ast::ObjectDecl const * transformVTable(
		ast::ObjectDecl const * decl, ast::VTableType const * type );

	ast::StructDecl const * postvisit( ast::StructDecl const * decl ) {
		// Exceptions don't get their own node type, so filter that.
		if ( ast::AggregateDecl::Exception == decl->kind ) {
			return transformExcept( decl );
		}
		return decl;
	}

	ast::ObjectDecl const * postvisit( ast::ObjectDecl const * decl ) {
		// Modify remaining objects that have a vtable type.
		if ( auto * type = decl->type.as<ast::VTableType>() ) {
			return transformVTable( decl, type );
		}
		return decl;
	}
};

ast::StructDecl const * ExceptDeclCore::transformExcept(
		ast::StructDecl const * decl ) {
	CodeLocation const & location = decl->location;
	std::string const & exceptionName = decl->name;
	std::vector<ast::ptr<ast::TypeDecl>> const & forall = decl->params;
	std::vector<ast::ptr<ast::Expr>> params = forallToParams( forall );
	std::vector<ast::ptr<ast::Decl>> const & members = decl->members;

	declsToAddBefore.push_back(
		createTypeIdStruct( location, exceptionName, forall ) );
	if ( forall.empty() ) {
		// Non-forall variant.
		declsToAddBefore.push_back(
			createTypeIdValue( location, exceptionName, params ) );
	}
	declsToAddBefore.push_back(
		createExceptionStructForward( location, exceptionName, forall ) );
	declsToAddBefore.push_back(
		createVirtualTableStruct( location, exceptionName, forall, params ) );
	return createExceptionStruct( location, exceptionName, forall, params, members );
}

ast::ObjectDecl const * ExceptDeclCore::transformVTable(
		ast::ObjectDecl const * decl, ast::VTableType const * type ) {
	CodeLocation const & location = decl->location;
	auto base = type->base.strict_as<ast::TypeInstType>();
	std::string const & exceptionName = base->name;
	std::vector<ast::ptr<ast::Expr>> const & params = base->params;
	std::string const & tableName = decl->name;

	ast::ObjectDecl * retDecl;
	if ( decl->storage.is_extern ) {
		// Unique type-ids are only needed for polymorphic instances.
		if ( !params.empty() ) {
			declsToAddBefore.push_back(
				createExternTypeId( location, exceptionName, params ) );
		}
		retDecl = createExternVTable( location, exceptionName, params, tableName );
	} else {
		// Unique type-ids are only needed for polymorphic instances.
		if ( !params.empty() ) {
			declsToAddBefore.push_back(
				createTypeIdValue( location, exceptionName, params ) );
		}
		declsToAddBefore.push_back(
			createCopy( location, exceptionName, params ) );
		declsToAddBefore.push_back(
			createMsg( location, exceptionName, params ) );
		retDecl = createVirtualTable(
			location, exceptionName, params, tableName );
	}

	for ( ast::ptr<ast::Attribute> const & attr : decl->attributes ) {
		retDecl->attributes.push_back( attr );
	}

	return retDecl;
}

struct VTableCore {
	ast::StructInstType const * postvisit( ast::VTableType const * type ) {
		auto inst = type->base.as<ast::BaseInstType>();

		std::string vtableName = Virtual::vtableTypeName( inst->name );

		auto newType = new ast::StructInstType( vtableName );
		for ( ast::ptr<ast::Expr> const & param : inst->params ) {
			newType->params.push_back( param );
		}

		return newType;
	}
};

} // namespace

void translateExcept( ast::TranslationUnit & translationUnit ) {
	// Can I combine these?
	// Second pass really only covers what the first has missed.
	// Maybe if the first one is all previsits and the second all postvisit.
	ast::Pass<ExceptDeclCore>::run( translationUnit );
	ast::Pass<VTableCore>::run( translationUnit );
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
