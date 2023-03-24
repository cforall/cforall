//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FunctionDecl.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 15:11:20 2019
// Update Count     : 77
//

#include <cassert>               // for assert
#include <list>                  // for list
#include <ostream>               // for operator<<, ostream, basic_ostream
#include <string>                // for operator<<, string, char_traits, ope...

#include "Attribute.h"           // for Attribute
#include "CodeGen/FixMain.h"     // for FixMain
#include "Common/utility.h"      // for maybeClone, printAll
#include "Declaration.h"         // for FunctionDecl, FunctionDecl::Parent
#include "Expression.h"
#include "LinkageSpec.h"         // for Spec, linkageName, Cforall
#include "Statement.h"           // for CompoundStmt
#include "Type.h"                // for Type, FunctionType, Type::FuncSpecif...
#include "DeclReplacer.h"

extern bool translation_unit_nomain;

FunctionDecl::FunctionDecl( const std::string &name, Type::StorageClasses scs, LinkageSpec::Spec linkage, FunctionType *type, CompoundStmt *statements, std::list< Attribute * > attributes, Type::FuncSpecifiers fs )
	: Parent( name, scs, linkage, attributes, fs ), type( type ), statements( statements ) {
	// hack forcing the function "main" to have Cforall linkage to replace main even if it is inside an extern
	if ( name == "main" ) {
		set_linkage( CodeGen::FixMain::mainLinkage() );
	} // if
}

FunctionDecl::FunctionDecl( const FunctionDecl &other )
		: Parent( other ), type( maybeClone( other.type ) ), statements( maybeClone( other.statements ) ) {

	DeclReplacer::DeclMap declMap;
	for ( auto p : group_iterate( other.type->parameters, type->parameters ) ) {
		declMap[ std::get<0>(p) ] = std::get<1>(p);
	}
	for ( auto p : group_iterate( other.type->returnVals, type->returnVals ) ) {
		declMap[ std::get<0>(p) ] = std::get<1>(p);
	}
	if ( ! declMap.empty() ) {
		DeclReplacer::replace( this, declMap );
	}
	cloneAll( other.withExprs, withExprs );
}

FunctionDecl::~FunctionDecl() {
	delete type;
	delete statements;
	deleteAll( withExprs );
}

FunctionDecl * FunctionDecl::newFunction( const std::string & name, FunctionType * type, CompoundStmt * statements ) {
	return new FunctionDecl( name, Type::StorageClasses(), LinkageSpec::C, type, statements );
}

void FunctionDecl::print( std::ostream &os, Indenter indent ) const {
	using std::endl;
	using std::string;

	if ( name != "" ) {
		os << name << ": ";
	} // if
	if ( linkage != LinkageSpec::Cforall ) {
		os << LinkageSpec::name( linkage ) << " ";
	} // if

	printAll( attributes, os, indent );

	get_storageClasses().print( os );
	get_funcSpec().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << "untyped entity ";
	} // if

	if ( !withExprs.empty() ) {
		os << indent << "... with clause" << std::endl;
		os << indent + 1;
		printAll( withExprs, os, indent + 1 );
	}

	if ( statements ) {
		os << indent << "... with body" << endl << indent+1;
		statements->print( os, indent+1 );
	} // if
}

void FunctionDecl::printShort( std::ostream &os, Indenter indent ) const {
	using std::endl;
	using std::string;

	if ( name != "" ) {
		os << name << ": ";
	} // if

	get_storageClasses().print( os );
	get_funcSpec().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << "untyped entity ";
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
