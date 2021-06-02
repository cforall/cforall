//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Declaration.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 18:35:39 2021
// Update Count     : 37
//

#include <map>                       // for _Rb_tree_const_iterator, map<>::...
#include <ostream>                   // for ostream, operator<<, basic_ostre...
#include <string>                    // for string
#include <utility>                   // for pair

#include "Common/utility.h"          // for maybeClone
#include "Declaration.h"
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "SynTree/Statement.h"       // for AsmStmt
#include "SynTree/SynTree.h"         // for UniqueId
#include "SynTree/Expression.h"
#include "Type.h"                    // for Type, Type::StorageClasses

// To canonicalize declarations
static UniqueId lastUniqueId = 0;

Declaration::Declaration( const std::string &name, Type::StorageClasses scs, LinkageSpec::Spec linkage )
		: name( name ), linkage( linkage ), uniqueId( 0 ), storageClasses( scs ) {
}

Declaration::Declaration( const Declaration &other )
	: BaseSyntaxNode( other ), name( other.name ), linkage( other.linkage ), extension( other.extension ), uniqueId( other.uniqueId ), storageClasses( other.storageClasses ) {
}

Declaration::~Declaration() {
}

void Declaration::fixUniqueId() {
	// don't need to set unique ID twice
	if ( uniqueId ) return;
	uniqueId = ++lastUniqueId;
}

AsmDecl::AsmDecl( AsmStmt *stmt ) : Declaration( "", Type::StorageClasses(), LinkageSpec::C ), stmt( stmt ) {
}

AsmDecl::AsmDecl( const AsmDecl &other ) : Declaration( other ), stmt( maybeClone( other.stmt ) ) {
}

AsmDecl::~AsmDecl() {
	delete stmt;
}

void AsmDecl::print( std::ostream &os, Indenter indent ) const {
	stmt->print( os, indent );
}

void AsmDecl::printShort( std::ostream &os, Indenter indent ) const {
	stmt->print( os, indent );
}


DirectiveDecl::DirectiveDecl( DirectiveStmt *stmt ) : Declaration( "", Type::StorageClasses(), LinkageSpec::C ), stmt( stmt ) {
}

DirectiveDecl::DirectiveDecl( const DirectiveDecl &other ) : Declaration( other ), stmt( maybeClone( other.stmt ) ) {
}

DirectiveDecl::~DirectiveDecl() {
	delete stmt;
}

void DirectiveDecl::print( std::ostream &os, Indenter indent ) const {
	stmt->print( os, indent );
}

void DirectiveDecl::printShort( std::ostream &os, Indenter indent ) const {
	stmt->print( os, indent );
}


StaticAssertDecl::StaticAssertDecl( Expression * condition, ConstantExpr * message ) : Declaration( "", Type::StorageClasses(), LinkageSpec::C ), condition( condition ), message( message )  {
}

StaticAssertDecl::StaticAssertDecl( const StaticAssertDecl & other ) : Declaration( other ), condition( maybeClone( other.condition ) ), message( maybeClone( other.message ) )  {
}

StaticAssertDecl::~StaticAssertDecl() {
	delete condition;
	delete message;
}

void StaticAssertDecl::print( std::ostream &os, Indenter indent ) const {
	os << "Static Assert with condition: ";
	condition->print( os, indent+1 );
	os << std::endl << indent << "and message: ";
	message->print( os, indent+1 );
os << std::endl;
}

void StaticAssertDecl::printShort( std::ostream &os, Indenter indent ) const {
	print( os, indent );
}


// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
