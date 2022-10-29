#include <list>                  // for list
#include <ostream>               // for operator<<, ostream, basic_ostream
#include <string>                // for operator<<, string, char_traits, ope...

#include "Attribute.h"           // for Attribute
#include "Declaration.h"
#include "Common/utility.h"      // for maybeClone, printAll
#include "LinkageSpec.h"         // for Spec, linkageName, Cforall
#include "Type.h"                // for Type, Type::StorageClasses, Type::Fu...

InlineValueDecl::InlineValueDecl( const std::string &name, Type::StorageClasses scs, LinkageSpec::Spec linkage,
Type * type, const std::list< Attribute * >attributes, Type::FuncSpecifiers fs) 
    : Parent( name, scs, linkage, attributes, fs ), type( type ) {}

InlineValueDecl::InlineValueDecl( const InlineValueDecl &other) 
    : Parent( other), type( maybeClone( other.type ) ) {}

InlineValueDecl::~InlineValueDecl() { delete type; }

InlineValueDecl * InlineValueDecl::newInlineValueDecl( const std::string &name, Type * type ) {
    return new InlineValueDecl( name, Type::StorageClasses(), LinkageSpec::C, type );
}

void InlineValueDecl::print( std::ostream &os, Indenter indent ) const {
    if ( name != "" ) os << name << ": ";

	if ( linkage != LinkageSpec::Cforall ) {
		os << LinkageSpec::name( linkage ) << " ";
	} // if

	get_storageClasses().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << " untyped entity ";
	} // if

	if ( ! attributes.empty() ) {
		os << std::endl << indent << "... with attributes:" << std::endl;
		printAll( attributes, os, indent+1 );
	} // if

}

void InlineValueDecl::printShort( std::ostream &os, Indenter indent ) const {
    if ( name != "" ) os << name << ": ";

	get_storageClasses().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << "untyped entity ";
	} // if
    
}