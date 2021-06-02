//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Occurs.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:47:41 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun May 17 09:49:26 2015
// Update Count     : 2
//

#include <set>                // for set, _Rb_tree_const_iterator
#include <string>             // for string

#include "Common/PassVisitor.h"
#include "SynTree/Type.h"     // for TypeInstType, Type
#include "TypeEnvironment.h"  // for EqvClass, TypeEnvironment

namespace ResolvExpr {
	struct Occurs : public WithVisitorRef<Occurs> {
		Occurs( std::string varName, const TypeEnvironment &env );
		void previsit( const TypeInstType * typeInst );

		bool result;
		std::set< std::string > eqvVars;
		const TypeEnvironment &tenv;
	};

	bool occurs( const Type *type, const std::string & varName, const TypeEnvironment &env ) {
		PassVisitor<Occurs> occur( varName, env );
		type->accept( occur );
		return occur.pass.result;
	}

	Occurs::Occurs( std::string varName, const TypeEnvironment & env ) : result( false ), tenv( env ) {
		if ( const EqvClass *eqvClass = tenv.lookup( varName ) ) {
			eqvVars = eqvClass->vars;
		} else {
			eqvVars.insert( varName );
		} // if
	}

	void Occurs::previsit( const TypeInstType * typeInst ) {
		///   std::cerr << "searching for vars: ";
///   std::copy( eqvVars.begin(), eqvVars.end(), std::ostream_iterator< std::string >( std::cerr, " " ) );
///   std::cerr << std::endl;
		if ( eqvVars.find( typeInst->get_name() ) != eqvVars.end() ) {
			result = true;
		} else if ( const EqvClass *eqvClass = tenv.lookup( typeInst->get_name() ) ) {
			if ( eqvClass->type ) {
///       std::cerr << typeInst->get_name() << " is bound to";
///       eqvClass.type->print( std::cerr );
///       std::cerr << std::endl;
				eqvClass->type->accept( *visitor );
			} // if
		} // if
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
