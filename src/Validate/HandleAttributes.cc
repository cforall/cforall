//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HandleAttributes.cc --
//
// Author           : Rob Schluntz
// Created On       : Fri Jul 27 10:15:06 2018
// Last Modified By : Rob Schluntz
// Last Modified On : Fri Jul 27 10:16:43 2018
// Update Count     : 2
//

#include "HandleAttributes.h"

#include "CompilationState.h"
#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"
#include "ResolvExpr/Resolver.h"
#include "SynTree/Attribute.h"
#include "SynTree/Declaration.h"
#include "SynTree/Type.h"

namespace Validate {
	namespace {
		struct HandleAttributes : public WithIndexer {
			void previsit( ObjectDecl * decl );
			void previsit( FunctionDecl * decl );
		};
	} // namespace

	void handleAttributes( std::list< Declaration * > &translationUnit ) {
		PassVisitor<HandleAttributes> handler;
		acceptAll( translationUnit, handler );
	}

	namespace {
		void HandleAttributes::previsit( ObjectDecl * decl ) {
			for ( Attribute * attr : decl->attributes ) {
				std::string name = attr->normalizedName();
				if (name == "init_priority") {
					// TODO: implement C++-like init_priority attribute
				}
			}
		}

		void HandleAttributes::previsit( FunctionDecl * decl ) {
			for ( Attribute * attr : decl->attributes ) {
				std::string name = attr->normalizedName();
				if (name == "constructor" || name == "destructor") {
					if (attr->parameters.size() == 1) {
						Expression *& arg = attr->parameters.front();
						ResolvExpr::findSingleExpression( arg, new BasicType( Type::Qualifiers(), BasicType::LongLongSignedInt ), indexer );
						auto result = eval(arg);
						if (! result.second) {
							SemanticWarning(attr->location, Warning::GccAttributes,
								toCString( name, " priorities must be integers from 0 to 65535 inclusive: ", arg ) );
							return;
						}
						auto priority = result.first;
						if (priority < 101) {
							SemanticWarning(attr->location, Warning::GccAttributes,
								toCString( name, " priorities from 0 to 100 are reserved for the implementation" ) );
						} else if (priority < 201 && ! buildingLibrary()) {
							SemanticWarning(attr->location, Warning::GccAttributes,
								toCString( name, " priorities from 101 to 200 are reserved for the implementation" ) );
						}
					} else if (attr->parameters.size() > 1) {
						SemanticWarning(attr->location, Warning::GccAttributes, toCString( "too many arguments to ", name, " attribute" ) );
					} else {
						SemanticWarning(attr->location, Warning::GccAttributes, toCString( "too few arguments to ", name, " attribute" ) );
					}
				}
			}
		}
	} // namespace
} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
