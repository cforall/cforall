//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixNames.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:39:14 2019
// Update Count     : 21
//

#include "FixNames.h"

#include <memory>                  // for unique_ptr
#include <string>                  // for string, operator!=, operator==

#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "FixMain.h"               // for FixMain
#include "SymTab/Mangler.h"        // for Mangler
#include "SynTree/LinkageSpec.h"   // for Cforall, isMangled
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for FunctionDecl, ObjectDecl, Declarat...
#include "SynTree/Expression.h"    // for ConstantExpr
#include "SynTree/Label.h"         // for Label, noLabels
#include "SynTree/Statement.h"     // for ReturnStmt, CompoundStmt
#include "SynTree/Type.h"          // for Type, BasicType, Type::Qualifiers
#include "SynTree/Visitor.h"       // for Visitor, acceptAll
#include "CompilationState.h"

namespace CodeGen {
	class FixNames : public WithGuards {
	  public:
		void postvisit( ObjectDecl *objectDecl );
		void postvisit( FunctionDecl *functionDecl );

		void previsit( CompoundStmt *compoundStmt );
	  private:
		int scopeLevel = 1;

		void fixDWT( DeclarationWithType *dwt );
	};

	std::string mangle_main() {
		FunctionType* main_type;
		std::unique_ptr<FunctionDecl> mainDecl { new FunctionDecl( "main", Type::StorageClasses(), LinkageSpec::Cforall,
																   main_type = new FunctionType( Type::Qualifiers(), true ), nullptr )
				};
		main_type->get_returnVals().push_back(
			new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, 0, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr )
		);

		auto && name = SymTab::Mangler::mangle( mainDecl.get() );
		// std::cerr << name << std::endl;
		return std::move(name);
	}
	std::string mangle_main_args() {
		FunctionType* main_type;
		std::unique_ptr<FunctionDecl> mainDecl { new FunctionDecl( "main", Type::StorageClasses(), LinkageSpec::Cforall,
																   main_type = new FunctionType( Type::Qualifiers(), false ), nullptr )
				};
		main_type->get_returnVals().push_back(
			new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, 0, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr )
		);

		main_type->get_parameters().push_back(
			new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, 0, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr )
		);

		main_type->get_parameters().push_back(
			new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, 0,
			new PointerType( Type::Qualifiers(), new PointerType( Type::Qualifiers(), new BasicType( Type::Qualifiers(), BasicType::Char ) ) ),
			nullptr )
		);

		auto&& name = SymTab::Mangler::mangle( mainDecl.get() );
		// std::cerr << name << std::endl;
		return std::move(name);
	}

	bool is_main(const std::string& name) {
		static std::string mains[] = {
			mangle_main(),
			mangle_main_args()
		};

		for(const auto& m : mains) {
			if( name == m ) return true;
		}
		return false;
	}

	void fixNames( std::list< Declaration* > & translationUnit ) {
		PassVisitor<FixNames> fixer;
		acceptAll( translationUnit, fixer );
	}

	void FixNames::fixDWT( DeclarationWithType * dwt ) {
		if ( dwt->get_name() != "" ) {
			if ( LinkageSpec::isMangled( dwt->get_linkage() ) ) {
				if (!useNewAST) {
					dwt->set_mangleName( SymTab::Mangler::mangle( dwt ) );
				}
				dwt->set_scopeLevel( scopeLevel );
			} // if
		} // if
	}

	void FixNames::postvisit( ObjectDecl * objectDecl ) {
		fixDWT( objectDecl );
	}

	void FixNames::postvisit( FunctionDecl * functionDecl ) {
		fixDWT( functionDecl );

		if(is_main( SymTab::Mangler::mangle(functionDecl, true, true) )) {
			int nargs = functionDecl->get_functionType()->get_parameters().size();
			if( !(nargs == 0 || nargs == 2 || nargs == 3) ) {
				SemanticError(functionDecl, "Main expected to have 0, 2 or 3 arguments\n");
			}
			functionDecl->get_statements()->get_kids().push_back( new ReturnStmt( new ConstantExpr( Constant::from_int( 0 ) ) ) );
			CodeGen::FixMain::registerMain( functionDecl );
		}
	}

	void FixNames::previsit( CompoundStmt * ) {
		scopeLevel++;
		GuardAction( [this](){ scopeLevel--; } );
	}
} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
