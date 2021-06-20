//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeGenerator.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 19:00:42 2021
// Update Count     : 536
//
#include "CodeGenerator.h"

#include <cassert>                   // for assert, assertf
#include <list>                      // for _List_iterator, list, list<>::it...

#include "Common/UniqueName.h"       // for UniqueName
#include "Common/utility.h"          // for CodeLocation, toString
#include "GenType.h"                 // for genType
#include "InitTweak/InitTweak.h"     // for getPointerBase
#include "OperatorTable.h"           // for OperatorInfo, operatorLookup
#include "SynTree/LinkageSpec.h"     // for Spec, Intrinsic
#include "SynTree/Attribute.h"       // for Attribute
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "SynTree/Constant.h"        // for Constant
#include "SynTree/Declaration.h"     // for DeclarationWithType, TypeDecl
#include "SynTree/Expression.h"      // for Expression, UntypedExpr, Applica...
#include "SynTree/Initializer.h"     // for Initializer, ListInit, Designation
#include "SynTree/Label.h"           // for Label, operator<<
#include "SynTree/Statement.h"       // for Statement, AsmStmt, BranchStmt
#include "SynTree/Type.h"            // for Type, Type::StorageClasses, Func...

using namespace std;

namespace CodeGen {
	int CodeGenerator::tabsize = 4;

	// The kinds of statements that would ideally be followed by whitespace.
	bool wantSpacing( Statement * stmt) {
		return dynamic_cast< IfStmt * >( stmt ) || dynamic_cast< CompoundStmt * >( stmt ) ||
			dynamic_cast< WhileStmt * >( stmt ) || dynamic_cast< ForStmt * >( stmt ) || dynamic_cast< SwitchStmt *>( stmt );
	}

	void CodeGenerator::extension( Expression * expr ) {
		if ( expr->get_extension() ) {
			output << "__extension__ ";
		} // if
	} // extension

	void CodeGenerator::extension( Declaration * decl ) {
		if ( decl->get_extension() ) {
			output << "__extension__ ";
		} // if
	} // extension

	void CodeGenerator::asmName( DeclarationWithType * decl ) {
		if ( ConstantExpr * asmName = dynamic_cast<ConstantExpr *>(decl->get_asmName()) ) {
			output << " asm ( " << asmName->get_constant()->get_value() << " )";
		} // if
	} // extension

	CodeGenerator::LabelPrinter & CodeGenerator::LabelPrinter::operator()( std::list< Label > & l ) {
		labels = &l;
		return *this;
	}

	ostream & operator<<( ostream & output, CodeGenerator::LabelPrinter & printLabels ) {
		std::list< Label > & labs = *printLabels.labels;
		// l.unique(); // assumes a sorted list. Why not use set? Does order matter?
		for ( Label & l : labs ) {
			output << l.get_name() + ": ";
			printLabels.cg.genAttributes( l.get_attributes() );
		} // for
		return output;
	}

	// Using updateLocation at the beginning of a node and endl within a node should become the method of formating.
	void CodeGenerator::updateLocation( CodeLocation const & to ) {
		// skip if linemarks shouldn't appear or if codelocation is unset
		if ( !options.lineMarks || to.isUnset() ) return;

		if ( currentLocation.followedBy( to, 0 ) ) {
			return;
		} else if ( currentLocation.followedBy( to, 1 ) ) {
			output << "\n" << indent;
			currentLocation.first_line += 1;
		} else if ( currentLocation.followedBy( to, 2 ) ) {
			output << "\n\n" << indent;
			currentLocation.first_line += 2;
		} else {
			output << "\n# " << to.first_line << " \"" << to.filename
				   << "\"\n" << indent;
			currentLocation = to;
		}
		output << std::flush;
	}

	void CodeGenerator::updateLocation( BaseSyntaxNode const * to ) {
		updateLocation( to->location );
	}

	// replace endl
	ostream & CodeGenerator::LineEnder::operator()( ostream & os ) const {
		// if ( !cg.lineMarks ) {
		// 	os << "\n" << cg.indent << std::flush;
		// }
		os << "\n" << std::flush;
		cg.currentLocation.first_line++;
		// os << "/* did endl; current loc is: " << cg.currentLocation.first_line << "*/";
		return os;
	}

	CodeGenerator::CodeGenerator( std::ostream & os, bool pretty, bool genC, bool lineMarks, bool printExprTypes ) : indent( 0, CodeGenerator::tabsize ), output( os ), printLabels( *this ), options( pretty, genC, lineMarks, printExprTypes ), endl( *this ) {}
	CodeGenerator::CodeGenerator( std::ostream & os, const Options &options ) : indent( 0, CodeGenerator::tabsize ), output( os ), printLabels( *this ), options(options), endl( *this ) {}

	string CodeGenerator::mangleName( DeclarationWithType * decl ) {
		// GCC builtins should always be printed unmangled
		if ( options.pretty || decl->linkage.is_gcc_builtin ) return decl->name;
		if ( LinkageSpec::isMangled(decl->linkage) && decl->mangleName != "" ) {
			// need to incorporate scope level in order to differentiate names for destructors
			return decl->get_scopedMangleName();
		} else {
			return decl->name;
		} // if
	}

	void CodeGenerator::genAttributes( list< Attribute * > & attributes ) {
		if ( attributes.empty() ) return;
		output << "__attribute__ ((";
		for ( list< Attribute * >::iterator attr( attributes.begin() );; ) {
			output << (*attr)->name;
			if ( ! (*attr)->parameters.empty() ) {
				output << "(";
				genCommaList( (*attr)->parameters.begin(), (*attr)->parameters.end() );
				output << ")";
			} // if
			if ( ++attr == attributes.end() ) break;
			output << ",";								// separator
		} // for
		output << ")) ";
	} // CodeGenerator::genAttributes

	// *** BaseSyntaxNode
	void CodeGenerator::previsit( BaseSyntaxNode * node ) {
		// turn off automatic recursion for all nodes, to allow each visitor to
		// precisely control the order in which its children are visited.
		visit_children = false;
		updateLocation( node );
	}

	// *** BaseSyntaxNode
	void CodeGenerator::postvisit( BaseSyntaxNode * node ) {
		std::stringstream ss;
		node->print( ss );
		assertf( false, "Unhandled node reached in CodeGenerator: %s", ss.str().c_str() );
	}

	// *** Expression
	void CodeGenerator::previsit( Expression * node ) {
		previsit( (BaseSyntaxNode *)node );
		GuardAction( [this, node](){
				if ( options.printExprTypes && node->result ) {
					output << " /* " << genType( node->result, "", options ) << " */ ";
				}
			} );
	}

	// *** Declarations
	void CodeGenerator::postvisit( FunctionDecl * functionDecl ) {
		// deleted decls should never be used, so don't print them
		if ( functionDecl->isDeleted && options.genC ) return;
		extension( functionDecl );
		genAttributes( functionDecl->get_attributes() );

		handleStorageClass( functionDecl );
		functionDecl->get_funcSpec().print( output );

		Options subOptions = options;
		subOptions.anonymousUnused = functionDecl->has_body();
		output << genType( functionDecl->get_functionType(), mangleName( functionDecl ), subOptions );

		asmName( functionDecl );

		if ( functionDecl->get_statements() ) {
			functionDecl->get_statements()->accept( *visitor );
		} // if
		if ( functionDecl->isDeleted ) {
			output << " = void";
		}
	}

	void CodeGenerator::postvisit( ObjectDecl * objectDecl ) {
		// deleted decls should never be used, so don't print them
		if ( objectDecl->isDeleted && options.genC ) return;

		// gcc allows an empty declarator (no name) for bit-fields and C states: 6.7.2.1 Structure and union specifiers,
		// point 4, page 113: If the (bit field) value is zero, the declaration shall have no declarator.  For anything
		// else, the anonymous name refers to the anonymous object for plan9 inheritance.
		if ( objectDecl->get_name().empty() && options.genC && ! objectDecl->get_bitfieldWidth() ) {
			// only generate an anonymous name when generating C code, otherwise it clutters the output too much
			static UniqueName name = { "__anonymous_object" };
			objectDecl->set_name( name.newName() );
			// Stops unused parameter warnings.
			if ( options.anonymousUnused ) {
				objectDecl->attributes.push_back( new Attribute( "unused" ) );
			}
		}

		extension( objectDecl );
		genAttributes( objectDecl->get_attributes() );

		handleStorageClass( objectDecl );
		output << genType( objectDecl->get_type(), mangleName( objectDecl ), options.pretty, options.genC );

		asmName( objectDecl );

		if ( objectDecl->get_init() ) {
			output << " = ";
			objectDecl->get_init()->accept( *visitor );
		} // if
		if ( objectDecl->isDeleted ) {
			output << " = void";
		}

		if ( objectDecl->get_bitfieldWidth() ) {
			output << ":";
			objectDecl->get_bitfieldWidth()->accept( *visitor );
		} // if
	}

	void CodeGenerator::handleAggregate( AggregateDecl * aggDecl, const std::string & kind ) {
		if( ! aggDecl->parameters.empty() && ! options.genC ) {
			// assertf( ! genC, "Aggregate type parameters should not reach code generation." );
			output << "forall(";
			genCommaList( aggDecl->parameters.begin(), aggDecl->parameters.end() );
			output << ")" << endl;
			output << indent;
		}

		output << kind;
		genAttributes( aggDecl->attributes );
		output << aggDecl->name;

		if ( aggDecl->has_body() ) {
			std::list< Declaration * > & memb = aggDecl->members;
			output << " {" << endl;

			++indent;
			for ( std::list< Declaration* >::iterator i = memb.begin(); i != memb.end(); i++ ) {
				output << indent;
				(*i)->accept( *visitor );
				output << ";" << endl;
			} // for

			--indent;

			output << indent << "}";
		} // if
	}

	void CodeGenerator::postvisit( StructDecl * structDecl ) {
		extension( structDecl );
		handleAggregate( structDecl, "struct " );
	}

	void CodeGenerator::postvisit( UnionDecl * unionDecl ) {
		extension( unionDecl );
		handleAggregate( unionDecl, "union " );
	}

	void CodeGenerator::postvisit( EnumDecl * enumDecl ) {
		extension( enumDecl );
		output << "enum ";
		genAttributes( enumDecl->get_attributes() );

		output << enumDecl->get_name();

		std::list< Declaration* > &memb = enumDecl->get_members();

		if ( ! memb.empty() ) {
			output << " {" << endl;

			++indent;
			for ( std::list< Declaration* >::iterator i = memb.begin(); i != memb.end();  i++) {
				ObjectDecl * obj = dynamic_cast< ObjectDecl* >( *i );
				assert( obj );
				output << indent << mangleName( obj );
				if ( obj->get_init() ) {
					output << " = ";
					obj->get_init()->accept( *visitor );
				} // if
				output << "," << endl;
			} // for

			--indent;

			output << indent << "}";
		} // if
	}

	void CodeGenerator::postvisit( TraitDecl * traitDecl ) {
		assertf( ! options.genC, "TraitDecls should not reach code generation." );
		extension( traitDecl );
		handleAggregate( traitDecl, "trait " );
	}

	void CodeGenerator::postvisit( TypedefDecl * typeDecl ) {
		assertf( ! options.genC, "Typedefs are removed and substituted in earlier passes." );
		output << "typedef ";
		output << genType( typeDecl->get_base(), typeDecl->get_name(), options ) << endl;
	}

	void CodeGenerator::postvisit( TypeDecl * typeDecl ) {
		assertf( ! options.genC, "TypeDecls should not reach code generation." );
		output << typeDecl->genTypeString() << " " << typeDecl->name;
		if ( typeDecl->sized ) {
			output << " | sized(" << typeDecl->name << ")";
		}
		if ( ! typeDecl->assertions.empty() ) {
			output << " | { ";
			for ( DeclarationWithType * assert :  typeDecl->assertions ) {
				assert->accept( *visitor );
				output << "; ";
			}
			output << " }";
		}
	}

	void CodeGenerator::postvisit( StaticAssertDecl * assertDecl ) {
		output << "_Static_assert(";
		assertDecl->condition->accept( *visitor );
		output << ", ";
		assertDecl->message->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( Designation * designation ) {
		std::list< Expression * > designators = designation->get_designators();
		if ( designators.size() == 0 ) return;
		for ( Expression * des : designators ) {
			if ( dynamic_cast< NameExpr * >( des ) || dynamic_cast< VariableExpr * >( des ) ) {
				// if expression is a NameExpr or VariableExpr, then initializing aggregate member
				output << ".";
				des->accept( *visitor );
			} else {
				// otherwise, it has to be a ConstantExpr or CastExpr, initializing array eleemnt
				output << "[";
				des->accept( *visitor );
				output << "]";
			} // if
		} // for
		output << " = ";
	}

	void CodeGenerator::postvisit( SingleInit * init ) {
		init->get_value()->accept( *visitor );
	}

	void CodeGenerator::postvisit( ListInit * init ) {
		auto initBegin = init->begin();
		auto initEnd = init->end();
		auto desigBegin = init->get_designations().begin();
		auto desigEnd = init->get_designations().end();

		output << "{ ";
		for ( ; initBegin != initEnd && desigBegin != desigEnd; ) {
			(*desigBegin)->accept( *visitor );
			(*initBegin)->accept( *visitor );
			++initBegin, ++desigBegin;
			if ( initBegin != initEnd ) {
				output << ", ";
			}
		}
		output << " }";
		assertf( initBegin == initEnd && desigBegin == desigEnd, "Initializers and designators not the same length. %s", toString( init ).c_str() );
	}

	void CodeGenerator::postvisit( ConstructorInit * init ){
		assertf( ! options.genC, "ConstructorInit nodes should not reach code generation." );
		// pseudo-output for constructor/destructor pairs
		output << "<ctorinit>{" << endl << ++indent << "ctor: ";
		maybeAccept( init->get_ctor(), *visitor );
		output << ", " << endl << indent << "dtor: ";
		maybeAccept( init->get_dtor(), *visitor );
		output << endl << --indent << "}";
	}

	void CodeGenerator::postvisit( Constant * constant ) {
		output << constant->get_value();
	}

	// *** Expressions
	void CodeGenerator::postvisit( ApplicationExpr * applicationExpr ) {
		extension( applicationExpr );
		if ( VariableExpr * varExpr = dynamic_cast< VariableExpr* >( applicationExpr->get_function() ) ) {
			const OperatorInfo * opInfo;
			if ( varExpr->get_var()->get_linkage() == LinkageSpec::Intrinsic && ( opInfo = operatorLookup( varExpr->get_var()->get_name() ) ) ) {
				std::list< Expression* >::iterator arg = applicationExpr->get_args().begin();
				switch ( opInfo->type ) {
				  case OT_INDEX:
					assert( applicationExpr->get_args().size() == 2 );
					(*arg++)->accept( *visitor );
					output << "[";
					(*arg)->accept( *visitor );
					output << "]";
					break;

				  case OT_CALL:
					// there are no intrinsic definitions of the function call operator
					assert( false );
					break;

				  case OT_CTOR:
				  case OT_DTOR:
					if ( applicationExpr->get_args().size() == 1 ) {
						// the expression fed into a single parameter constructor or destructor may contain side
						// effects, so must still output this expression
						output << "(";
						(*arg++)->accept( *visitor );
						output << ") /* " << opInfo->inputName << " */";
					} else if ( applicationExpr->get_args().size() == 2 ) {
						// intrinsic two parameter constructors are essentially bitwise assignment
						output << "(";
						(*arg++)->accept( *visitor );
						output << opInfo->symbol;
						(*arg)->accept( *visitor );
						output << ") /* " << opInfo->inputName << " */";
					} else {
						// no constructors with 0 or more than 2 parameters
						assert( false );
					} // if
					break;

				  case OT_PREFIX:
				  case OT_PREFIXASSIGN:
					assert( applicationExpr->get_args().size() == 1 );
					output << "(";
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ")";
					break;

				  case OT_POSTFIX:
				  case OT_POSTFIXASSIGN:
					assert( applicationExpr->get_args().size() == 1 );
					(*arg)->accept( *visitor );
					output << opInfo->symbol;
					break;


				  case OT_INFIX:
				  case OT_INFIXASSIGN:
					assert( applicationExpr->get_args().size() == 2 );
					output << "(";
					(*arg++)->accept( *visitor );
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ")";
					break;

				  case OT_CONSTANT:
				  case OT_LABELADDRESS:
					// there are no intrinsic definitions of 0/1 or label addresses as functions
					assert( false );
				} // switch
			} else {
				varExpr->accept( *visitor );
				output << "(";
				genCommaList( applicationExpr->get_args().begin(), applicationExpr->get_args().end() );
				output << ")";
			} // if
		} else {
			applicationExpr->get_function()->accept( *visitor );
			output << "(";
			genCommaList( applicationExpr->get_args().begin(), applicationExpr->get_args().end() );
			output << ")";
		} // if
	}

	void CodeGenerator::postvisit( UntypedExpr * untypedExpr ) {
		extension( untypedExpr );
		if ( NameExpr * nameExpr = dynamic_cast< NameExpr* >( untypedExpr->function ) ) {
			const OperatorInfo * opInfo = operatorLookup( nameExpr->name );
			if ( opInfo ) {
				std::list< Expression* >::iterator arg = untypedExpr->args.begin();
				switch ( opInfo->type ) {
				  case OT_INDEX:
					assert( untypedExpr->args.size() == 2 );
					(*arg++)->accept( *visitor );
					output << "[";
					(*arg)->accept( *visitor );
					output << "]";
					break;

				  case OT_CALL:
					assert( false );

				  case OT_CTOR:
				  case OT_DTOR:
					if ( untypedExpr->args.size() == 1 ) {
						// the expression fed into a single parameter constructor or destructor may contain side
						// effects, so must still output this expression
						output << "(";
						(*arg++)->accept( *visitor );
						output << ") /* " << opInfo->inputName << " */";
					} else if ( untypedExpr->get_args().size() == 2 ) {
						// intrinsic two parameter constructors are essentially bitwise assignment
						output << "(";
						(*arg++)->accept( *visitor );
						output << opInfo->symbol;
						(*arg)->accept( *visitor );
						output << ") /* " << opInfo->inputName << " */";
					} else {
						// no constructors with 0 or more than 2 parameters
						assertf( ! options.genC, "UntypedExpr constructor/destructor with 0 or more than 2 parameters." );
						output << "(";
						(*arg++)->accept( *visitor );
						output << opInfo->symbol << "{ ";
						genCommaList( arg, untypedExpr->args.end() );
						output << "}) /* " << opInfo->inputName << " */";
					} // if
					break;

				  case OT_PREFIX:
				  case OT_PREFIXASSIGN:
				  case OT_LABELADDRESS:
					assert( untypedExpr->args.size() == 1 );
					output << "(";
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ")";
					break;

				  case OT_POSTFIX:
				  case OT_POSTFIXASSIGN:
					assert( untypedExpr->args.size() == 1 );
					(*arg)->accept( *visitor );
					output << opInfo->symbol;
					break;

				  case OT_INFIX:
				  case OT_INFIXASSIGN:
					assert( untypedExpr->args.size() == 2 );
					output << "(";
					(*arg++)->accept( *visitor );
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ")";
					break;

				  case OT_CONSTANT:
					// there are no intrinsic definitions of 0 or 1 as functions
					assert( false );
				} // switch
			} else {
				// builtin routines
				nameExpr->accept( *visitor );
				output << "(";
				genCommaList( untypedExpr->args.begin(), untypedExpr->args.end() );
				output << ")";
			} // if
		} else {
			untypedExpr->function->accept( *visitor );
			output << "(";
			genCommaList( untypedExpr->args.begin(), untypedExpr->args.end() );
			output << ")";
		} // if
	}

	void CodeGenerator::postvisit( RangeExpr * rangeExpr ) {
		rangeExpr->low->accept( *visitor );
		output << " ... ";
		rangeExpr->high->accept( *visitor );
	}

	void CodeGenerator::postvisit( NameExpr * nameExpr ) {
		extension( nameExpr );
		const OperatorInfo * opInfo = operatorLookup( nameExpr->name );
		if ( opInfo ) {
			if ( opInfo->type == OT_CONSTANT ) {
				output << opInfo->symbol;
			} else {
				output << opInfo->outputName;
			}
		} else {
			output << nameExpr->get_name();
		} // if
	}

	void CodeGenerator::postvisit( DimensionExpr * dimensionExpr ) {
		extension( dimensionExpr );
		output << "/*non-type*/" << dimensionExpr->get_name();
	}

	void CodeGenerator::postvisit( AddressExpr * addressExpr ) {
		extension( addressExpr );
		output << "(&";
		addressExpr->arg->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( LabelAddressExpr *addressExpr ) {
		extension( addressExpr );
		output << "(&&" << addressExpr->arg << ")";
	}

	void CodeGenerator::postvisit( CastExpr * castExpr ) {
		extension( castExpr );
		output << "(";
		if ( castExpr->get_result()->isVoid() ) {
			output << "(void)";
		} else {
			// at least one result type of cast.
			// Note: previously, lvalue casts were skipped. Since it's now impossible for the user to write
			// an lvalue cast, this has been taken out.
			output << "(";
			output << genType( castExpr->get_result(), "", options );
			output << ")";
		} // if
		castExpr->arg->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( KeywordCastExpr * castExpr ) {
		assertf( ! options.genC, "KeywordCast should not reach code generation." );
		extension( castExpr );
		output << "((" << castExpr->targetString() << " &)";
		castExpr->arg->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( VirtualCastExpr * castExpr ) {
		assertf( ! options.genC, "VirtualCastExpr should not reach code generation." );
		extension( castExpr );
		output << "(virtual ";
		castExpr->get_arg()->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( UntypedMemberExpr * memberExpr ) {
		assertf( ! options.genC, "UntypedMemberExpr should not reach code generation." );
		extension( memberExpr );
		memberExpr->get_aggregate()->accept( *visitor );
		output << ".";
		memberExpr->get_member()->accept( *visitor );
	}

	void CodeGenerator::postvisit( MemberExpr * memberExpr ) {
		extension( memberExpr );
		memberExpr->get_aggregate()->accept( *visitor );
		output << "." << mangleName( memberExpr->get_member() );
	}

	void CodeGenerator::postvisit( VariableExpr * variableExpr ) {
		extension( variableExpr );
		const OperatorInfo * opInfo;
		if ( variableExpr->get_var()->get_linkage() == LinkageSpec::Intrinsic && (opInfo = operatorLookup( variableExpr->get_var()->get_name() )) && opInfo->type == OT_CONSTANT ) {
			output << opInfo->symbol;
		} else {
			output << mangleName( variableExpr->get_var() );
		} // if
	}

	void CodeGenerator::postvisit( ConstantExpr * constantExpr ) {
		assert( constantExpr->get_constant() );
		extension( constantExpr );
		constantExpr->get_constant()->accept( *visitor );
	}

	void CodeGenerator::postvisit( SizeofExpr * sizeofExpr ) {
		extension( sizeofExpr );
		output << "sizeof(";
		if ( sizeofExpr->get_isType() ) {
			output << genType( sizeofExpr->get_type(), "", options );
		} else {
			sizeofExpr->get_expr()->accept( *visitor );
		} // if
		output << ")";
	}

	void CodeGenerator::postvisit( AlignofExpr * alignofExpr ) {
		// use GCC extension to avoid bumping std to C11
		extension( alignofExpr );
		output << "__alignof__(";
		if ( alignofExpr->get_isType() ) {
			output << genType( alignofExpr->get_type(), "", options );
		} else {
			alignofExpr->get_expr()->accept( *visitor );
		} // if
		output << ")";
	}

	void CodeGenerator::postvisit( UntypedOffsetofExpr * offsetofExpr ) {
		assertf( ! options.genC, "UntypedOffsetofExpr should not reach code generation." );
		output << "offsetof(";
		output << genType( offsetofExpr->get_type(), "", options );
		output << ", " << offsetofExpr->get_member();
		output << ")";
	}

	void CodeGenerator::postvisit( OffsetofExpr * offsetofExpr ) {
		// use GCC builtin
		output << "__builtin_offsetof(";
		output << genType( offsetofExpr->get_type(), "", options );
		output << ", " << mangleName( offsetofExpr->get_member() );
		output << ")";
	}

	void CodeGenerator::postvisit( OffsetPackExpr * offsetPackExpr ) {
		assertf( ! options.genC, "OffsetPackExpr should not reach code generation." );
		output << "__CFA_offsetpack(" << genType( offsetPackExpr->get_type(), "", options ) << ")";
	}

	void CodeGenerator::postvisit( LogicalExpr * logicalExpr ) {
		extension( logicalExpr );
		output << "(";
		logicalExpr->get_arg1()->accept( *visitor );
		if ( logicalExpr->get_isAnd() ) {
			output << " && ";
		} else {
			output << " || ";
		} // if
		logicalExpr->get_arg2()->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( ConditionalExpr * conditionalExpr ) {
		extension( conditionalExpr );
		output << "(";
		conditionalExpr->get_arg1()->accept( *visitor );
		output << " ? ";
		conditionalExpr->get_arg2()->accept( *visitor );
		output << " : ";
		conditionalExpr->get_arg3()->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( CommaExpr * commaExpr ) {
		extension( commaExpr );
		output << "(";
		if ( options.genC ) {
			// arg1 of a CommaExpr is never used, so it can be safely cast to void to reduce gcc warnings.
			commaExpr->set_arg1( new CastExpr( commaExpr->get_arg1() ) );
		}
		commaExpr->get_arg1()->accept( *visitor );
		output << " , ";
		commaExpr->get_arg2()->accept( *visitor );
		output << ")";
	}

	void CodeGenerator::postvisit( TupleAssignExpr * tupleExpr ) {
		assertf( ! options.genC, "TupleAssignExpr should not reach code generation." );
		tupleExpr->stmtExpr->accept( *visitor );
	}

	void CodeGenerator::postvisit( UntypedTupleExpr * tupleExpr ) {
		assertf( ! options.genC, "UntypedTupleExpr should not reach code generation." );
		extension( tupleExpr );
		output << "[";
		genCommaList( tupleExpr->get_exprs().begin(), tupleExpr->get_exprs().end() );
		output << "]";
	}

	void CodeGenerator::postvisit( TupleExpr * tupleExpr ) {
		assertf( ! options.genC, "TupleExpr should not reach code generation." );
		extension( tupleExpr );
		output << "[";
		genCommaList( tupleExpr->get_exprs().begin(), tupleExpr->get_exprs().end() );
		output << "]";
	}

	void CodeGenerator::postvisit( TupleIndexExpr * tupleExpr ) {
		assertf( ! options.genC, "TupleIndexExpr should not reach code generation." );
		extension( tupleExpr );
		tupleExpr->get_tuple()->accept( *visitor );
		output << "." << tupleExpr->get_index();
	}

	void CodeGenerator::postvisit( TypeExpr * typeExpr ) {
		// if ( options.genC ) std::cerr << "typeexpr still exists: " << typeExpr << std::endl;
		// assertf( ! options.genC, "TypeExpr should not reach code generation." );
		if ( ! options.genC ) {
			output << genType( typeExpr->get_type(), "", options );
		}
	}

	void CodeGenerator::postvisit( AsmExpr * asmExpr ) {
		if ( !asmExpr->inout.empty() ) {
			output << "[ ";
			output << asmExpr->inout;
			output << " ] ";
		} // if
		asmExpr->constraint->accept( *visitor );
		output << " ( ";
		asmExpr->operand->accept( *visitor );
		output << " )";
	}

	void CodeGenerator::postvisit( CompoundLiteralExpr *compLitExpr ) {
		assert( compLitExpr->get_result() && dynamic_cast< ListInit * > ( compLitExpr->get_initializer() ) );
		output << "(" << genType( compLitExpr->get_result(), "", options ) << ")";
		compLitExpr->get_initializer()->accept( *visitor );
	}

	void CodeGenerator::postvisit( UniqueExpr * unqExpr ) {
		assertf( ! options.genC, "Unique expressions should not reach code generation." );
		output << "unq<" << unqExpr->get_id() << ">{ ";
		unqExpr->get_expr()->accept( *visitor );
		output << " }";
	}

	void CodeGenerator::postvisit( StmtExpr * stmtExpr ) {
		std::list< Statement * > & stmts = stmtExpr->statements->kids;
		output << "({" << endl;
		++indent;
		unsigned int numStmts = stmts.size();
		unsigned int i = 0;
		for ( Statement * stmt : stmts ) {
			output << indent << printLabels( stmt->get_labels() );
			if ( i+1 == numStmts ) {
				// last statement in a statement expression needs to be handled specially -
				// cannot cast to void, otherwise the expression statement has no value
				if ( ExprStmt * exprStmt = dynamic_cast< ExprStmt * >( stmt ) ) {
					exprStmt->expr->accept( *visitor );
					output << ";" << endl;
					++i;
					break;
				}
			}
			stmt->accept( *visitor );
			output << endl;
			if ( wantSpacing( stmt ) ) {
				output << endl;
			} // if
			++i;
		}
		--indent;
		output << indent << "})";
	}

	void CodeGenerator::postvisit( ConstructorExpr * expr ) {
		assertf( ! options.genC, "Unique expressions should not reach code generation." );
		expr->callExpr->accept( *visitor );
	}

	void CodeGenerator::postvisit( DeletedExpr * expr ) {
		assertf( ! options.genC, "Deleted expressions should not reach code generation." );
		expr->expr->accept( *visitor );
	}

	void CodeGenerator::postvisit( DefaultArgExpr * arg ) {
		assertf( ! options.genC, "Default argument expressions should not reach code generation." );
		arg->expr->accept( *visitor );
	}

	void CodeGenerator::postvisit( GenericExpr * expr ) {
		assertf( ! options.genC, "C11 _Generic expressions should not reach code generation." );
		output << "_Generic(";
		expr->control->accept( *visitor );
		output << ", ";
		unsigned int numAssocs = expr->associations.size();
		unsigned int i = 0;
		for ( GenericExpr::Association & assoc : expr->associations ) {
			if (assoc.isDefault) {
				output << "default: ";
			} else {
				output << genType( assoc.type, "", options ) << ": ";
			}
			assoc.expr->accept( *visitor );
			if ( i+1 != numAssocs ) {
				output << ", ";
			}
			i++;
		}
		output << ")";
	}


	// *** Statements
	void CodeGenerator::postvisit( CompoundStmt * compoundStmt ) {
		std::list<Statement*> ks = compoundStmt->get_kids();
		output << "{" << endl;

		++indent;

		for ( std::list<Statement *>::iterator i = ks.begin(); i != ks.end();  i++ ) {
			output << indent << printLabels( (*i)->get_labels() );
			(*i)->accept( *visitor );

			output << endl;
			if ( wantSpacing( *i ) ) {
				output << endl;
			} // if
		} // for
		--indent;

		output << indent << "}";
	}

	void CodeGenerator::postvisit( ExprStmt * exprStmt ) {
		assert( exprStmt );
		if ( options.genC ) {
			// cast the top-level expression to void to reduce gcc warnings.
			exprStmt->set_expr( new CastExpr( exprStmt->get_expr() ) );
		}
		exprStmt->get_expr()->accept( *visitor );
		output << ";";
	}

	void CodeGenerator::postvisit( AsmStmt * asmStmt ) {
		output << "asm ";
		if ( asmStmt->get_voltile() ) output << "volatile ";
		if ( ! asmStmt->get_gotolabels().empty()  ) output << "goto ";
		output << "( ";
		if ( asmStmt->get_instruction() ) asmStmt->get_instruction()->accept( *visitor );
		output << " : ";
		genCommaList( asmStmt->get_output().begin(), asmStmt->get_output().end() );
		output << " : ";
		genCommaList( asmStmt->get_input().begin(), asmStmt->get_input().end() );
		output << " : ";
		genCommaList( asmStmt->get_clobber().begin(), asmStmt->get_clobber().end() );
		if ( ! asmStmt->get_gotolabels().empty() ) {
			output << " : ";
			for ( std::list<Label>::iterator begin = asmStmt->get_gotolabels().begin();; ) {
				output << *begin++;
				if ( begin == asmStmt->get_gotolabels().end() ) break;
				output << ", ";
			} // for
		} // if
		output << " );";
	}

	void CodeGenerator::postvisit( AsmDecl * asmDecl ) {
		output << "asm ";
		AsmStmt * asmStmt = asmDecl->get_stmt();
		output << "( ";
		if ( asmStmt->get_instruction() ) asmStmt->get_instruction()->accept( *visitor );
		output << " )";
	}

	void CodeGenerator::postvisit( DirectiveDecl * directiveDecl ) {
		output << endl << directiveDecl->get_stmt()->directive;	// endl prevents spaces before directive
	}

	void CodeGenerator::postvisit( DirectiveStmt * dirStmt ) {
		output << endl << dirStmt->directive;			// endl prevents spaces before directive
	}

	void CodeGenerator::postvisit( IfStmt * ifStmt ) {
		output << "if ( ";
		ifStmt->get_condition()->accept( *visitor );
		output << " ) ";

		ifStmt->get_thenPart()->accept( *visitor );

		if ( ifStmt->get_elsePart() != 0) {
			output << " else ";
			ifStmt->get_elsePart()->accept( *visitor );
		} // if
	}

	void CodeGenerator::postvisit( SwitchStmt * switchStmt ) {
		output << "switch ( ";
		switchStmt->get_condition()->accept( *visitor );
		output << " ) ";

		output << "{" << endl;
		++indent;
		acceptAll( switchStmt->get_statements(), *visitor );
		--indent;
		output << indent << "}";
	}

	void CodeGenerator::postvisit( CaseStmt * caseStmt ) {
		updateLocation( caseStmt );
		output << indent;
		if ( caseStmt->isDefault()) {
			output << "default";
		} else {
			output << "case ";
			caseStmt->get_condition()->accept( *visitor );
		} // if
		output << ":" << endl;

		std::list<Statement *> sts = caseStmt->get_statements();

		++indent;
		for ( std::list<Statement *>::iterator i = sts.begin(); i != sts.end();  i++) {
			output << indent << printLabels( (*i)->get_labels() ) ;
			(*i)->accept( *visitor );
			output << endl;
		} // for
		--indent;
	}

	void CodeGenerator::postvisit( BranchStmt * branchStmt ) {
		switch ( branchStmt->get_type()) {
		  case BranchStmt::Goto:
			if ( ! branchStmt->get_target().empty() )
				output << "goto " << branchStmt->get_target();
			else {
				if ( branchStmt->get_computedTarget() != 0 ) {
					output << "goto *";
					branchStmt->get_computedTarget()->accept( *visitor );
				} // if
			} // if
			break;
		  case BranchStmt::Break:
			output << "break";
			break;
		  case BranchStmt::Continue:
			output << "continue";
			break;
		  case BranchStmt::FallThrough:
		  case BranchStmt::FallThroughDefault:
			assertf( ! options.genC, "fallthru should not reach code generation." );
			output << "fallthru";
			break;
		} // switch
		// print branch target for labelled break/continue/fallthru in debug mode
		if ( ! options.genC && branchStmt->get_type() != BranchStmt::Goto ) {
			if ( ! branchStmt->get_target().empty() ) {
				output << " " << branchStmt->get_target();
			} else if ( branchStmt->get_type() == BranchStmt::FallThrough ) {
				output << " default";
			}
		}
		output << ";";
	}

	void CodeGenerator::postvisit( ReturnStmt * returnStmt ) {
		output << "return ";
		maybeAccept( returnStmt->get_expr(), *visitor );
		output << ";";
	}

	void CodeGenerator::postvisit( ThrowStmt * throwStmt ) {
		assertf( ! options.genC, "Throw statements should not reach code generation." );

		output << ((throwStmt->get_kind() == ThrowStmt::Terminate) ?
				   "throw" : "throwResume");
		if (throwStmt->get_expr()) {
			output << " ";
			throwStmt->get_expr()->accept( *visitor );
		}
		if (throwStmt->get_target()) {
			output << " _At ";
			throwStmt->get_target()->accept( *visitor );
		}
		output << ";";
	}
	void CodeGenerator::postvisit( CatchStmt * stmt ) {
		assertf( ! options.genC, "Catch statements should not reach code generation." );

		output << ((stmt->get_kind() == CatchStmt::Terminate) ?
				   "catch" : "catchResume");
		output << "( ";
		stmt->decl->accept( *visitor );
		output << " ) ";

		if( stmt->cond ) {
			output << "if/when(?) (";
			stmt->cond->accept( *visitor );
			output << ") ";
		}
		stmt->body->accept( *visitor );
	}

	void CodeGenerator::postvisit( WaitForStmt * stmt ) {
		assertf( ! options.genC, "Waitfor statements should not reach code generation." );

		bool first = true;
		for( auto & clause : stmt->clauses ) {
			if(first) { output << "or "; first = false; }
			if( clause.condition ) {
				output << "when(";
				stmt->timeout.condition->accept( *visitor );
				output << ") ";
			}
			output << "waitfor(";
			clause.target.function->accept( *visitor );
			for( Expression * expr : clause.target.arguments ) {
				output << ",";
				expr->accept( *visitor );
			}
			output << ") ";
			clause.statement->accept( *visitor );
		}

		if( stmt->timeout.statement ) {
			output << "or ";
			if( stmt->timeout.condition ) {
				output << "when(";
				stmt->timeout.condition->accept( *visitor );
				output << ") ";
			}
			output << "timeout(";
			stmt->timeout.time->accept( *visitor );
			output << ") ";
			stmt->timeout.statement->accept( *visitor );
		}

		if( stmt->orelse.statement ) {
			output << "or ";
			if( stmt->orelse.condition ) {
				output << "when(";
				stmt->orelse.condition->accept( *visitor );
				output << ")";
			}
			output << "else ";
			stmt->orelse.statement->accept( *visitor );
		}
	}

	void CodeGenerator::postvisit( WithStmt * with ) {
		if ( ! options.genC ) {
			output << "with ( ";
			genCommaList( with->exprs.begin(), with->exprs.end() );
			output << " ) ";
		}
		with->stmt->accept( *visitor );
	}

	void CodeGenerator::postvisit( WhileStmt * whileStmt ) {
		if ( whileStmt->get_isDoWhile() ) {
			output << "do";
		} else {
			output << "while (";
			whileStmt->get_condition()->accept( *visitor );
			output << ")";
		} // if
		output << " ";

		output << CodeGenerator::printLabels( whileStmt->get_body()->get_labels() );
		whileStmt->get_body()->accept( *visitor );

		output << indent;

		if ( whileStmt->get_isDoWhile() ) {
			output << " while (";
			whileStmt->get_condition()->accept( *visitor );
			output << ");";
		} // if
	}

	void CodeGenerator::postvisit( ForStmt * forStmt ) {
		// initialization is always hoisted, so don't bother doing anything with that
		output << "for (;";

		if ( forStmt->get_condition() != 0 ) {
			forStmt->get_condition()->accept( *visitor );
		} // if
		output << ";";

		if ( forStmt->get_increment() != 0 ) {
			// cast the top-level expression to void to reduce gcc warnings.
			Expression * expr = new CastExpr( forStmt->get_increment() );
			expr->accept( *visitor );
		} // if
		output << ") ";

		if ( forStmt->get_body() != 0 ) {
			output << CodeGenerator::printLabels( forStmt->get_body()->get_labels() );
			forStmt->get_body()->accept( *visitor );
		} // if
	}

	void CodeGenerator::postvisit( __attribute__((unused)) NullStmt * nullStmt ) {
		//output << indent << CodeGenerator::printLabels( nullStmt->get_labels() );
		output << "/* null statement */ ;";
	}

	void CodeGenerator::postvisit( DeclStmt * declStmt ) {
		declStmt->get_decl()->accept( *visitor );

		if ( doSemicolon( declStmt->get_decl() ) ) {
			output << ";";
		} // if
	}

	void CodeGenerator::postvisit( ImplicitCtorDtorStmt * stmt ) {
		assertf( ! options.genC, "ImplicitCtorDtorStmts should not reach code generation." );
		stmt->callStmt->accept( *visitor );
	}

	void CodeGenerator::handleStorageClass( DeclarationWithType * decl ) {
		if ( decl->get_storageClasses().any() ) {
			decl->get_storageClasses().print( output );
		} // if
	} // CodeGenerator::handleStorageClass

	std::string genName( DeclarationWithType * decl ) {
		const OperatorInfo * opInfo = operatorLookup( decl->get_name() );
		if ( opInfo ) {
			return opInfo->outputName;
		} else {
			return decl->get_name();
		} // if
	}
} // namespace CodeGen


unsigned Indenter::tabsize = 2;

std::ostream & operator<<( std::ostream & out, const BaseSyntaxNode * node ) {
	if ( node ) {
		node->print( out );
	} else {
		out << "nullptr";
	}
	return out;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
