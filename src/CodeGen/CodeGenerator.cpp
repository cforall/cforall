//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeGenerator.cpp --
//
// Author           : Andrew Beach
// Created On       : Tue Oct 17 15:54:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct 25 18:28:00 2023
// Update Count     : 0
//

#include "CodeGenerator.hpp"

#include "AST/Print.hpp"
#include "OperatorTable.h"           // for OperatorInfo, operatorLookup
#include "CodeGen/GenType.h"         // for genType
#include "Common/ToString.hpp"       // for toString
#include "Common/UniqueName.h"       // for UniqueName

namespace CodeGen {

int CodeGenerator::tabsize = 4;

// The kinds of statements that should be followed by whitespace.
static bool wantSpacing( ast::Stmt const * stmt ) {
	return dynamic_cast<ast::IfStmt const *>( stmt )
		|| dynamic_cast<ast::CompoundStmt const *>( stmt )
		|| dynamic_cast<ast::WhileDoStmt const *>( stmt )
		|| dynamic_cast<ast::ForStmt const *>( stmt )
		|| dynamic_cast<ast::SwitchStmt const *>( stmt );
}

void CodeGenerator::extension( ast::Expr const * expr ) {
	if ( expr->extension ) output << "__extension__ ";
}

void CodeGenerator::extension( ast::Decl const * decl ) {
	if ( decl->extension ) output << "__extension__ ";
}

void CodeGenerator::asmName( ast::DeclWithType const * decl ) {
	if ( auto asmName = decl->asmName.as<ast::ConstantExpr>() ) {
		output << " asm ( " << asmName->rep << " )";
	}
}

CodeGenerator::LabelPrinter & CodeGenerator::LabelPrinter::operator()(
		std::vector<ast::Label> const & l ) {
	labels = &l;
	return *this;
}

std::ostream & CodeGenerator::LabelPrinter::operator()( std::ostream & output ) const {
	const std::vector<ast::Label> & labels = *this->labels;
	for ( const ast::Label & label : labels ) {
		output << label.name + ": ";
		this->cg.genAttributes( label.attributes );
	}
	return output;
}

// Using updateLocation at the beginning of a node and endl within a node
// should become the method of formating.
void CodeGenerator::updateLocation( CodeLocation const & to ) {
	// Skip if linemarks shouldn't appear or if location is unset.
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

void CodeGenerator::updateLocation( ast::ParseNode const * to ) {
	updateLocation( to->location );
}

std::ostream & CodeGenerator::LineEnder::operator()( std::ostream & os ) const {
	os << "\n" << std::flush;
	cg.currentLocation.first_line++;
	return os;
}

CodeGenerator::CodeGenerator( std::ostream & os, const Options & options ) :
		indent( 0, CodeGenerator::tabsize ), output( os ),
		options( options ), printLabels( *this ), endl( *this )
{}

std::string CodeGenerator::mangleName( ast::DeclWithType const * decl ) {
	if ( !options.pretty && decl->linkage.is_mangled && decl->mangleName != "" ) {
		return decl->scopedMangleName();
	} else {
		return decl->name;
	}
}

void CodeGenerator::genAttributes(
		const std::vector<ast::ptr<ast::Attribute>> & attributes ) {
	if ( attributes.empty() ) return;
	output << "__attribute__ ((";
	for ( auto attr = attributes.begin() ;; ) {
		output << (*attr)->name;
		if ( !(*attr)->params.empty() ) {
			output << "(";
			genCommaList( (*attr)->params );
			output << ")";
		}
		if ( ++attr == attributes.end() ) break;
		output << ",";
	}
	output << ")) ";
}

void CodeGenerator::previsit( ast::Node const * ) {
	// All traversal is manual.
	// TODO: Which means the ast::Pass is just providing a default no visit?
	visit_children = false;
}

void CodeGenerator::previsit( ast::ParseNode const * node ) {
	previsit( (ast::Node const *)node );
	updateLocation( node );
}

void CodeGenerator::postvisit( ast::Node const * node ) {
	std::stringstream ss;
	ast::print( ss, node );
	assertf( false, "Unhandled node reached in CodeGenerator: %s", ss.str().c_str() );
}

void CodeGenerator::previsit( ast::Expr const * expr ) {
	previsit( (ast::ParseNode const *)expr );
	GuardAction( [this, expr](){
		if ( options.printExprTypes && expr->result ) {
			output << " /* " << genType( expr->result, "", options ) << " */ ";
		}
	} );
}

void CodeGenerator::postvisit( ast::FunctionDecl const * decl ) {
	// Deleted decls should never be used, so don't print them in C.
	if ( decl->isDeleted && options.genC ) return;
	extension( decl );
	genAttributes( decl->attributes );

	handleStorageClass( decl );
	ast::print( output, decl->funcSpec );

	Options subOptions = options;
	subOptions.anonymousUnused = decl->stmts;

	std::ostringstream acc;
	ast::Pass<CodeGenerator> subCG( acc, subOptions );
	// Add the forall clause.
	// TODO: These probably should be removed by now and the assert used.
	if ( !decl->type_params.empty() ) {
		assertf( !options.genC, "FunctionDecl forall should not reach code generation." );
		acc << "forall(";
		subCG.core.genCommaList( decl->type_params );
		acc << ")" << std::endl;
	}

	acc << mangleName( decl );

	if ( 0 == decl->params.size() ) {
		if ( decl->type->isVarArgs ) {
			acc << "()";
		} else {
			acc << "(void)";
		}
	} else {
		acc << "(";
		subCG.core.genCommaList( decl->params );
		if ( decl->type->isVarArgs ) {
			acc << ", ...";
		}
		acc << ")";
	}

	assert( decl->returns.size() < 2 );
	if ( 1 == decl->returns.size() ) {
		ast::ptr<ast::Type> const & type = decl->returns[0]->get_type();
		output << genTypeNoAttr( type, acc.str(), subOptions );
	} else {
		output << "void " + acc.str();
	}

	asmName( decl );

	if ( decl->stmts ) {
		decl->stmts->accept( *visitor );
	}
	if ( decl->isDeleted ) {
		output << " = void";
	}
}

ast::ObjectDecl const * CodeGenerator::postvisit(
		ast::ObjectDecl const * decl ) {
	// Deleted decls should never be used, so don't print them in C.
	if ( decl->isDeleted && options.genC ) return decl;

	// GCC allows an empty declarator (no name) for bit-fields and C
	// states: 6.7.2.1 Structure and union specifiers, point 4, page 113:
	// If the (bit field) value is zero, the declaration shall have no
	// declarator. For anything else, the anonymous name refers to the
	// anonymous object for plan9 inheritance.
	if ( decl->name.empty() && options.genC && !decl->bitfieldWidth ) {
		// TODO: Should this be changed in a pervious pass?
		auto mutDecl = ast::mutate( decl );
		// Only generate an anonymous name when generating C code,
		// otherwise it clutters the output too much.
		static UniqueName name = { "__anonymous_object" };
		mutDecl->name = name.newName();
		// Stops unused parameter warnings.
		if ( options.anonymousUnused ) {
			mutDecl->attributes.push_back( new ast::Attribute( "unused" ) );
		}
		decl = mutDecl;
	}

	extension( decl );
	genAttributes( decl->attributes );

	handleStorageClass( decl );
	output << genType( decl->type, mangleName( decl ),
		Options( options.pretty, options.genC, false, false ) );

	asmName( decl );

	if ( decl->init ) {
		output << " = ";
		decl->init->accept( *visitor );
	}
	if ( decl->isDeleted ) {
		output << " = void";
	}

	if ( decl->bitfieldWidth ) {
		output << ":";
		decl->bitfieldWidth->accept( *visitor );
	}
	return decl;
}

void CodeGenerator::handleStorageClass( ast::DeclWithType const * decl ) {
	if ( decl->storage.any() ) {
		ast::print( output, decl->storage );
	}
}

void CodeGenerator::handleAggregate(
		ast::AggregateDecl const * decl, std::string const & kind ) {
	if ( !decl->params.empty() && !options.genC ) {
		output << "forall(";
		genCommaList( decl->params );
		output << ")\n" << indent << std::flush;
	}

	output << kind;
	genAttributes( decl->attributes );
	output << decl->name;

	if ( decl->body ) {
		auto & members = decl->members;
		output << " {" << endl;

		++indent;
		for ( auto & member : members ) {
			output << indent;
			member->accept( *visitor );
			output << ";" << endl;
		}
		--indent;

		output << indent << "}";
	}
}

void CodeGenerator::postvisit( ast::StructDecl const * decl ) {
	extension( decl );
	handleAggregate( decl, "struct " );
}

void CodeGenerator::postvisit( ast::UnionDecl const * decl ) {
	extension( decl );
	handleAggregate( decl, "union " );
}

template<typename pass_type>
inline void genEnumInitializer( ast::Pass<pass_type> * visitor,
		ast::Type const * baseType, std::ostream & output,
		ast::Init const * init, long long * curVal, Options options ) {
	auto baseTypeAsBasic = dynamic_cast<ast::BasicType const *>( baseType );
	// Value is provided.
	if ( init ) {
		output << " = (" << genType( baseType, "", options ) << ")";
		init->accept( *visitor );
		// If it is an integral type and initilizer offered,
		// need to update the curVal.
		if ( baseTypeAsBasic && baseTypeAsBasic->isInteger() ) {
			ast::Expr const * expr = ((ast::SingleInit const *)(init))->value;
			// Unwrap introduced cast.
			while ( auto temp = dynamic_cast<ast::CastExpr const *>( expr ) ) {
				expr = temp->arg;
			}
			*curVal = ((ast::ConstantExpr const *)(expr))->intValue() + 1;
		}
	// Generate next value from previous value.
	} else if ( baseTypeAsBasic && baseTypeAsBasic->isInteger() ) {
		output << " = (" << genType( baseType, "", options ) << ")";
		output << (*curVal)++;
	}
}

void CodeGenerator::postvisit( ast::EnumDecl const * decl ) {
	extension( decl );
	auto members = decl->members;
	if ( decl->base && !members.empty() ) {
		long long curVal = 0;
		for ( auto member : members ) {
			auto obj = member.strict_as<ast::ObjectDecl>();
			output << "static ";
			output << genType( decl->base, mangleName( obj ), options );
			genEnumInitializer( visitor, decl->base, output, obj->init, &curVal, options );
			output << ";" << endl;
		}
	} else {
		output << "enum ";
		genAttributes( decl->attributes );

		output << decl->name;

		if ( !members.empty() ) {
			output << " {" << endl;

			++indent;
			for ( auto member : members ) {
				auto obj = member.strict_as<ast::ObjectDecl>();
				output << indent << mangleName( obj );
				if ( obj->init ) {
					output << " = ";
					obj->init->accept( *visitor );
				}
				output << "," << endl;
			}
			--indent;

			output << indent << "}";
		}
	}
}

void CodeGenerator::postvisit( ast::TraitDecl const * decl ) {
	assertf( !options.genC, "TraitDecls should not reach code generation." );
	extension( decl );
	handleAggregate( decl, "trait " );
}

void CodeGenerator::postvisit( ast::TypedefDecl const * decl ) {
	assertf( !options.genC, "Typedefs should not reach code generation." );
	output << "typedef " << genType( decl->base, decl->name, options ) << endl;
}

void CodeGenerator::postvisit( ast::TypeDecl const * decl ) {
	assertf( !options.genC, "TypeDecls should not reach code generation." );
	output << decl->genTypeString() << " " << decl->name;
	if ( decl->sized ) {
		output << " | sized(" << decl->name << ")";
	}
	if ( !decl->assertions.empty() ) {
		output << " | { ";
		for ( ast::DeclWithType const * assert : decl->assertions ) {
			assert->accept( *visitor );
			output << "; ";
		}
		output << " }";
	}
}

void CodeGenerator::postvisit( ast::StaticAssertDecl const * decl ) {
	output << "_Static_assert(";
	decl->cond->accept( *visitor );
	output << ", ";
	decl->msg->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::Designation const * designation ) {
	auto designators = designation->designators;
	if ( 0 == designators.size() ) return;
	for ( ast::ptr<ast::Expr> const & des : designators ) {
		// If the expression is a NameExpr or VariableExpr, then it is a field.
		if ( des.as<ast::NameExpr>() || des.as<ast::VariableExpr>() ) {
			output << ".";
			des->accept( *visitor );
		// Otherwise, it is a ConstantExpr or CastExpr, then it is an index.
		} else {
			output << "[";
			des->accept( *visitor );
			output << "]";
		}
	}
	output << " = ";
}

void CodeGenerator::postvisit( ast::SingleInit const * init ) {
	init->value->accept( *visitor );
}

void CodeGenerator::postvisit( ast::ListInit const * init ) {
	auto initBegin = init->initializers.begin();
	auto initEnd = init->initializers.end();
	auto desigBegin = init->designations.begin();
	auto desigEnd = init->designations.end();

	output << "{ ";
	if ( initBegin != initEnd ) while (true) {
		(*desigBegin)->accept( *visitor );
		(*initBegin)->accept( *visitor );
		++initBegin, ++desigBegin;
		if ( initBegin == initEnd ) break;
		output << ", ";
	}
	output << " }";
	assertf( initBegin == initEnd && desigBegin == desigEnd,
		"Initializers and designators not the same length. %s", toCString( init ) );
}

void CodeGenerator::postvisit( ast::ConstructorInit const * init ) {
	assertf( !options.genC, "ConstructorInit nodes should not reach code generation." );
	// This isn't actual code, but labels the constructor/destructor pairs.
	output << "<ctorinit>{" << endl << ++indent << "ctor: ";
	if ( init->ctor ) init->ctor->accept( *visitor );
	output << ", " << endl << indent << "dtor: ";
	if ( init->dtor ) init->dtor->accept( *visitor );
	output << endl << --indent << "}";
}

void CodeGenerator::postvisit( ast::ApplicationExpr const * expr ) {
	extension( expr );
	if ( auto var = expr->func.as<ast::VariableExpr>() ) {
		const OperatorInfo * opInfo;
		if ( var->var->linkage == ast::Linkage::Intrinsic &&
				( opInfo = operatorLookup( var->var->name ) ) ) {
			auto arg = expr->args.begin();
			switch ( opInfo->type ) {
			case OT_INDEX:
				assert( 2 == expr->args.size() );
				(*arg++)->accept( *visitor );
				output << "[";
				(*arg)->accept( *visitor );
				output << "]";
				break;

			// There are no intrinsic definitions of the function call operator.
			case OT_CALL:
				assert( false );
				break;

			case OT_CTOR:
			case OT_DTOR:
				// No-op constructor, but run the internal expression.
				if ( 1 == expr->args.size() ) {
					output << "(";
					(*arg++)->accept( *visitor );
					output << ") /* " << opInfo->inputName << " */";
				// These are all implemented as some form of assignment.
				} else if ( 2 == expr->args.size() ) {
					output << "(";
					(*arg++)->accept( *visitor );
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ") /* " << opInfo->inputName << " */";
				// No constructors with 0 or more than 2 parameters.
				} else {
					assert( false );
				}
				break;

			case OT_PREFIX:
			case OT_PREFIXASSIGN:
				assert( 1 == expr->args.size() );
				output << "(" << opInfo->symbol;
				(*arg)->accept( *visitor );
				output << ")";
				break;

			case OT_POSTFIX:
			case OT_POSTFIXASSIGN:
				assert( 1 == expr->args.size() );
				(*arg)->accept( *visitor );
				output << opInfo->symbol;
				break;

			case OT_INFIX:
			case OT_INFIXASSIGN:
				assert( 2 == expr->args.size() );
				output << "(";
				(*arg++)->accept( *visitor );
				output << opInfo->symbol;
				(*arg)->accept( *visitor );
				output << ")";
				break;

			// There are no intrinsic definitions of 0/1 or label address
			// as function.
			case OT_CONSTANT:
			case OT_LABELADDRESS:
				assert( false );
			}
		// TODO: This is a work-around to make it a constant until a proper
		// constexpr solution is created.
		} else if ( var->var->linkage == ast::Linkage::BuiltinCFA &&
				var->var->name == "intptr" ) {
			output << "((void*)";
			auto arg = expr->args.begin();
			(*arg++)->accept( *visitor );
			output << ")";
		} else {
			var->accept( *visitor );
			output << "(";
			genCommaList( expr->args );
			output << ")";
		}
	} else {
		expr->func->accept( *visitor );
		output << "(";
		genCommaList( expr->args );
		output << ")";
	}
}

void CodeGenerator::postvisit( ast::UntypedExpr const * expr ) {
	extension( expr );
	if ( auto name = expr->func.as<ast::NameExpr>() ) {
		if ( const OperatorInfo * opInfo = operatorLookup( name->name ) ) {
			auto arg = expr->args.begin();
			switch ( opInfo->type ) {
			case OT_INDEX:
				assert( 2 == expr->args.size() );
				(*arg++)->accept( *visitor );
				output << "[";
				(*arg)->accept( *visitor );
				output << "]";
				break;

			case OT_CALL:
				assert( false );

			case OT_CTOR:
			case OT_DTOR:
				// No-op constructor, but run the internal expression.
				if ( 1 == expr->args.size() ) {
					output << "(";
					(*arg++)->accept( *visitor );
					output << ")";
				// These are all implemented as some form of assignment.
				} else if ( 2 == expr->args.size() ) {
					output << "(";
					(*arg++)->accept( *visitor );
					output << opInfo->symbol;
					(*arg)->accept( *visitor );
					output << ") /* " << opInfo->inputName << " */";
				// No constructors with 0 or more than 2 parameters.
				} else {
					assertf( !options.genC, "UntypedExpr constructor/destructor with 0 or more than 2 parameters." );
					output << "(";
					(*arg++)->accept( *visitor );
					output << opInfo->symbol << "{ ";
					genCommaList( arg, expr->args.end() );
					output << "}) /* " << opInfo->inputName << " */";
				}
				break;

			case OT_PREFIX:
			case OT_PREFIXASSIGN:
			case OT_LABELADDRESS:
				assert( 1 == expr->args.size() );
				output << "(" << opInfo->symbol;
				(*arg)->accept( *visitor );
				output << ")";
				break;

			case OT_POSTFIX:
			case OT_POSTFIXASSIGN:
				assert( 1 == expr->args.size() );
				(*arg)->accept( *visitor );
				output << opInfo->symbol;
				break;

			case OT_INFIX:
			case OT_INFIXASSIGN:
				assert( 2 == expr->args.size() );
				output << "(";
				(*arg++)->accept( *visitor );
				output << opInfo->symbol;
				(*arg)->accept( *visitor );
				output << ")";
				break;

			// There are no intrinsic definitions of 0/1 or label address
			// as function.
			case OT_CONSTANT:
				assert( false );
			}
		// builtin routines
		} else {
			name->accept( *visitor );
			output << "(";
			genCommaList( expr->args );
			output << ")";
		}
	} else {
		expr->func->accept( *visitor );
		output << "(";
		genCommaList( expr->args );
		output << ")";
	}
}

void CodeGenerator::postvisit( ast::RangeExpr const * expr ) {
	expr->low->accept( *visitor );
	output << " ... ";
	expr->high->accept( *visitor );
}

void CodeGenerator::postvisit( ast::NameExpr const * expr ) {
	extension( expr );
	if ( const OperatorInfo * opInfo = operatorLookup( expr->name ) ) {
		if ( OT_CONSTANT == opInfo->type ) {
			output << opInfo->symbol;
		} else {
			output << opInfo->outputName;
		}
	} else {
		output << expr->name;
	}
}

void CodeGenerator::postvisit( ast::DimensionExpr const * expr ) {
	extension( expr );
	output << "/*non-type*/" << expr->name;
}

void CodeGenerator::postvisit( ast::AddressExpr const * expr ) {
	extension( expr );
	output << "(&";
	expr->arg->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::LabelAddressExpr const * expr ) {
	extension( expr );
	output << "(&&" << expr->arg << ")";
}

void CodeGenerator::postvisit( ast::CastExpr const * expr ) {
	extension( expr );
	output << "(";
	if ( expr->result->isVoid() ) {
		output << "(void)";
	} else {
		output << "(";
		output << genType( expr->result, "", options );
		output << ")";
	}
	expr->arg->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::KeywordCastExpr const * expr ) {
	assertf( !options.genC, "KeywordCastExpr should not reach code generation." );
	extension( expr );
	output << "((" << expr->targetString() << " &)";
	expr->arg->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::VirtualCastExpr const * expr ) {
	assertf( !options.genC, "VirtualCastExpr should not reach code generation." );
	extension( expr );
	// TODO: Is this busted?
	output << "(virtual ";
	expr->arg->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::UntypedMemberExpr const * expr ) {
	assertf( !options.genC, "UntypedMemberExpr should not reach code generation." );
	extension( expr );
	expr->aggregate->accept( *visitor );
	output << ".";
	expr->member->accept( *visitor );
}

void CodeGenerator::postvisit( ast::MemberExpr const * expr ) {
	extension( expr );
	expr->aggregate->accept( *visitor );
	output << "." << mangleName( expr->member );
}

void CodeGenerator::postvisit( ast::VariableExpr const * expr ) {
	extension( expr );
	const OperatorInfo * opInfo;
	if ( dynamic_cast<ast::ZeroType const *>( expr->var->get_type() ) ) {
		output << "0";
	} else if ( expr->var->linkage == ast::Linkage::Intrinsic
			&& ( opInfo = operatorLookup( expr->var->name ) )
			&& opInfo->type == OT_CONSTANT ) {
		output << opInfo->symbol;
	} else {
		output << mangleName( expr->var );
	}
}

void CodeGenerator::postvisit( ast::ConstantExpr const * expr ) {
	extension( expr );
	output << expr->rep;
}

void CodeGenerator::postvisit( ast::SizeofExpr const * expr ) {
	extension( expr );
	output << "sizeof(";
	if ( expr->type ) {
		output << genType( expr->type, "", options );
	} else {
		expr->expr->accept( *visitor );
	}
	output << ")";
}

void CodeGenerator::postvisit( ast::AlignofExpr const * expr ) {
	// Using the GCC extension to avoid changing the std to C11.
	extension( expr );
	output << "__alignof__(";
	if ( expr->type ) {
		output << genType( expr->type, "", options );
	} else {
		expr->expr->accept( *visitor );
	}
	output << ")";
}

void CodeGenerator::postvisit( ast::UntypedOffsetofExpr const * expr ) {
	assertf( !options.genC, "UntypedOffsetofExpr should not reach code generation." );
	output << "offsetof(";
	output << genType( expr->type, "", options );
	output << ", " << expr->member;
	output << ")";
}

void CodeGenerator::postvisit( ast::OffsetofExpr const * expr ) {
	// Use GCC builtin
	output << "__builtin_offsetof(";
	output << genType( expr->type, "", options );
	output << ", " << mangleName( expr->member );
	output << ")";
}

void CodeGenerator::postvisit( ast::OffsetPackExpr const * expr ) {
	assertf( !options.genC, "OffsetPackExpr should not reach code generation." );
	output << "__CFA_offsetpack(" << genType( expr->type, "", options ) << ")";
}

void CodeGenerator::postvisit( ast::LogicalExpr const * expr ) {
	extension( expr );
	output << "(";
	expr->arg1->accept( *visitor );
	output << ( ( expr->isAnd ) ? " && " : " || " );
	expr->arg2->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::ConditionalExpr const * expr ) {
	extension( expr );
	output << "(";
	expr->arg1->accept( *visitor );
	output << " ? ";
	expr->arg2->accept( *visitor );
	output << " : ";
	expr->arg3->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::CommaExpr const * expr ) {
	extension( expr );
	output << "(";
	if ( options.genC ) {
		// arg1 of a comma expression is never used, so it can be safely cast
		// to void to reduce gcc warnings.
		ast::ptr<ast::Expr> arg1 = new ast::CastExpr( expr->location, expr->arg1 );
		arg1->accept( *visitor );
	} else {
		expr->arg1->accept( *visitor );
	}
	output << " , ";
	expr->arg2->accept( *visitor );
	output << ")";
}

void CodeGenerator::postvisit( ast::TupleAssignExpr const * expr ) {
	assertf( !options.genC, "TupleAssignExpr should not reach code generation." );
	expr->stmtExpr->accept( *visitor );
}

void CodeGenerator::postvisit( ast::UntypedTupleExpr const * expr ) {
	assertf( !options.genC, "UntypedTupleExpr should not reach code generation." );
	extension( expr );
	output << "[";
	genCommaList( expr->exprs );
	output << "]";
}

void CodeGenerator::postvisit( ast::TupleExpr const * expr ) {
	assertf( !options.genC, "TupleExpr should not reach code generation." );
	extension( expr );
	output << "[";
	genCommaList( expr->exprs );
	output << "]";
}

void CodeGenerator::postvisit( ast::TupleIndexExpr const * expr ) {
	assertf( !options.genC, "TupleIndexExpr should not reach code generation." );
	extension( expr );
	expr->tuple->accept( *visitor );
	output << "." << expr->index;
}

void CodeGenerator::postvisit( ast::TypeExpr const * expr ) {
	// TODO: Should there be an assertion there?
	if ( !options.genC ) {
		output << genType( expr->type, "", options );
	}
}

void CodeGenerator::postvisit( ast::AsmExpr const * expr ) {
	if ( !expr->inout.empty() ) {
		output << "[ " << expr->inout << " ] ";
	}
	expr->constraint->accept( *visitor );
	output << " ( ";
	expr->operand->accept( *visitor );
	output << " )";
}

void CodeGenerator::postvisit( ast::CompoundLiteralExpr const * expr ) {
	//assert( expr->result && dynamic_cast<ast::ListInit const *>( expr->init ) );
	assert( expr->result && expr->init.as<ast::ListInit>() );
	output << "(" << genType( expr->result, "", options ) << ")";
	expr->init->accept( *visitor );
}

void CodeGenerator::postvisit( ast::UniqueExpr const * expr ) {
	assertf( !options.genC, "UniqueExpr should not reach code generation." );
	output << "unq<" << expr->id << ">{ ";
	expr->expr->accept( *visitor );
	output << " }";
}

void CodeGenerator::postvisit( ast::StmtExpr const * expr ) {
	auto stmts = expr->stmts->kids;
	output << "({" << endl;
	++indent;
	unsigned int numStmts = stmts.size();
	unsigned int i = 0;
	for ( ast::ptr<ast::Stmt> const & stmt : stmts ) {
		output << indent << printLabels( stmt->labels );
		if ( i + 1 == numStmts ) {
			// Last statement in a statement expression needs to be handled
			// specially - cannot cast to void, otherwise the expression
			// statement has no value.
			if ( ast::ExprStmt const * exprStmt = stmt.as<ast::ExprStmt>() ) {
				exprStmt->expr->accept( *visitor );
				output << ";" << endl;
				++i;
				break;
			}
		}
		stmt->accept( *visitor );
		output << endl;
		if ( wantSpacing( stmt ) ) output << endl;
		++i;
	}
	--indent;
	output << indent << "})";
}

void CodeGenerator::postvisit( ast::ConstructorExpr const * expr ) {
	assertf( !options.genC, "ConstructorExpr should not reach code generation." );
	expr->callExpr->accept( *visitor );
}

void CodeGenerator::postvisit( ast::DeletedExpr const * expr ) {
	assertf( !options.genC, "DeletedExpr should not reach code generation." );
	expr->expr->accept( *visitor );
}

void CodeGenerator::postvisit( ast::DefaultArgExpr const * expr ) {
	assertf( !options.genC, "DefaultArgExpr should not reach code generation." );
	expr->expr->accept( *visitor );
}

void CodeGenerator::postvisit( ast::GenericExpr const * expr ) {
	assertf( !options.genC, "GenericExpr should not reach code generation." );
	output << "_Generic(";
	expr->control->accept( *visitor );
	output << ", ";
	unsigned int numAssocs = expr->associations.size();
	unsigned int i = 0;
	for ( const ast::GenericExpr::Association & assoc : expr->associations ) {
		if ( nullptr == assoc.type ) {
			output << "default: ";
		} else {
			output << genType( assoc.type, "", options ) << ": ";
		}
		assoc.expr->accept( *visitor );
		++i;
		if ( i != numAssocs ) output << ", ";
	}
	output << ")";
}

void CodeGenerator::postvisit( ast::CompoundStmt const * stmt ) {
	output << "{" << endl;

	++indent;
	for ( auto kid : stmt->kids ) {
		output << indent << printLabels( kid->labels );
		kid->accept( *visitor );
		output << endl;
		if ( wantSpacing( kid ) ) output << endl;
	}
	--indent;

	output << indent << "}";
}

void CodeGenerator::postvisit( ast::ExprStmt const * stmt ) {
	assert( stmt );
	// Cast the top-level expression to void to reduce gcc warnings.
	if ( options.genC ) {
		ast::ptr<ast::Expr> expr = new ast::CastExpr( stmt->location, stmt->expr );
		expr->accept( *visitor );
	} else {
		stmt->expr->accept( *visitor );
	}
	output << ";";
}

void CodeGenerator::postvisit( ast::AsmStmt const * stmt ) {
	output << "asm ";
	if ( stmt->isVolatile ) output << "volatile ";
	if ( !stmt->gotoLabels.empty() ) output << "goto ";
	output << "( ";
	if ( stmt->instruction ) stmt->instruction->accept( *visitor );
	output << " : ";
	genCommaList( stmt->output );
	output << " : ";
	genCommaList( stmt->input );
	output << " : ";
	genCommaList( stmt->clobber );
	if ( !stmt->gotoLabels.empty() ) {
		output << " : ";
		auto it = stmt->gotoLabels.begin();
		while (true) {
			output << *it++;
			if ( stmt->gotoLabels.end() == it ) break;
			output << ", ";
		}
	}
	output << " );";
}

void CodeGenerator::postvisit( ast::AsmDecl const * decl ) {
	output << "asm ";
	ast::AsmStmt const * stmt = decl->stmt;
	output << "( ";
	if ( stmt->instruction ) stmt->instruction->accept( *visitor );
	output << " )";
}

void CodeGenerator::postvisit( ast::DirectiveDecl const * decl ) {
	// endl prevents spaces before the directive.
	output << endl << decl->stmt->directive;
}

void CodeGenerator::postvisit( ast::DirectiveStmt const * stmt ) {
	// endl prevents spaces before the directive.
	output << endl << stmt->directive;
}

void CodeGenerator::postvisit( ast::IfStmt const * stmt ) {
	output << "if ( ";
	stmt->cond->accept( *visitor );
	output << " ) ";

	stmt->then->accept( *visitor );

	if ( nullptr != stmt->else_ ) {
		output << " else ";
		stmt->else_->accept( *visitor );
	}
}

void CodeGenerator::postvisit( ast::SwitchStmt const * stmt ) {
	output << "switch ( ";
	stmt->cond->accept( *visitor );
	output << " ) ";

	output << "{" << endl;
	++indent;
	for ( auto node : stmt->cases ) {
		node->accept( *visitor );
	}
	--indent;
	output << indent << "}";
}

void CodeGenerator::postvisit( ast::CaseClause const * clause ) {
	updateLocation( clause );
	output << indent;
	if ( clause->isDefault() ) {
		output << "default";
	} else {
		output << "case ";
		clause->cond->accept( *visitor );
	}
	output << ":" << endl;

	++indent;
	for ( auto stmt : clause->stmts ) {
		output << indent << printLabels( stmt->labels ) ;
		stmt->accept( *visitor );
		output << endl;
	}
	--indent;
}

void CodeGenerator::postvisit( ast::BranchStmt const * stmt ) {
	switch ( stmt->kind ) {
	case ast::BranchStmt::Goto:
		if ( !stmt->target.empty() ) {
			output << "goto " << stmt->target;
		} else if ( nullptr != stmt->computedTarget ) {
			output << "goto *";
			stmt->computedTarget->accept( *visitor );
		}
		break;
	case ast::BranchStmt::Break:
		output << "break";
		break;
	case ast::BranchStmt::Continue:
		output << "continue";
		break;
	case ast::BranchStmt::FallThrough:
	case ast::BranchStmt::FallThroughDefault:
		assertf( !options.genC, "fallthru should not reach code generation." );
		output << "fallthru";
		break;
	default:
		assertf( false, "Bad BranchStmt value." );
	}
	// Print branch target for labelled break/continue/fallthru in debug mode.
	if ( !options.genC && stmt->kind != ast::BranchStmt::Goto ) {
		if ( !stmt->target.empty() ) {
			output << " " << stmt->target;
		} else if ( stmt->kind == ast::BranchStmt::FallThrough ) {
			output << " default";
		}
	}
	output << ";";
}

void CodeGenerator::postvisit( ast::ReturnStmt const * stmt ) {
	output << "return ";
	if ( stmt->expr ) stmt->expr->accept( *visitor );
	output << ";";
}

void CodeGenerator::postvisit( ast::ThrowStmt const * stmt ) {
	assertf( !options.genC, "ThrowStmt should not reach code generation." );

	output << ((stmt->kind == ast::Terminate) ? "throw" : "throwResume");
	if ( stmt->expr ) {
		output << " ";
		stmt->expr->accept( *visitor );
	}
	if ( stmt->target ) {
		output << " _At ";
		stmt->target->accept( *visitor );
	}
	output << ";";
}

void CodeGenerator::postvisit( ast::CatchClause const * stmt ) {
	assertf( !options.genC, "CatchClause should not reach code generation." );

	output << ((stmt->kind == ast::Terminate) ? "catch" : "catchResume");
	output << "( ";
	stmt->decl->accept( *visitor );
	if ( stmt->cond ) {
		output << " ; ";
		stmt->cond->accept( *visitor );
	}
	output << " ) ";
	stmt->body->accept( *visitor );
}

void CodeGenerator::postvisit( ast::WaitForStmt const * stmt ) {
	assertf( !options.genC, "WaitforStmt should not reach code generation." );

	bool first = true;
	for ( ast::ptr<ast::WaitForClause> const & clause : stmt->clauses ) {
		if (first) { output << "or "; first = false; }
		if ( clause->when_cond ) {
			output << "when(";
			stmt->timeout_cond->accept( *visitor );
			output << ") ";
		}
		output << "waitfor(";
		clause->target->accept( *visitor );
		for ( ast::ptr<ast::Expr> const & expr : clause->target_args ) {
			output << ",";
			expr->accept( *visitor );
		}
		output << ") ";
		clause->stmt->accept( *visitor );
	}

	if ( stmt->timeout_stmt ) {
		output << "or ";
		if ( stmt->timeout_cond ) {
			output << "when(";
			stmt->timeout_cond->accept( *visitor );
			output << ") ";
		}
		output << "timeout(";
		stmt->timeout_time->accept( *visitor );
		output << ") ";
		stmt->timeout_stmt->accept( *visitor );
	}

	if ( stmt->else_stmt ) {
		output << "or ";
		if ( stmt->else_cond ) {
			output << "when(";
			stmt->else_cond->accept( *visitor );
			output << ")";
		}
		output << "else ";
		stmt->else_stmt->accept( *visitor );
	}
}

void CodeGenerator::postvisit( ast::WithStmt const * stmt ) {
	assertf( !options.genC, "WithStmt should not reach code generation." );

	output << "with ( ";
	genCommaList( stmt->exprs );
	output << " ) ";
	stmt->stmt->accept( *visitor );
}

void CodeGenerator::postvisit( ast::WhileDoStmt const * stmt ) {
	if ( stmt->isDoWhile ) {
		output << "do";
	} else {
		output << "while (";
		stmt->cond->accept( *visitor );
		output << ")";
	}
	output << " ";

	output << CodeGenerator::printLabels( stmt->body->labels );
	stmt->body->accept( *visitor );

	output << indent;

	if ( stmt->isDoWhile ) {
		output << " while (";
		stmt->cond->accept( *visitor );
		output << ");";
	}
}

void CodeGenerator::postvisit( ast::ForStmt const * stmt ) {
	// Initializer is always hoised so don't generate it.
	// TODO: Do an assertion check?
	output << "for (;";

	if ( nullptr != stmt->cond ) {
		stmt->cond->accept( *visitor );
	}
	output << ";";

	if ( nullptr != stmt->inc ) {
		// cast the top-level expression to void to reduce gcc warnings.
		ast::Expr * expr = new ast::CastExpr( stmt->inc );
		expr->accept( *visitor );
	}
	output << ") ";

	if ( nullptr != stmt->body ) {
		output << printLabels( stmt->body->labels );
		stmt->body->accept( *visitor );
	}
}

void CodeGenerator::postvisit( ast::NullStmt const * ) {
	output << "/* null statement */ ;";
}

void CodeGenerator::postvisit( ast::DeclStmt const * stmt ) {
	stmt->decl->accept( *visitor );

	if ( doSemicolon( stmt->decl ) ) output << ";";
}

void CodeGenerator::postvisit( ast::ImplicitCtorDtorStmt const * stmt ) {
	assertf( !options.genC, "ImplicitCtorCtorStmt should not reach code generation." );
	stmt->callStmt->accept( *visitor );
}

void CodeGenerator::postvisit( ast::MutexStmt const * stmt ) {
	assertf( !options.genC, "MutexStmt should not reach code generation." );
	// TODO: But this isn't what a mutex statement looks like.
	stmt->stmt->accept( *visitor );
}

std::string genName( ast::DeclWithType const * decl ) {
	if ( const OperatorInfo * opInfo = operatorLookup( decl->name ) ) {
		return opInfo->outputName;
	} else {
		return decl->name;
	}
}

} // namespace CodeGen
