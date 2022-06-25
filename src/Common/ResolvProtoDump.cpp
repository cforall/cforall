//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolvProtoDump.cpp -- Prints AST as instances for resolv-proto.
//
// Author           : Andrew Beach
// Created On       : Wed Oct  6 14:10:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Tue Oct 18 11:23:00 2021
// Update Count     : 0
//

#include "ResolvProtoDump.hpp"

#include <cctype>
#include <iostream>
#include <set>
#include <unordered_set>

#include "AST/Copy.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/utility.h"

namespace {

/// Add a prefix to an existing name.
std::string add_prefix( const std::string & prefix, const char * added ) {
	if ( prefix.empty() ) {
		return std::string("$") + added;
	} else {
		return prefix + added;
	}
}

/// Shortens operator names.
std::string op_name( const std::string & name ) {
	if ( name.compare( 0, 10, "_operator_" ) == 0 ) {
		return name.substr( 10 );
	} else if ( name.compare( "_constructor" ) == 0
			|| name.compare( "_destructor" ) == 0 ) {
		return name.substr( 1 );
	} else if ( name.compare( 0, 11, "__operator_" ) == 0 ) {
		return name.substr( 11 );
	} else {
		return name;
	}
}

/// Get the resolv-proto names for operators.
std::string rp_name( const std::string & name, std::string && pre = "" ) {
	// Check for anonymous names.
	if ( name.empty() ) {
		return add_prefix( pre, "anon" );
	}

	// Replace operator names.
	const CodeGen::OperatorInfo * opInfo = CodeGen::operatorLookup( name );
	if ( nullptr != opInfo ) {
		return add_prefix( pre, "" ) + op_name( opInfo->outputName );
	}

	// Replace return value prefix.
	if ( name.compare( 0, 8, "_retval_" ) == 0 ) {
		return add_prefix( pre, "rtn_" ) + op_name( name.substr( 8 ) );
	}

	// Default to just name, with first character in lowercase.
	if ( std::isupper( name[0] ) ) {
		std::string copy = name;
		copy[0] = std::tolower( copy[0] );
		return pre + copy;
	}
	return pre + name;
}

/// Normalise a type instance name.
std::string ti_name( const std::string & name ) {
	// Replace built-in names
	if ( name == "char16_t" || name == "char32_t" || name == "wchar_t" ) {
		return std::string("#") + name;
	}

	// Strip leadng underscores.
	unsigned i = 0;
	while ( i < name.size() && name[i] == '_' ) { ++i; }
	if ( i == name.size() ) {
		return "Anon";
	}

	std::string stripped = name.substr( i );
	// Strip trailing generic from autogen names ()
	static char generic[] = "_generic_";
	static size_t n_generic = sizeof(generic) - 1;
	if ( stripped.size() >= n_generic
			&& stripped.substr( stripped.size() - n_generic ) == generic ) {
		stripped.resize( stripped.size() - n_generic );
	}

	// Uppercase first character.
	stripped[0] = std::toupper( stripped[0] );
	return stripped;
}

std::vector<ast::ptr<ast::Type>> to_types(
		const std::vector<ast::ptr<ast::Expr>> & data ) {
	std::vector<ast::ptr<ast::Type>> ret_val;
	ret_val.reserve( data.size() );
	for ( auto entry : data ) {
		if ( auto * typeExpr = entry.as<ast::TypeExpr>() ) {
			ret_val.emplace_back( typeExpr->type );
		}
	}
	return ret_val;
}

enum class septype { separated, terminated, preceded };

template<typename V>
void build(
		V & visitor,
		const std::vector<ast::ptr<ast::Type>> & types,
		std::stringstream & ss,
		septype mode );

template<typename V>
void buildAsTuple(
		V & visitor, const std::vector<ast::ptr<ast::Type>> & types,
		std::stringstream & ss );

struct TypePrinter : public ast::WithShortCircuiting, ast::WithVisitorRef<TypePrinter> {
	/// Accumulator for the printed type.
	std::stringstream ss;
	/// Closed type variables.
	const std::unordered_set<std::string> & closed;
	/// Depth of nesting from root type.
	unsigned depth;

	TypePrinter( const std::unordered_set<std::string> & closed ) :
		ss(), closed(closed), depth(0)
	{}

	std::string result() const {
		return ss.str();
	}

	// Basic type represented as an integer type.
	// TODO: Maybe hard-code conversion graph and make named type.
	void previsit( const ast::BasicType * type ) {
		ss << (int)type->kind;
	}

	// Pointers (except function pointers) are represented as generic type.
	void previsit( const ast::PointerType * type ) {
		if ( nullptr == type->base.as<ast::FunctionType>() ) {
			ss << "#$ptr<";
			++depth;
		}
	}
	void postvisit( const ast::PointerType * type ) {
		if ( nullptr == type->base.as<ast::FunctionType>() ) {
			--depth;
			ss << '>';
		}
	}

	// Arrays repersented as pointers.
	void previsit( const ast::ArrayType * type ) {
		ss << "#$ptr<";
		++depth;
		type->base->accept( *visitor );
		--depth;
		ss << '>';
		visit_children = false;
	}

	// Ignore top-level references as they are mostly transparent to resolution.
	void previsit( const ast::ReferenceType * ) {
		if ( !atTopLevel() ) { ss << "#$ref<"; }
		++depth;
	}
	void postvisit( const ast::ReferenceType * ) {
		--depth;
		if ( !atTopLevel() ) { ss << '>'; }
	}

	void previsit( const ast::FunctionType * type ) {
		ss << '[';
		++depth;
		build( *visitor, type->returns, ss, septype::preceded );
		ss << " : ";
		build( *visitor, type->params, ss, septype::terminated );
		--depth;
		ss << ']';
		visit_children = false;
	}

private:
	bool atTopLevel() const {
		return 0 == depth;
	}

	void handleAggregate( const ast::BaseInstType * type ) {
		ss << '#' << type->name;
		if ( !type->params.empty() ) {
			ss << '<';
			++depth;
			build( *visitor, to_types( type->params ), ss, septype::separated );
			--depth;
			ss << '>';
		}
		visit_children = false;
	}
public:

	void previsit( const ast::StructInstType * type ) {
		handleAggregate( type );
	}

	void previsit( const ast::UnionInstType * type ) {
		handleAggregate( type );
	}

	void previsit( const ast::EnumInstType * ) {
		// TODO: Add the meaningful text representation of typed enum
		ss << (int)ast::BasicType::SignedInt;
	}

	void previsit( const ast::TypeInstType * type ) {
		// Print closed variables as named types.
		if ( closed.count( type->name ) ) {
			ss << '#' << type->name;
		// Otherwise normalize the name.
		} else {
			ss << ti_name( type->name );
		}
	}

	void previsit( const ast::TupleType * tupleType ) {
		++depth;
		buildAsTuple( *visitor, tupleType->types, ss );
		--depth;
		visit_children = false;
	}

	void previsit( const ast::VarArgsType * ) {
		if ( atTopLevel() ) ss << "#$varargs";
	}

	// TODO: Support 0 and 1 with their type names and conversions.
	void previsit( const ast::ZeroType * ) {
		ss << (int)ast::BasicType::SignedInt;
	}

	void previsit( const ast::OneType * ) {
		ss << (int)ast::BasicType::SignedInt;
	}

	void previsit( const ast::VoidType * ) {
		if ( !atTopLevel() ) {
			ss << "#void";
		}
	}
};

struct ExprPrinter : public ast::WithShortCircuiting, ast::WithVisitorRef<ExprPrinter> {
	// TODO: Change interface to generate multiple expression canditates.
	/// Accumulator of the printed expression.
	std::stringstream ss;
	/// Set of closed type variables.
	const std::unordered_set<std::string> & closed;

	ExprPrinter( const std::unordered_set<std::string> & closed ) :
		ss(), closed( closed )
	{}

	std::string result() const {
		return ss.str();
	}

	void previsit( const ast::NameExpr * expr ) {
		ss << '&' << rp_name( expr->name );
	}

	/// Handle already resolved variables as type constants.
	void previsit( const ast::VariableExpr * expr ) {
		ss << ast::Pass<TypePrinter>::read( expr->var->get_type(), closed );
		visit_children = false;
	}

	void previsit( const ast::UntypedExpr * expr ) {
		// TODO: Handle name extraction more generally.
		const ast::NameExpr * name = expr->func.as<ast::NameExpr>();

		// TODO: Incorporate function type into resolv-proto.
		if ( !name ) {
			expr->func->accept( *visitor );
			visit_children = false;
			return;
		}

		ss << rp_name( name->name );
		if ( expr->args.empty() ) {
			ss << "()";
		} else {
			ss << "( ";
			auto it = expr->args.begin();
			while (true) {
				(*it)->accept( *visitor );
				if ( ++it == expr->args.end() ) break;
				ss << ' ';
			}
			ss << " )";
		}
		visit_children = false;
	}

	void previsit( const ast::ApplicationExpr * expr ) {
		ss << ast::Pass<TypePrinter>::read( static_cast<const ast::Expr *>( expr ), closed );
		visit_children = false;
	}

	void previsit( const ast::AddressExpr * expr ) {
		ss << "$addr( ";
		expr->arg->accept( *visitor );
		ss << " )";
		visit_children = false;
	}

	void previsit( const ast::CastExpr * expr ) {
		ss << ast::Pass<TypePrinter>::read( expr->result.get(), closed );
		visit_children = false;
	}

	/// Member access handled as function from aggregate to member.
	void previsit( const ast::UntypedMemberExpr * expr ) {
		// TODO: Handle name extraction more generally.
		const ast::NameExpr * name = expr->member.as<ast::NameExpr>();

		// TODO: Incorporate function type into resolve-proto.
		if ( !name ) {
			expr->member->accept( *visitor );
			visit_children = false;
			return;
		}

		ss << rp_name( name->name, "$field_" );
		ss << "( ";
		expr->aggregate->accept( *visitor );
		ss << " )";
		visit_children = false;
	}

	/// Constant expression replaced by its type.
	void previsit( const ast::ConstantExpr * expr ) {
		ss << ast::Pass<TypePrinter>::read( static_cast<const ast::Expr *>( expr ), closed );
		visit_children = false;
	}

	/// sizeof, alignof, & offsetof are replaced by constant type.
	// TODO: Extra expression to resolve argument.
	void previsit( const ast::SizeofExpr * ) {
		ss << (int)ast::BasicType::LongUnsignedInt;
		visit_children = false;
	}
	void previsit( const ast::AlignofExpr * ) {
		ss << (int)ast::BasicType::LongUnsignedInt;
		visit_children = false;
	}
	void previsit( const ast::UntypedOffsetofExpr * ) {
		ss << (int)ast::BasicType::LongUnsignedInt;
		visit_children = false;
	}

	/// Logical expressions represented as operators.
	void previsit( const ast::LogicalExpr * expr ) {
		ss << ( (ast::AndExpr == expr->isAnd) ? "$and( " : "$or( " );
		expr->arg1->accept( *visitor );
		ss << ' ';
		expr->arg2->accept( *visitor );
		ss << " )";
		visit_children = false;
	}

	/// Conditional expression represented as an operator.
	void previsit( const ast::ConditionalExpr * expr ) {
		ss << "$if( ";
		expr->arg1->accept( *visitor );
		ss << ' ';
		expr->arg2->accept( *visitor );
		ss << ' ';
		expr->arg3->accept( *visitor );
		ss << " )";
		visit_children = false;
	}

	/// Comma expression represented as on operator.
	void previsit( const ast::CommaExpr * expr ) {
		ss << "$seq( ";
		expr->arg1->accept( *visitor );
		ss << ' ';
		expr->arg2->accept( *visitor );
		ss << " )";
		visit_children = false;
	}

	// TODO: Handle ignored ImplicitCopyCtorExpr and below.
};

template<typename V>
void build(
		V & visitor,
		const std::vector<ast::ptr<ast::Type>> & types,
		std::stringstream & ss,
		septype mode ) {
	if ( types.empty() ) return;

	if ( septype::preceded == mode ) { ss << ' '; }

	auto it = types.begin();
	(*it)->accept( visitor );

	while ( ++it != types.end() ) {
		ss << ' ';
		(*it)->accept( visitor );
	}

	if ( septype::terminated == mode ) { ss << ' '; }
}

std::string buildType(
		const std::string & name, const ast::Type * type,
		const std::unordered_set<std::string> & closed );

/// Build a string representing a function type.
std::string buildFunctionType(
		const std::string & name, const ast::FunctionType * type,
		const std::unordered_set<std::string> & closed ) {
	ast::Pass<TypePrinter> printer( closed );
	std::stringstream & ss = printer.core.ss;

	build( printer, type->returns, ss, septype::terminated );
	ss << rp_name( name );
	build( printer, type->params, ss, septype::preceded );
	for ( const auto & assertion : type->assertions ) {
		auto var = assertion->var;
		ss << " | " << buildType( var->name, var->get_type(), closed );
	}
	return ss.str();
}

/// Build a description of a type.
std::string buildType(
		const std::string & name, const ast::Type * type,
		const std::unordered_set<std::string> & closed ) {
	const ast::Type * norefs = type->stripReferences();

	if ( const auto & ptrType = dynamic_cast<const ast::PointerType *>( norefs ) ) {
		if ( const auto & funcType = ptrType->base.as<ast::FunctionType>() ) {
			return buildFunctionType( name, funcType, closed );
		}
	} else if ( const auto & funcType = dynamic_cast<const ast::FunctionType *>( norefs ) ) {
		return buildFunctionType( name, funcType, closed );
	}

	std::stringstream ss;
	ss << ast::Pass<TypePrinter>::read( norefs, closed );
	ss << " &" << rp_name( name );
	return ss.str();
}

/// Builds description of a field access.
std::string buildAggregateDecl( const std::string & name,
		const ast::AggregateDecl * agg, const ast::Type * type,
		const std::unordered_set<std::string> & closed ) {
	const ast::Type * norefs = type->stripReferences();
	std::stringstream ss;

	ss << ast::Pass<TypePrinter>::read( norefs, closed ) << ' ';
	ss << rp_name( name, "$field_" );
	ss << " #" << agg->name;
	if ( !agg->params.empty() ) {
		ss << '<';
		auto it = agg->params.begin();
		while (true) {
			ss << ti_name( (*it)->name );
			if ( ++it == agg->params.end() ) break;
			ss << ' ';
		}
		ss << '>';
	}
	return ss.str();
}

template<typename V>
void buildAsTuple(
		V & visitor, const std::vector<ast::ptr<ast::Type>> & types,
		std::stringstream & ss ) {
	switch ( types.size() ) {
	case 0:
		ss << "#void";
		break;
	case 1:
		types.front()->accept( visitor );
		break;
	default:
		ss << "#$" << types.size() << '<';
		build( visitor, types, ss, septype::separated );
		ss << '>';
		break;
	}
}

/// Adds a return
std::string buildReturn(
		const ast::Type * returnType,
		const ast::Expr * expr,
		const std::unordered_set<std::string> & closed ) {
	std::stringstream ss;
	ss << "$constructor( ";
	ss << ast::Pass<TypePrinter>::read( returnType, closed );
	ss << ' ';
	ss << ast::Pass<ExprPrinter>::read( expr, closed );
	ss << " )";
	return ss.str();
}

void buildInitComponent(
		std::stringstream & out, const ast::Init * init,
		const std::unordered_set<std::string> & closed ) {
	if ( const auto * s = dynamic_cast<const ast::SingleInit *>( init ) ) {
		out << ast::Pass<ExprPrinter>::read( s->value.get(), closed ) << ' ';
	} else if ( const auto * l = dynamic_cast<const ast::ListInit *>( init ) ) {
		for ( const auto & it : l->initializers ) {
			buildInitComponent( out, it, closed );
		}
	}
}

/// Build a representation of an initializer.
std::string buildInitializer(
		const std::string & name, const ast::Init * init,
		const std::unordered_set<std::string> & closed ) {
	std::stringstream ss;
	ss << "$constructor( &";
	ss << rp_name( name );
	ss << ' ';
	buildInitComponent( ss, init, closed );
	ss << ')';
	return ss.str();
}

/// Visitor for collecting and printing resolver prototype output.
class ProtoDump : public ast::WithShortCircuiting, ast::WithVisitorRef<ProtoDump> {
	/// Declarations in this scope.
	// Set is used for ordering of printing.
	std::set<std::string> decls;
	/// Expressions in this scope.
	std::vector<std::string> exprs;
	/// Sub-scopes
	std::vector<ProtoDump> subs;
	/// Closed type variables
	std::unordered_set<std::string> closed;
	/// Outer lexical scope
	const ProtoDump * parent;
	/// Return type for this scope
	ast::ptr<ast::Type> returnType;

	/// Is the declaration in this scope or a parent scope?
	bool hasDecl( const std::string & decl ) const {
		return decls.count( decl ) || (parent && parent->hasDecl( decl ));
	}

	/// Adds a declaration to this scope if it is new.
	void addDecl( const std::string & decl ) {
		if ( !hasDecl( decl ) ) decls.insert( decl );
	}

	/// Adds a new expression to this scope.
	void addExpr( const std::string & expr ) {
		if ( !expr.empty() ) exprs.emplace_back( expr );
	}

	/// Adds a new scope as a child scope.
	void addSub( ast::Pass<ProtoDump> && pass ) {
		subs.emplace_back( std::move( pass.core ) );
	}

	/// Adds all named declaration in a list to the local scope.
	void addAll( const std::vector<ast::ptr<ast::DeclWithType>> & decls ) {
		for ( auto decl : decls ) {
			// Skip anonymous decls.
			if ( decl->name.empty() ) continue;

			if ( const auto & obj = decl.as<ast::ObjectDecl>() ) {
				previsit( obj );
			}
		}
	}

public:
	ProtoDump() :
		parent( nullptr ), returnType( nullptr )
	{}

	ProtoDump( const ProtoDump * parent, const ast::Type * returnType ) :
		closed( parent->closed ), parent( parent ), returnType( returnType )
	{}

	ProtoDump( const ProtoDump & other ) :
		decls( other.decls ), exprs( other.exprs ), subs( other.subs ),
		closed( other.closed ), parent( other.parent ),
		returnType( other.returnType )
	{}

	ProtoDump( ProtoDump && ) = default;

	ProtoDump & operator=( const ProtoDump & ) = delete;
	ProtoDump & operator=( ProtoDump && ) = delete;

	void previsit( const ast::ObjectDecl * decl ) {
		// Add variable as declaration.
		addDecl( buildType( decl->name, decl->type, closed ) );

		// Add initializer as expression if applicable.
		if ( decl->init ) {
			addExpr( buildInitializer( decl->name, decl->init, closed ) );
		}
	}

	void previsit( const ast::FunctionDecl * decl ) {
		visit_children = false;

		// Skips declarations with ftype parameters.
		for ( const auto & typeDecl : decl->type->forall ) {
			if ( ast::TypeDecl::Ftype == typeDecl->kind ) {
				return;
			}
		}

		// Add function as declaration.
		// NOTE: I'm not sure why the assertions are only present on the
		// declaration and not the function type. Is that an error?
		ast::FunctionType * new_type = ast::shallowCopy( decl->type.get() );
		for ( const ast::ptr<ast::DeclWithType> & assertion : decl->assertions ) {
			new_type->assertions.push_back(
				new ast::VariableExpr( assertion->location , assertion )
			);
		}
		addDecl( buildFunctionType( decl->name, new_type, closed ) );
		delete new_type;

		// Add information body if available.
		if ( !decl->stmts ) return;
		const std::vector<ast::ptr<ast::Type>> & returns =
				decl->type->returns;

		// Add the return statement.
		ast::ptr<ast::Type> retType = nullptr;
		if ( 1 == returns.size() ) {
			if ( !returns.front().as<ast::VoidType>() ) {
				retType = returns.front();
			}
		} else if ( 1 < returns.size() ) {
			retType = new ast::TupleType( copy( returns ) );
		}
		ast::Pass<ProtoDump> body( this, retType.get() );

		// Handle the forall clause (type parameters and assertions).
		for ( const ast::ptr<ast::TypeDecl> & typeDecl : decl->type_params ) {
			// Add set of "closed" types to body so that it can print them as NamedType.
			body.core.closed.insert( typeDecl->name );

			// Add assertions to local scope as declarations as well.
			for ( const ast::DeclWithType * assertion : typeDecl->assertions ) {
				assertion->accept( body );
			}
		}

		// NOTE: Related to the last NOTE; this is where the assertions are now.
		for ( const ast::ptr<ast::DeclWithType> & assertion : decl->assertions ) {
			assertion->accept( body );
		}

		// Add named parameters and returns to local scope.
		body.core.addAll( decl->returns );
		body.core.addAll( decl->params );

		// Add contents of the function to a new scope.
		decl->stmts->accept( body );

		// Store sub-scope
		addSub( std::move( body ) );
	}

private:
	void addAggregateFields( const ast::AggregateDecl * agg ) {
		for ( const auto & member : agg->members ) {
			if ( const ast::ObjectDecl * obj = member.as<ast::ObjectDecl>() ) {
				addDecl( buildAggregateDecl( obj->name, agg, obj->type, closed ) );
			}
		}
		visit_children = false;
	}

public:

	void previsit( const ast::StructDecl * decl ) {
		addAggregateFields( decl );
	}

	void previsit( const ast::UnionDecl * decl ) {
		addAggregateFields( decl );
	}

	void previsit( const ast::EnumDecl * decl ) {
		for ( const auto & member : decl->members ) {
			if ( const auto * obj = member.as<ast::ObjectDecl>() ) {
				previsit( obj );
			}
		}

		visit_children = false;
	}

	void previsit( const ast::ReturnStmt * stmt ) {
		// Do nothing for void-returning functions or statements returning nothing.
		if ( !returnType || !stmt->expr ) return;

		// Otherwise constuct the return type from the expression.
		addExpr( buildReturn( returnType.get(), stmt->expr, closed ) );
		visit_children = false;
	}

	void previsit( const ast::AsmStmt * ) {
		// Skip asm statements.
		visit_children = false;
	}

	void previsit( const ast::Expr * expr ) {
		addExpr( ast::Pass<ExprPrinter>::read( expr, closed ) );
		visit_children = false;
	}

private:
	/// Print the pesudo-declarations not in any scope.
	void printGlobal( std::ostream & out ) const {
		// &? Address of operator.
		out << "#$ptr<T> $addr T" << std::endl;
		const int intId = (int)ast::BasicType::SignedInt;
		// ?&&? ?||? ?: Logical operators.
		out << intId << " $and " << intId << ' ' << intId << std::endl;
		out << intId << " $or " << intId << ' ' << intId << std::endl;
		out << "T $if " << intId << " T T" << std::endl;
		// ?,? Sequencing.
		out << "T $seq X T" << std::endl;
	}

	/// Print everything in this scope and its child scopes.
	void printLocal( std::ostream & out, unsigned indent ) const {
		const std::string tab( indent, '\t' );

		// Print Declarations:
		for ( const std::string & decl : decls ) {
			out << tab << decl << std::endl;
		}

		// Print Divider:
		out << '\n' << tab << "%%\n" << std::endl;

		// Print Top-Level Expressions:
		for ( const std::string & expr : exprs ) {
			out << tab << expr << std::endl;
		}

		// Print Children Scopes:
		++indent;
		for ( const ProtoDump & sub : subs ) {
			out << tab << '{' << std::endl;
			sub.printLocal( out, indent );
			out << tab << '}' << std::endl;
		}
	}
public:
	/// Start printing, the collected information.
	void print( std::ostream & out ) const {
		printGlobal( out );
		printLocal( out, 0 );
	}
};

} // namespace

void dumpAsResolverProto( ast::TranslationUnit & transUnit ) {
	ast::Pass<ProtoDump> dump;
	accept_all( transUnit, dump );
	dump.core.print( std::cout );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
