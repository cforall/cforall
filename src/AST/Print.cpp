//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Print.cpp -- Print an AST (or sub-tree) to a stream.
//
// Author           : Thierry Delisle
// Created On       : Tue May 21 16:20:15 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "Print.hpp"

#include "Decl.hpp"
#include "Expr.hpp"
#include "Stmt.hpp"
#include "Type.hpp"
#include "TypeSubstitution.hpp"
#include "CompilationState.h"

#include "Common/utility.h" // for group_iterate

using namespace std;

namespace ast {

namespace {

template<typename C, typename... T>
constexpr array<C, sizeof...(T)> make_array( T&&... values ) {
	return array<C, sizeof...(T)>{ std::forward<T>( values )... };
}

namespace Names {
	static constexpr auto FuncSpecifiers = make_array<const char*>(
		"inline", "_Noreturn", "fortran"
	);

	static constexpr auto StorageClasses = make_array<const char*>(
		"extern", "static", "auto", "register", "__thread", "_Thread_local"
	);

	static constexpr auto Qualifiers = make_array<const char*>(
		"const", "restrict", "volatile", "mutex", "_Atomic"
	);
}

template<typename bits_t, size_t N>
void print( ostream & os, const bits_t & bits,
		const array<const char *, N> & names ) {
	if ( !bits.any() ) return;
	for ( size_t i = 0 ; i < N ; i += 1 ) {
		if ( bits[i] ) {
			os << names[i] << ' ';
		}
	}
}

class Printer final : public Visitor {
public:
	ostream & os;
	Indenter indent;
	bool short_mode;

	Printer(ostream & os, Indenter indent, bool short_mode) : os( os ), indent( indent ), short_mode(short_mode) {}

private:
	template< typename C >
	void printAll( const C & c ) {
		for ( const auto & i : c ) {
			if ( i ) {
				os << indent;
				i->accept( *this );
				// need an endl after each element because it's not
				// easy to know when each individual item should end
				os << endl;
			} // if
		} // for
	}

	/// call if mandatory field is missing
	void undefined() {
		os << "UNDEFINED";
	}

	/// call for fields that should be mandatory
	void safe_print( const ast::Node * n ) {
		if ( n ) n->accept( *this );
		else undefined();
	}

	/// call to print short form. Incorporates features of safe_print()
	void short_print( const ast::Decl * n ) {
		if ( ! n ) { undefined(); return; }
		bool old_short = short_mode; short_mode = true;
		n->accept( *this );
		short_mode = old_short;
	}

	static const char* Names[];

	void print( const std::vector<ast::Label> & labels ) {
		if ( labels.empty() ) return;
		os << indent << "... Labels: {";
		bool isFirst = true;
		for ( const Label & l : labels ) {
			if ( isFirst ) { isFirst = false; } else { os << ","; }
			os << l;
		}
		os << "}" << endl;
	}

	void print( const ast::Expr::InferUnion & inferred, unsigned level = 0 ) {
		if (inferred.data.resnSlots && !inferred.data.resnSlots->empty()) {
			os << indent << "with " << inferred.data.resnSlots->size()
			   << " pending inference slots" << endl;
		}
		if (inferred.data.inferParams && !inferred.data.inferParams->empty()) {
			os << indent << "with inferred parameters " << level << ":" << endl;
			++indent;
			for ( const auto & i : *inferred.data.inferParams ) {
				os << indent;
				short_print( i.second.declptr );
				os << endl;
				print( i.second.expr->inferred, level+1 );
			}
			--indent;
		}
	}

	void print( const ast::FunctionType::ForallList & forall ) {
		if ( forall.empty() ) return;
		os << "forall" << endl;
		++indent;
		printAll( forall );
		os << indent;
		--indent;
	}

	void print( const ast::FunctionType::AssertionList & assts ) {
		if (assts.empty()) return;
		os << "with assertions" << endl;
		++indent;
		printAll(assts);
		os << indent;
		--indent;
	}

	void print( const std::vector<ptr<Attribute>> & attrs ) {
		if ( attrs.empty() ) return;
		os << "with attributes" << endl;
		++indent;
		printAll( attrs );
		--indent;
	}

	void print( const std::vector<ptr<Expr>> & params ) {
		if ( params.empty() ) return;
		os << endl << indent << "... with parameters" << endl;
		++indent;
		printAll( params );
		--indent;
	}

	void print( const ast::AggregateDecl * node ) {
		os << node->typeString() << " " << node->name;

		if ( ! short_mode && node->linkage != Linkage::Cforall ) {
			os << " " << Linkage::name( node->linkage );
		}

		os << " " << (node->body ? "with" : "without") << " body";

		if ( ! node->params.empty() ) {
			os << endl << indent << "... with parameters" << endl;
			++indent;
			printAll( node->params );
			--indent;
		}

		if ( ! short_mode && ! node->members.empty() ) {
			os << endl << indent << "... with members" << endl;
			++indent;
			printAll( node->members );
			--indent;
		}

		if ( ! short_mode && ! node->attributes.empty() ) {
			os << endl << indent << "... with attributes" << endl;
			++indent;
			printAll( node->attributes );
			--indent;
		}

		auto ptrToEnum = dynamic_cast<const ast::EnumDecl *>(node);
		if ( ! short_mode && ptrToEnum && ptrToEnum->base ) {
			os << endl << indent << ".. with (enum) base" << endl;
			++indent;
			ptrToEnum->base->accept( *this );
			--indent;
		}

		os << endl;
	}

    void print( const ast::WaitStmt * node ) {
		if ( node->timeout_time ) {
			os << indent-1 << "timeout of:" << endl;
			node->timeout_time->accept( *this );

			if ( node->timeout_stmt ) {
				os << indent-1 << "... with statment:" << endl;
				node->timeout_stmt->accept( *this );
			}

			if ( node->timeout_cond ) {
				os << indent-1 << "... with condition:" << endl;
				node->timeout_cond->accept( *this );
			}
		}

		if ( node->else_stmt ) {
			os << indent-1 << "else:" << endl;
			node->else_stmt->accept( *this );

			if ( node->else_cond ) {
				os << indent-1 << "... with condition:" << endl;
				node->else_cond->accept( *this );
			}
		}
	}

	void preprint( const ast::NamedTypeDecl * node ) {
		if ( ! node->name.empty() ) {
			os << node->name << ": ";
		}

		if ( ! short_mode && node->linkage != Linkage::Cforall ) {
			os << Linkage::name( node->linkage ) << " ";
		}

		ast::print( os, node->storage );
		os << node->typeString();

		if ( node->base ) {
			os << " for ";
			++indent;
			node->base->accept( *this );
			--indent;
		}

		if ( ! node->assertions.empty() ) {
			os << endl << indent << "... with assertions" << endl;
			++indent;
			printAll( node->assertions );
			--indent;
		}
	}

	void postprint( const ast::Expr * node ) {
		print( node->inferred );

		if ( node->result ) {
			os << endl << indent << "... with resolved type:" << endl;
			++indent;
			os << indent;
			node->result->accept( *this );
			--indent;
		}

		if ( node->env ) {
			os << endl << indent << "... with environment:" << endl;
			++indent;
			node->env->accept( *this );
			--indent;
		}

		if ( node->extension ) {
			os << endl << indent << "... with extension";
		}
	}

	void preprint( const ast::Type * node ) {
		ast::print( os, node->qualifiers );
	}

	void preprint( const ast::FunctionType * node ) {
		print( node->forall );
		print( node->assertions );
		ast::print( os, node->qualifiers );
	}

	void preprint( const ast::BaseInstType * node ) {
		print( node->attributes );
		ast::print( os, node->qualifiers );
	}

public:
	virtual const ast::DeclWithType * visit( const ast::ObjectDecl * node ) override final {
		if ( ! node->name.empty() ) os << node->name << ": ";

		if ( ! short_mode && node->linkage != Linkage::Cforall ) {
			os << Linkage::name( node->linkage ) << " ";
		}

		ast::print( os, node->storage );

		if ( node->type ) {
			node->type->accept( *this );
		} else {
			os << "untyped entity";
		}

		if ( ! short_mode && node->init ) {
			++indent;
			os << " with initializer (" << (
				node->init->maybeConstructed
					? "maybe constructed"
					: "not constructed"
				) << ")" << endl << indent;
			node->init->accept( *this );
			--indent;
			os << endl;
		}

		if ( ! short_mode && ! node->attributes.empty() ) {
			os << endl << indent << "... with attributes:" << endl;
			++indent;
			printAll( node->attributes );
			--indent;
		}

		if ( node->bitfieldWidth ) {
			os << indent << " with bitfield width ";
			node->bitfieldWidth->accept( *this );
		}

		return node;
	}

	virtual const ast::DeclWithType * visit( const ast::FunctionDecl * node ) override final {
		if ( !node->name.empty() ) os << node->name << ": ";

		if ( ! short_mode && node->linkage != Linkage::Cforall ) {
			os << Linkage::name( node->linkage ) << " ";
		}

		if ( ! short_mode ) printAll( node->attributes );

		ast::print( os, node->storage );
		ast::print( os, node->funcSpec );

		if ( node->type && node->isTypeFixed ) {
			node->type->accept( *this );
		} else {
			if (!node->type_params.empty()) {
				os << "forall" << endl;
				++indent;
				printAll(node->type_params);
				os << indent;
				--indent;

				if (!node->assertions.empty()) {
					os << "with assertions" << endl;
					++indent;
					printAll(node->assertions);
					os << indent;
					--indent;
				}
			}

			os << "function" << endl;
			if ( ! node->params.empty() ) {
				os << indent << "... with parameters" << endl;
				++indent;
				printAll( node->params );
				if ( node->type->isVarArgs ) {
					os << indent << "and a variable number of other arguments" << endl;
				}
				--indent;
			} else if ( node->type->isVarArgs ) {
				os << indent+1 << "accepting unspecified arguments" << endl;
			}

			os << indent << "... returning";
			if ( node->returns.empty() ) {
				os << " nothing" << endl;
			} else {
				os << endl;
				++indent;
				printAll( node->returns );
				--indent;
			}
		}

		if ( ! node->withExprs.empty() ) {
			// Not with a clause, but the 'with clause'.
			++indent;
			os << " with clause" << endl << indent;
			printAll( node->withExprs );
			--indent;
		}

		if ( ! short_mode && node->stmts ) {
			++indent;
			os << " with body" << endl << indent;
			node->stmts->accept( *this );
			--indent;
		}

		return node;
	}

	virtual const ast::Decl * visit( const ast::StructDecl * node ) override final {
		print(node);
		return node;
	}

	virtual const ast::DeclWithType * visit( const ast::InlineMemberDecl * node ) override final {
		os << "inline ";
		if ( ! node->name.empty() ) os << node->name;

		return node;
	}

	virtual const ast::Decl * visit( const ast::UnionDecl * node ) override final {
		print(node);
		return node;
	}

	virtual const ast::Decl * visit( const ast::EnumDecl * node ) override final {
		print(node);
		return node;
	}

	virtual const ast::Decl * visit( const ast::TraitDecl * node ) override final {
		print(node);
		return node;
	}

	virtual const ast::Decl * visit( const ast::TypeDecl * node ) override final {
		preprint( node );
		if ( ! short_mode && node->init ) {
			os << endl << indent << "with type initializer: ";
			++indent;
			node->init->accept( *this );
			--indent;
		}

		return node;
	}

	virtual const ast::Decl * visit( const ast::TypedefDecl * node ) override final {
		preprint( node );
		return node;
	}

	virtual const ast::AsmDecl * visit( const ast::AsmDecl * node ) override final {
		safe_print( node->stmt );
		return node;
	}

	virtual const ast::DirectiveDecl * visit( const ast::DirectiveDecl * node ) override final {
		safe_print( node->stmt );
		return node;
	}

	virtual const ast::StaticAssertDecl * visit( const ast::StaticAssertDecl * node ) override final {
		os << "Static Assert with condition: ";
		++indent;
		safe_print( node->cond );
		os << endl << indent-1 << "and message: ";
		safe_print( node->msg );
		--indent;
		os << endl;

		return node;
	}

	virtual const ast::CompoundStmt * visit( const ast::CompoundStmt * node ) override final {
		os << "Compound Statement:" << endl;
		++indent;
		printAll( node->kids );
		--indent;
		return node;
	}

	virtual const ast::Stmt * visit( const ast::ExprStmt * node ) override final {
		++indent;
		os << "Expression Statement:" << endl << indent;
		safe_print( node->expr );
		--indent;
		return node;
	}

	virtual const ast::Stmt * visit( const ast::AsmStmt * node ) override final {
		os << "Assembler Statement:" << endl;
		++indent;
		os << indent-1 << "instruction:" << endl << indent;
		safe_print( node->instruction );
		if ( ! node->output.empty() ) {
			os << endl << indent << "output:" << endl;
			printAll( node->output );
		} // if
		if ( ! node->input.empty() ) {
			os << indent << "input:" << endl;
			printAll( node->input );
		} // if
		if ( ! node->clobber.empty() ) {
			os << indent << "clobber:" << endl;
			printAll( node->clobber );
		} // if
		--indent;
		return node;
	}

	virtual const ast::Stmt * visit( const ast::DirectiveStmt * node ) override final {
		os << "GCC Directive: " << node->directive << endl;
		return node;
	}

	virtual const ast::Stmt * visit( const ast::IfStmt * node ) override final {
		os << "If on condition:" << endl;
		++indent;
		os << indent;
		safe_print( node->cond );
		--indent;

		if ( ! node->inits.empty() ) {
			os << indent << "... with initialization:" << endl;
			++indent;
			for ( const ast::Stmt * stmt : node->inits ) {
				os << indent;
				safe_print( stmt );
			}
			--indent;
			os << endl;
		}

		os << indent << "... then:" << endl;

		++indent;
		os << indent;
		safe_print( node->then );
		--indent;

		if ( node->else_ != 0 ) {
			os << indent << "... else:" << endl;
			++indent;
			os << indent;
			node->else_->accept( *this );
			--indent;
		} // if
		return node;
	}

	virtual const ast::Stmt * visit( const ast::WhileDoStmt * node ) override final {
		if ( node->isDoWhile ) { os << "Do-"; }
		os << "While on condition:" << endl;
		++indent;
		safe_print( node->cond );
		os << indent-1 << "... with body:" << endl;
		safe_print( node->body );

		if ( ! node->inits.empty() ) {
			os << indent-1 << "... with inits:" << endl;
			printAll( node->inits );
		}
		--indent;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::ForStmt * node ) override final {
		os << "For Statement" << endl;

		if ( ! node->inits.empty() ) {
			os << indent << "... initialization:" << endl;
			++indent;
			for ( const ast::Stmt * stmt : node->inits ) {
				os << indent+1;
				safe_print( stmt );
			}
			--indent;
		}

		if ( node->cond ) {
			os << indent << "... condition:" << endl;
			++indent;
			os << indent;
			node->cond->accept( *this );
			--indent;
		}

		if ( node->inc ) {
			os << indent << "... increment:" << endl;
			++indent;
			os << indent;
			node->inc->accept( *this );
			--indent;
		}

		if ( node->body ) {
			os << indent << "... with body:" << endl;
			++indent;
			os << indent;
			node->body->accept( *this );
			--indent;
		}
		os << endl;
		print( node->labels );

		return node;
	}

	virtual const ast::Stmt * visit( const ast::SwitchStmt * node ) override final {
		os << "Switch on condition: ";
		safe_print( node->cond );
		os << endl;

		++indent;
		for ( const ast::CaseClause * stmt : node->cases ) {
			stmt->accept( *this );
		}
		--indent;

		return node;
	}

	virtual const ast::CaseClause * visit( const ast::CaseClause * node ) override final {
		if ( node->isDefault() ) {
			os << indent << "Default ";
		} else {
			os << indent << "Case ";
			safe_print( node->cond );
		} // if
		os << endl;

		++indent;
		for ( const ast::Stmt * stmt : node->stmts ) {
			os << indent;
			stmt->accept( *this );
		}
		--indent;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::BranchStmt * node ) override final {
		os << "Branch (" << node->kindName() << ")" << endl;
		++indent;
		if ( ! node->target.empty() ) {
			os << indent << "with target: " << node->target << endl;
		}

		if ( ! node->originalTarget.empty() ) {
			os << indent << "with original target: " << node->originalTarget << endl;
		}

		if ( node->computedTarget ) {
			os << indent << "with computed target: ";
			node->computedTarget->accept( *this );
			os << endl;
		}
		--indent;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::ReturnStmt * node ) override final {
		os << "Return Statement, returning";
		if ( node->expr ) {
			++indent;
			os << ":" << endl << indent;
			node->expr->accept( *this );
			--indent;
		} else {
			os << " void";
		}
		os << endl;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::ThrowStmt * node ) override final {
		if ( node->target ) os << "Non-Local ";

		switch( node->kind ) {
		case ast::ExceptionKind::Terminate: os << "Terminate "; break;
		case ast::ExceptionKind::Resume:    os << "Resume ";    break;
		}

		++indent;
		os << "Throw Statement, raising: ";
		safe_print( node->expr );
		if ( node->target ) {
			os << "... at: ";
			node->target->accept( *this );
		}
		--indent;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::TryStmt * node ) override final {
		++indent;
		os << "Try Statement" << endl << indent-1
		   << "... with block:" << endl << indent;
		safe_print( node->body );

		os << indent-1 << "... and handlers:" << endl;
		for ( const ast::CatchClause * stmt : node->handlers ) {
			os << indent;
			stmt->accept( *this );
		}

		if ( node->finally ) {
			os << indent-1 << "... and finally:" << endl << indent;
			node->finally->accept( *this );
		}
		--indent;

		return node;
	}

	virtual const ast::CatchClause * visit( const ast::CatchClause * node ) override final {
		os << "Catch ";
		switch ( node->kind ) {
		case ast::ExceptionKind::Terminate: os << "Terminate "; break;
		case ast::ExceptionKind::Resume:    os << "Resume ";    break;
		}
		os << "Statement" << endl << indent;

		++indent;
		os << "... catching: ";
		short_print( node->decl );
		os << endl;

		if ( node->cond ) {
			os << indent-1 << "... with conditional:" << endl << indent;
			node->cond->accept( *this );
		}

		os << indent-1 << "... with block:" << endl << indent;
		safe_print( node->body );
		--indent;

		return node;
	}

	virtual const ast::FinallyClause * visit( const ast::FinallyClause * node ) override final {
		os << "Finally Statement" << endl;
		os << indent << "... with block:" << endl;
		++indent;
		os << indent;
		safe_print( node->body );
		--indent;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::SuspendStmt * node ) override final {
		os << "Suspend Statement";
		switch (node->kind) {
		case ast::SuspendStmt::None     : os << " with implicit target"; break;
		case ast::SuspendStmt::Generator: os << " for generator"; break;
		case ast::SuspendStmt::Coroutine: os << " for coroutine"; break;
		}
		os << endl;

		++indent;
		if(node->then) {
			os << indent << " with post statement :" << endl;
			safe_print( node->then );
		}
		++indent;

		return node;
	}

	virtual const ast::WhenClause * visit( const ast::WhenClause * node ) override final {
		os << indent-1 << "target: ";
		safe_print( node->target );

		if ( node->stmt ) {
			os << indent-1 << "... with statment:" << endl;
			node->stmt->accept( *this );
		}

		if ( node->when_cond ) {
			os << indent-1 << "... with when condition:" << endl;
			node->when_cond->accept( *this );
		}

		return node;
	}

	virtual const ast::Stmt * visit( const ast::WaitForStmt * node ) override final {
		os << "Waitfor Statement" << endl;
		indent += 2;
		for( const auto & clause : node->clauses ) {
			clause->accept( *this );
		}

		if ( node->timeout_time ) {
			os << indent-1 << "timeout of:" << endl;
			node->timeout_time->accept( *this );

			if ( node->timeout_stmt ) {
				os << indent-1 << "... with statment:" << endl;
				node->timeout_stmt->accept( *this );
			}

			if ( node->timeout_cond ) {
				os << indent-1 << "... with condition:" << endl;
				node->timeout_cond->accept( *this );
			}
		}

		if ( node->else_stmt ) {
			os << indent-1 << "else:" << endl;
			node->else_stmt->accept( *this );

			if ( node->else_cond ) {
				os << indent-1 << "... with condition:" << endl;
				node->else_cond->accept( *this );
			}
		}

		return node;
	}

	virtual const ast::WaitForClause * visit( const ast::WaitForClause * node ) override final {
		os << indent-1 << "target function: ";
		safe_print( node->target );

		if ( !node->target_args.empty() ) {
			os << endl << indent-1 << "... with arguments:" << endl;
			for( const ast::Expr * arg : node->target_args ) {
				arg->accept( *this );
			}
		}

		if ( node->stmt ) {
			os << indent-1 << "... with statment:" << endl;
			node->stmt->accept( *this );
		}

		if ( node->when_cond ) {
			os << indent-1 << "... with condition:" << endl;
			node->when_cond->accept( *this );
		}

		return node;
	}

    virtual const ast::Stmt * visit( const ast::WaitUntilStmt * node ) override final {
		os << "Waituntil Statement" << endl;
		indent += 2;
		for( const auto & clause : node->clauses ) {
			clause->accept( *this );
		}
        print(node);    // calls print( const ast::WaitStmt * node )
		return node;
	}

	virtual const ast::Decl * visit( const ast::WithStmt * node ) override final {
		os << "With statement" << endl;
		os << indent << "... with expressions:" << endl;
		++indent;
		printAll( node->exprs );
		os << indent-1 << "... with statement:" << endl << indent;
		safe_print( node->stmt );
		--indent;

		return node;
	}

	virtual const ast::NullStmt * visit( const ast::NullStmt * node ) override final {
		os << "Null Statement" << endl;
		print( node->labels );

		return node;
	}

	virtual const ast::Stmt * visit( const ast::DeclStmt * node ) override final {
		os << "Declaration of ";
		safe_print( node->decl );

		return node;
	}

	virtual const ast::Stmt * visit( const ast::ImplicitCtorDtorStmt * node ) override final {
		os << "Implicit Ctor Dtor Statement" << endl;
		os << indent << "... with Ctor/Dtor: ";
		++indent;
		safe_print( node->callStmt );
		--indent;
		os << endl;

		return node;
	}

	virtual const ast::Stmt * visit( const ast::MutexStmt * node ) override final {
		os << "Mutex Statement" << endl;
		os << indent << "... with Mutex Parameters: ";
		++indent;
		printAll( node->mutexObjs );
		--indent;
		os << indent << "... with Statement: ";
		++indent;
		safe_print( node->stmt );
		--indent;
		os << endl;

		return node;
	}

	virtual const ast::Expr * visit( const ast::ApplicationExpr * node ) override final {
		++indent;
		os << "Application of" << endl << indent;
		safe_print( node->func );
		os << endl;
		if ( ! node->args.empty() ) {
			os << indent << "... to arguments" << endl;
			printAll( node->args );
		}
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UntypedExpr * node ) override final {
		++indent;
		os << "Applying untyped:" << endl;
		os << indent;
		safe_print( node->func );
		os << endl << indent-1 << "...to:" << endl;
		printAll( node->args );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::NameExpr * node ) override final {
		os << "Name: " << node->name;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::QualifiedNameExpr * node ) override final {
		os << "QualifiedNameExpr: " << std::endl;
		os << ++indent << "Type: ";
		safe_print( node->type_decl );
		os << std::endl;
		os <<  indent << "Name: " << node->name  << std::endl;
		--indent;
		postprint( node );
		return node;
	}

	virtual const ast::Expr * visit( const ast::AddressExpr * node ) override final {
		os << "Address of:" << endl;
		++indent;
		os << indent;
		safe_print( node->arg );

		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::LabelAddressExpr * node ) override final {
		os << "Address of label:" << node->arg;

		return node;
	}

	virtual const ast::Expr * visit( const ast::CastExpr * node ) override final {
		++indent;
		os << (node->isGenerated ? "Generated" : "Explicit") << " Cast of:" << endl << indent;
		safe_print( node->arg );
		os << endl << indent-1 << "... to:";
		if ( ! node->result ) {
			os << " ";
			undefined();
		} else if ( node->result->isVoid() ) {
			os << " nothing";
		} else {
			os << endl << indent;
			node->result->accept( *this );
		} // if
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::KeywordCastExpr * node ) override final {
		++indent;
		os << "Keyword Cast of:" << endl << indent;
		safe_print( node->arg );
		--indent;
		os << endl << indent << "... to: " << node->targetString();
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::VirtualCastExpr * node ) override final {
		++indent;
		os << "Virtual Cast of:" << endl << indent;
		safe_print( node->arg );
		os << endl << indent-1 << "... to:";
		if ( ! node->result ) {
			os << " unknown";
		} else {
			os << endl << indent;
			node->result->accept( *this );
		}
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UntypedMemberExpr * node ) override final {
		++indent;
		os << "Untyped Member Expression, with field: " << endl << indent;
		safe_print( node->member );
		os << indent-1 << "... from aggregate:" << endl << indent;
		safe_print( node->aggregate );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::MemberExpr * node ) override final {
		++indent;
		os << "Member Expression, with field:" << endl << indent;
		safe_print( node->member );
		os << endl << indent-1 << "... from aggregate:" << endl << indent;
		safe_print( node->aggregate );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::VariableExpr * node ) override final {
		os << "Variable Expression: ";
		short_print( node->var );
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::ConstantExpr * node ) override final {
		os << "Constant Expression (" << node->rep;
		if ( node->result ) {
			os << ": ";
			node->result->accept( *this );
		}
		os << ")";
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::SizeofExpr * node ) override final {
		os << "Sizeof Expression on: ";
		++indent;
		if ( node->type ) node->type->accept( *this );
		else safe_print( node->expr );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::AlignofExpr * node ) override final {
		os << "Alignof Expression on: ";
		++indent;
		if ( node->type ) node->type->accept( *this );
		else safe_print( node->expr );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UntypedOffsetofExpr * node ) override final {
		os << "Untyped Offsetof Expression on member " << node->member << " of ";
		++indent;
		safe_print( node->type );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::OffsetofExpr * node ) override final {
		os << "Offsetof Expression on member " << node->member->name << " of ";
		++indent;
		safe_print( node->type );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::OffsetPackExpr * node ) override final {
		os << "Offset Pack Expression on: ";
		++indent;
		safe_print( node->type );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::LogicalExpr * node ) override final {
		os << "Short-circuited operation (" << (node->isAnd ? "and" : "or") << ") on: ";
		safe_print( node->arg1 );
		os << " and ";
		safe_print( node->arg2 );
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::ConditionalExpr * node ) override final {
		++indent;
		os << "Conditional expression on:" << endl << indent;
		safe_print( node->arg1 );
		os << indent-1 << "First alternative:" << endl << indent;
		safe_print( node->arg2 );
		os << indent-1 << "Second alternative:" << endl << indent;
		safe_print( node->arg3 );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::CommaExpr * node ) override final {
		++indent;
		os << "Comma Expression:" << endl << indent;
		safe_print( node->arg1 );
		os << endl << indent;
		safe_print( node->arg2 );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::TypeExpr * node ) override final {
		safe_print( node->type );
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::DimensionExpr * node ) override final {
		os << "Type-Sys Value: " << node->name;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::AsmExpr * node ) override final {
		os << "Asm Expression:" << endl;
		++indent;
		if ( !node->inout.empty() ) os << "[" << node->inout << "] ";
		if ( node->constraint ) node->constraint->accept( *this );
		if ( node->operand ) node->operand->accept( *this );
		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::ImplicitCopyCtorExpr * node ) override final {
		++indent;
		os << "Implicit Copy Constructor Expression:" << endl << indent;
		safe_print( node->callExpr );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::ConstructorExpr * node ) override final {
		os <<  "Constructor Expression:" << endl << indent+1;
		indent += 2;
		safe_print( node->callExpr );
		indent -= 2;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::CompoundLiteralExpr * node ) override final {
		++indent;
		os << "Compound Literal Expression: " << endl << indent;
		safe_print( node->result );
		os << indent;
		safe_print( node->init );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::RangeExpr * node ) override final {
		os << "Range Expression: ";
		safe_print( node->low );
		os << " ... ";
		safe_print( node->high );
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UntypedTupleExpr * node ) override final {
		os << "Untyped Tuple:" << endl;
		++indent;
		printAll( node->exprs );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::TupleExpr * node ) override final {
		os << "Tuple:" << endl;
		++indent;
		printAll( node->exprs );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::TupleIndexExpr * node ) override final {
		os << "Tuple Index Expression, with tuple:" << endl;
		++indent;
		os << indent;
		safe_print( node->tuple );
		os << indent << "with index: " << node->index << endl;
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::TupleAssignExpr * node ) override final {
		os << "Tuple Assignment Expression, with stmt expr:" << endl;
		++indent;
		os << indent;
		safe_print( node->stmtExpr );
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::StmtExpr * node ) override final {
		++indent;
		os << "Statement Expression:" << endl << indent;
		safe_print( node->stmts );
		if ( ! node->returnDecls.empty() ) {
			os << indent << "... with returnDecls: ";
			printAll( node->returnDecls );
		}
		if ( ! node->dtors.empty() ) {
			os << indent << "... with dtors: ";
			printAll( node->dtors );
		}
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UniqueExpr * node ) override final {
		++indent;
		os << "Unique Expression with id: " << node->id << endl << indent;
		safe_print( node->expr );
		if ( node->object ) {
			os << indent-1 << "... with decl: ";
			short_print( node->object );
		}
		--indent;
		postprint( node );

		return node;
	}

	virtual const ast::Expr * visit( const ast::UntypedInitExpr * node ) override final {
		++indent;
		os << "Untyped Init Expression" << endl << indent;
		safe_print( node->expr );
		if ( ! node->initAlts.empty() ) {
			for ( const InitAlternative & alt : node->initAlts ) {
				os << indent <<  "InitAlternative: ";
				safe_print( alt.type );
				safe_print( alt.designation );
			}
		}
		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::InitExpr * node ) override final {
		++indent;
		os << "Init Expression" << endl << indent;
		safe_print( node->expr );
		os << indent << "... with designation: ";
		safe_print( node->designation );
		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::DeletedExpr * node ) override final {
		++indent;
		os << "Deleted Expression" << endl << indent;
		safe_print( node->expr );
		os << endl << indent << "... deleted by: ";
		safe_print( node->deleteStmt );
		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::DefaultArgExpr * node ) override final {
		++indent;
		os << "Default Argument Expression" << endl << indent;
		safe_print( node->expr );
		--indent;

		return node;
	}

	virtual const ast::Expr * visit( const ast::GenericExpr * node ) override final {
		++indent;
		os << "C11 _Generic Expression" << endl << indent;
		safe_print( node->control );
		os << endl << indent << "... with associations:" << endl;
		for ( const auto & assoc : node->associations ) {
			os << indent;
			if ( assoc.type ) {
				os << "... type: ";
				assoc.type->accept( *this );
				os << endl << indent << "... expression: ";
				safe_print( assoc.expr );
			} else {
				os << "... default: ";
				safe_print( assoc.expr );
			}
			os << endl;
		}
		--indent;

		return node;
	}

	virtual const ast::Type * visit( const ast::VoidType * node ) override final {
		preprint( node );
		os << "void";
		return node;
	}

	virtual const ast::Type * visit( const ast::BasicType * node ) override final {
		preprint( node );
		os << ast::BasicType::typeNames[ node->kind ];
		return node;
	}

	virtual const ast::Type * visit( const ast::PointerType * node ) override final {
		preprint( node );
		if ( ! node->isArray() ) {
			os << "pointer to ";
		} else {
			os << "decayed ";
			if ( node->isStatic ) {
				os << "static ";
			}

			if ( node->isVarLen ) {
				os << "variable length array of ";
			} else if ( node->dimension ) {
				os << "array of ";
				node->dimension->accept( *this );
				os << " ";
			}
		}
		safe_print( node->base );

		return node;
	}

	virtual const ast::Type * visit( const ast::ArrayType * node ) override final {
		preprint( node );
		if ( node->isStatic ) {
			os << "static ";
		}

		if ( node->isVarLen ) {
			os << "variable length array of ";
		} else if ( node->dimension ) {
			os << "array of ";
		} else {
			os << "open array of ";
		}

		safe_print( node->base );

		if ( node->dimension ) {
			os << " with dimension of ";
			node->dimension->accept( *this );
		}

		return node;
	}

	virtual const ast::Type * visit( const ast::ReferenceType * node ) override final {
		preprint( node );
		os << "reference to ";
		safe_print( node->base );

		return node;
	}

	virtual const ast::Type * visit( const ast::QualifiedType * node ) override final {
		preprint( node );
		++indent;
		os << "Qualified Type:" << endl << indent;
		safe_print( node->parent );
		os << endl << indent;
		safe_print( node->child );
		os << endl;
		--indent;

		return node;
	}

	virtual const ast::Type * visit( const ast::FunctionType * node ) override final {
		preprint( node );

		os << "function" << endl;
		if ( ! node->params.empty() ) {
			os << indent << "... with parameters" << endl;
			++indent;
			printAll( node->params );
			if ( node->isVarArgs ) {
				os << indent << "and a variable number of other arguments" << endl;
			}
			--indent;
		} else if ( node->isVarArgs ) {
			os << indent+1 << "accepting unspecified arguments" << endl;
		}

		os << indent << "... returning";
		if ( node->returns.empty() ) {
			os << " nothing" << endl;
		} else {
			os << endl;
			++indent;
			printAll( node->returns );
			--indent;
		}

		return node;
	}

	virtual const ast::Type * visit( const ast::StructInstType * node ) override final {
		preprint( node );
		os << "instance of struct " << node->name;
		if ( node->base ) {
			os << " " << ( node->base->body ? "with" : "without" ) << " body";
		}
		print( node->params );

		return node;
	}

	virtual const ast::Type * visit( const ast::UnionInstType * node ) override final {
		preprint( node );
		os << "instance of union " << node->name;
		if ( node->base ) {
			os << " " << ( node->base->body ? "with" : "without" ) << " body";
		}
		print( node->params );

		return node;
	}

	virtual const ast::Type * visit( const ast::EnumInstType * node ) override final {
		preprint( node );
		os << "instance of enum " << node->name;
		if ( node->base ) {
			os << " " << ( node->base->body ? "with" : "without" ) << " body";
		}
		print( node->params );

		return node;
	}

	virtual const ast::Type * visit( const ast::TraitInstType * node ) override final {
		preprint( node );
		os << "instance of trait " << node->name;
		print( node->params );

		return node;
	}

	virtual const ast::Type * visit( const ast::TypeInstType * node ) override final {
		preprint( node );
		const auto & _name = deterministic_output && isUnboundType(node) ? "[unbound]" : node->typeString();
		os << "instance of type " << _name
		   << " (" << (node->kind == ast::TypeDecl::Ftype ? "" : "not ") << "function type)";
		print( node->params );

		return node;
	}

	virtual const ast::Type * visit( const ast::TupleType * node ) override final {
		preprint( node );
		os << "tuple of types" << endl;
		++indent;
		printAll( node->types );
		--indent;

		return node;
	}

	virtual const ast::Type * visit( const ast::TypeofType * node ) override final {
		preprint( node );
		if ( node->kind == ast::TypeofType::Basetypeof ) { os << "base-"; }
		os << "type-of expression ";
		safe_print( node->expr );

		return node;
	}

	virtual const ast::Type * visit( const ast::VTableType * node ) override final {
		preprint( node );
		os << "vtable for ";
		safe_print( node->base );

		return node;
	}

	virtual const ast::Type * visit( const ast::VarArgsType * node ) override final {
		preprint( node );
		os << "builtin var args pack";
		return node;
	}

	virtual const ast::Type * visit( const ast::ZeroType * node ) override final {
		preprint( node );
		os << "zero_t";
		return node;
	}

	virtual const ast::Type * visit( const ast::OneType * node ) override final {
		preprint( node );
		os << "one_t";
		return node;
	}

	virtual const ast::Type * visit( const ast::GlobalScopeType * node ) override final {
		preprint( node );
		os << "Global Scope Type";
		return node;
	}

	virtual const ast::Designation * visit( const ast::Designation * node ) override final {
		if ( node->designators.empty() ) return node;
		os << "... designated by: " << endl;
		++indent;
		for ( const ast::Expr * d : node->designators ) {
			os << indent;
			d->accept( *this );
			os << endl;
		}
		--indent;
		return node;
	}

	virtual const ast::Init * visit( const ast::SingleInit * node ) override final {
		os << "Simple Initializer: ";
		safe_print( node->value );
		return node;
	}

	virtual const ast::Init * visit( const ast::ListInit * node ) override final {
		os << "Compound initializer: " << endl;
		++indent;
		for ( auto p : group_iterate( node->designations, node->initializers ) ) {
			const ast::Designation * d = std::get<0>(p);
			const ast::Init * init = std::get<1>(p);
			os << indent;
			init->accept( *this );
			os << endl;
			if ( ! d->designators.empty() ) {
				os << indent;
				d->accept( *this );
			}
		}
		--indent;
		return node;
	}

	virtual const ast::Init * visit( const ast::ConstructorInit * node ) override final {
		os << "Constructor initializer: " << endl;
		if ( node->ctor ) {
			os << indent << "... initially constructed with ";
			++indent;
			node->ctor->accept( *this );
			--indent;
		}

		if ( node->dtor ) {
			os << indent << "... destructed with ";
			++indent;
			node->dtor->accept( *this );
			--indent;
		}

		if ( node->init ) {
			os << indent << "... with fallback C-style initializer: ";
			++indent;
			node->init->accept( *this );
			--indent;
		}
		return node;
	}

	virtual const ast::Attribute * visit( const ast::Attribute * node ) override final {
		if ( node->empty() ) return node;
		os << "Attribute with name: " << node->name;
		if ( node->params.empty() ) return node;
		os << " with parameters: " << endl;
		++indent;
		printAll( node->params );
		--indent;
		return node;
	}

	virtual const ast::TypeSubstitution * visit( const ast::TypeSubstitution * node ) override final {
		os << indent << "Types:" << endl;
		for ( const auto& i : *node ) {
			os << indent+1 << i.first.typeString() << " -> ";
			indent += 2;
			safe_print( i.second );
			indent -= 2;
			os << endl;
		}
		return node;
	}

};

} // namespace

void print( ostream & os, const ast::Node * node, Indenter indent ) {
	Printer printer { os, indent, false };
	node->accept(printer);
}

void printShort( ostream & os, const ast::Decl * node, Indenter indent ) {
	Printer printer { os, indent, true };
	node->accept(printer);
}

void print( ostream & os, Function::Specs specs ) {
	print( os, specs, Names::FuncSpecifiers );
}

void print( ostream & os, Storage::Classes storage ) {
	print( os, storage, Names::StorageClasses );
}

void print( ostream & os, CV::Qualifiers qualifiers ) {
	print( os, qualifiers, Names::Qualifiers );
}

} // namespace ast
