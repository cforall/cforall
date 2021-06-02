//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptVisitor.cc --
//
// Author           : Andrew Beach
// Created On       : Wed Jun 14 16:49:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 24 11:18:00 2020
// Update Count     : 17
//

#include "ExceptTranslate.h"

#include <stddef.h>                   // for NULL
#include <cassert>                    // for assert, assertf
#include <iterator>                   // for back_inserter, inserter
#include <string>                     // for string, operator==

#include "Common/PassVisitor.h"       // for PassVisitor, WithGuards
#include "Common/SemanticError.h"     // for SemanticError
#include "Common/utility.h"           // for CodeLocation
#include "SynTree/LinkageSpec.h"      // for Cforall
#include "SynTree/Attribute.h"        // for Attribute
#include "SynTree/Constant.h"         // for Constant
#include "SynTree/Declaration.h"      // for ObjectDecl, FunctionDecl, Struc...
#include "SynTree/Expression.h"       // for UntypedExpr, ConstantExpr, Name...
#include "SynTree/Initializer.h"      // for SingleInit, ListInit
#include "SynTree/Label.h"            // for Label
#include "SynTree/Mutator.h"          // for mutateAll
#include "SynTree/Statement.h"        // for CompoundStmt, CatchStmt, ThrowStmt
#include "SynTree/Type.h"             // for FunctionType, Type, noQualifiers
#include "SynTree/DeclReplacer.h"     // for DeclReplacer
#include "SynTree/Visitor.h"          // for acceptAll

namespace ControlStruct {

	// Buricratic Helpers (Not having to do with the paritular operation.)

	typedef std::list<CatchStmt*> CatchList;

	void split( CatchList& allHandlers, CatchList& terHandlers,
				CatchList& resHandlers ) {
		while ( !allHandlers.empty() ) {
			CatchStmt * stmt = allHandlers.front();
			allHandlers.pop_front();
			if (CatchStmt::Terminate == stmt->get_kind()) {
				terHandlers.push_back(stmt);
			} else {
				resHandlers.push_back(stmt);
			}
		}
	}

	void appendDeclStmt( CompoundStmt * block, Declaration * item ) {
		block->push_back(new DeclStmt(item));
	}

	Expression * nameOf( DeclarationWithType * decl ) {
		return new VariableExpr( decl );
	}

	class ThrowMutatorCore : public WithGuards {
		ObjectDecl * terminate_handler_except;
		enum Context { NoHandler, TerHandler, ResHandler } cur_context;

		// The helper functions for code/syntree generation.
		Statement * create_either_throw(
			ThrowStmt * throwStmt, const char * throwFunc );
		Statement * create_terminate_rethrow( ThrowStmt * throwStmt );

	public:
		ThrowMutatorCore() :
			terminate_handler_except( nullptr ),
			cur_context( NoHandler )
		{}

		void premutate( CatchStmt *catchStmt );
		Statement * postmutate( ThrowStmt *throwStmt );
	};

	// ThrowStmt Mutation Helpers

	Statement * ThrowMutatorCore::create_either_throw(
			ThrowStmt * throwStmt, const char * throwFunc ) {
		// `throwFunc`( `throwStmt->get_name()` );
		UntypedExpr * call = new UntypedExpr( new NameExpr( throwFunc ) );
		call->get_args().push_back( throwStmt->get_expr() );
		throwStmt->set_expr( nullptr );
		delete throwStmt;
		return new ExprStmt( call );
	}

	Statement * ThrowMutatorCore::create_terminate_rethrow(
			ThrowStmt *throwStmt ) {
		// { `terminate_handler_except` = 0p; __rethrow_terminate(); }
		assert( nullptr == throwStmt->get_expr() );
		assert( terminate_handler_except );

		CompoundStmt * result = new CompoundStmt();
		result->labels =  throwStmt->labels;
		result->push_back( new ExprStmt( UntypedExpr::createAssign(
			nameOf( terminate_handler_except ),
			new ConstantExpr( Constant::null(
				terminate_handler_except->get_type()->clone()
				) )
			) ) );
		result->push_back( new ExprStmt(
			new UntypedExpr( new NameExpr( "__cfaehm_rethrow_terminate" ) )
			) );
		delete throwStmt;
		return result;
	}

	// Visiting/Mutating Functions

	void ThrowMutatorCore::premutate( CatchStmt *catchStmt ) {
		// Validate the statement's form.
		ObjectDecl * decl = dynamic_cast<ObjectDecl *>( catchStmt->get_decl() );
		// Also checking the type would be nice.
		if ( !decl || !dynamic_cast<PointerType *>( decl->type ) ) {
			std::string kind = (CatchStmt::Terminate == catchStmt->kind) ? "catch" : "catchResume";
			SemanticError( catchStmt->location, kind + " must have pointer to an exception type" );
		}

		// Track the handler context.
		GuardValue( cur_context );
		if ( CatchStmt::Terminate == catchStmt->get_kind() ) {
			cur_context = TerHandler;

			GuardValue( terminate_handler_except );
			terminate_handler_except = decl;
		} else {
			cur_context = ResHandler;
		}
	}

	Statement * ThrowMutatorCore::postmutate( ThrowStmt *throwStmt ) {
		// Ignoring throwStmt->get_target() for now.
		if ( ThrowStmt::Terminate == throwStmt->get_kind() ) {
			if ( throwStmt->get_expr() ) {
				return create_either_throw( throwStmt, "$throw" );
			} else if ( TerHandler == cur_context ) {
				return create_terminate_rethrow( throwStmt );
			} else {
				abort("Invalid throw in %s at %i\n",
					throwStmt->location.filename.c_str(),
					throwStmt->location.first_line);
			}
		} else {
			if ( throwStmt->get_expr() ) {
				return create_either_throw( throwStmt, "$throwResume" );
			} else if ( ResHandler == cur_context ) {
				// This has to be handled later.
				return throwStmt;
			} else {
				abort("Invalid throwResume in %s at %i\n",
					throwStmt->location.filename.c_str(),
					throwStmt->location.first_line);
			}
		}
	}

	class TryMutatorCore {
		// The built in types used in translation.
		StructDecl * except_decl;
		StructDecl * node_decl;
		StructDecl * hook_decl;

		// The many helper functions for code/syntree generation.
		CompoundStmt * take_try_block( TryStmt * tryStmt );
		FunctionDecl * create_try_wrapper( CompoundStmt * body );
		FunctionDecl * create_terminate_catch( CatchList &handlers );
		CompoundStmt * create_single_matcher(
			DeclarationWithType * except_obj, CatchStmt * modded_handler );
		FunctionDecl * create_terminate_match( CatchList &handlers );
		CompoundStmt * create_terminate_caller( FunctionDecl * try_wrapper,
			FunctionDecl * terminate_catch, FunctionDecl * terminate_match );
		FunctionDecl * create_resume_handler( CatchList &handlers );
		CompoundStmt * create_resume_wrapper(
			Statement * wraps, FunctionDecl * resume_handler );
		FunctionDecl * create_finally_wrapper( TryStmt * tryStmt );
		ObjectDecl * create_finally_hook( FunctionDecl * finally_wrapper );
		Statement * create_resume_rethrow( ThrowStmt * throwStmt );

		// Types used in translation, make sure to use clone.
		// void (*function)();
		FunctionType try_func_t;
		// void (*function)(int, exception);
		FunctionType catch_func_t;
		// int (*function)(exception);
		FunctionType match_func_t;
		// bool (*function)(exception);
		FunctionType handle_func_t;
		// void (*function)(__attribute__((unused)) void *);
		FunctionType finally_func_t;

		StructInstType * create_except_type() {
			assert( except_decl );
			return new StructInstType( noQualifiers, except_decl );
		}
		void init_func_types();

	public:
		TryMutatorCore() :
			except_decl( nullptr ), node_decl( nullptr ), hook_decl( nullptr ),
			try_func_t( noQualifiers, false ),
			catch_func_t( noQualifiers, false ),
			match_func_t( noQualifiers, false ),
			handle_func_t( noQualifiers, false ),
			finally_func_t( noQualifiers, false )
		{}

		void premutate( StructDecl *structDecl );
		Statement * postmutate( TryStmt *tryStmt );
		Statement * postmutate( ThrowStmt *throwStmt );
	};

	void TryMutatorCore::init_func_types() {
		assert( except_decl );

		ObjectDecl index_obj(
			"__handler_index",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			/*bitfieldWidth*/ NULL,
			new BasicType( noQualifiers, BasicType::SignedInt ),
			/*init*/ NULL
			);
		ObjectDecl exception_obj(
			"__exception_inst",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			/*bitfieldWidth*/ NULL,
			new PointerType(
				noQualifiers,
				new StructInstType( noQualifiers, except_decl )
				),
			/*init*/ NULL
			);
		ObjectDecl bool_obj(
			"__ret_bool",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			/*bitfieldWidth*/ NULL,
			new BasicType( noQualifiers, BasicType::Bool ),
			/*init*/ NULL,
			std::list<Attribute *>{ new Attribute( "unused" ) }
			);
		ObjectDecl voidptr_obj(
			"__hook",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			NULL,
			new PointerType(
				noQualifiers,
				new VoidType(
					noQualifiers
					),
				std::list<Attribute *>{ new Attribute( "unused" ) }
				),
			NULL
			);

		ObjectDecl * unused_index_obj = index_obj.clone();
		unused_index_obj->attributes.push_back( new Attribute( "unused" ) );

		catch_func_t.get_parameters().push_back( index_obj.clone() );
		catch_func_t.get_parameters().push_back( exception_obj.clone() );
		match_func_t.get_returnVals().push_back( unused_index_obj );
		match_func_t.get_parameters().push_back( exception_obj.clone() );
		handle_func_t.get_returnVals().push_back( bool_obj.clone() );
		handle_func_t.get_parameters().push_back( exception_obj.clone() );
		finally_func_t.get_parameters().push_back( voidptr_obj.clone() );
	}

	// TryStmt Mutation Helpers

	CompoundStmt * TryMutatorCore::take_try_block( TryStmt *tryStmt ) {
		CompoundStmt * block = tryStmt->get_block();
		tryStmt->set_block( nullptr );
		return block;
	}

	FunctionDecl * TryMutatorCore::create_try_wrapper(
			CompoundStmt *body ) {

		return new FunctionDecl( "try", Type::StorageClasses(),
			LinkageSpec::Cforall, try_func_t.clone(), body );
	}

	FunctionDecl * TryMutatorCore::create_terminate_catch(
			CatchList &handlers ) {
		std::list<CaseStmt *> handler_wrappers;

		FunctionType *func_type = catch_func_t.clone();
		DeclarationWithType * index_obj = func_type->get_parameters().front();
		DeclarationWithType * except_obj = func_type->get_parameters().back();

		// Index 1..{number of handlers}
		int index = 0;
		CatchList::iterator it = handlers.begin();
		for ( ; it != handlers.end() ; ++it ) {
			++index;
			CatchStmt * handler = *it;

			// case `index`:
			// {
			//     `handler.decl` = { (virtual `decl.type`)`except` };
			//     `handler.body`;
			// }
			// return;
			CompoundStmt * block = new CompoundStmt();

			// Just copy the exception value. (Post Validation)
			ObjectDecl * handler_decl =
				static_cast<ObjectDecl *>( handler->get_decl() );
			ObjectDecl * local_except = handler_decl->clone();
			local_except->set_init(
				new ListInit({ new SingleInit(
					new VirtualCastExpr( nameOf( except_obj ),
						local_except->get_type()
						)
					) })
				);
			block->push_back( new DeclStmt( local_except ) );

			// Add the cleanup attribute.
			local_except->get_attributes().push_back( new Attribute(
				"cleanup",
				{ new NameExpr( "__cfaehm_cleanup_terminate" ) }
				) );

			// Update variables in the body to point to this local copy.
			{
				DeclReplacer::DeclMap mapping;
				mapping[ handler_decl ] = local_except;
				DeclReplacer::replace( handler->body, mapping );
			}

			block->push_back( handler->body );
			handler->body = nullptr;

			std::list<Statement *> caseBody
					{ block, new ReturnStmt( nullptr ) };
			handler_wrappers.push_back( new CaseStmt(
				new ConstantExpr( Constant::from_int( index ) ),
				caseBody
				) );
		}
		// TODO: Some sort of meaningful error on default perhaps?

		std::list<Statement*> stmt_handlers;
		while ( !handler_wrappers.empty() ) {
			stmt_handlers.push_back( handler_wrappers.front() );
			handler_wrappers.pop_front();
		}

		SwitchStmt * handler_lookup = new SwitchStmt(
			nameOf( index_obj ),
			stmt_handlers
			);
		CompoundStmt * body = new CompoundStmt();
		body->push_back( handler_lookup );

		return new FunctionDecl("catch", Type::StorageClasses(),
			LinkageSpec::Cforall, func_type, body);
	}

	// Create a single check from a moddified handler.
	// except_obj is referenced, modded_handler will be freed.
	CompoundStmt * TryMutatorCore::create_single_matcher(
			DeclarationWithType * except_obj, CatchStmt * modded_handler ) {
		// {
		//     `modded_handler.decl`
		//     if ( `decl.name = (virtual `decl.type`)`except`
		//             [&& `modded_handler.cond`] ) {
		//         `modded_handler.body`
		//     }
		// }

		CompoundStmt * block = new CompoundStmt();

		// Local Declaration
		ObjectDecl * local_except =
			dynamic_cast<ObjectDecl *>( modded_handler->get_decl() );
		assert( local_except );
		block->push_back( new DeclStmt( local_except ) );

		// Check for type match.
		Expression * cond = UntypedExpr::createAssign( nameOf( local_except ),
			new VirtualCastExpr( nameOf( except_obj ),
				local_except->get_type()->clone() ) );

		// Add the check on the conditional if it is provided.
		if ( modded_handler->get_cond() ) {
			cond = new LogicalExpr( cond, modded_handler->get_cond() );
		}
		// Construct the match condition.
		block->push_back( new IfStmt(
			cond, modded_handler->get_body(), nullptr ) );

		modded_handler->set_decl( nullptr );
		modded_handler->set_cond( nullptr );
		modded_handler->set_body( nullptr );
		delete modded_handler;
		return block;
	}

	FunctionDecl * TryMutatorCore::create_terminate_match(
			CatchList &handlers ) {
		// int match(exception * except) {
		//     HANDLER WRAPPERS { return `index`; }
		// }

		CompoundStmt * body = new CompoundStmt();

		FunctionType * func_type = match_func_t.clone();
		DeclarationWithType * except_obj = func_type->get_parameters().back();

		// Index 1..{number of handlers}
		int index = 0;
		CatchList::iterator it;
		for ( it = handlers.begin() ; it != handlers.end() ; ++it ) {
			++index;
			CatchStmt * handler = *it;

			// Body should have been taken by create_terminate_catch.
			assert( nullptr == handler->get_body() );

			// Create new body.
			handler->set_body( new ReturnStmt(
				new ConstantExpr( Constant::from_int( index ) ) ) );

			// Create the handler.
			body->push_back( create_single_matcher( except_obj, handler ) );
			*it = nullptr;
		}

		body->push_back( new ReturnStmt(
			new ConstantExpr( Constant::from_int( 0 ) ) ) );

		return new FunctionDecl("match", Type::StorageClasses(),
			LinkageSpec::Cforall, func_type, body);
	}

	CompoundStmt * TryMutatorCore::create_terminate_caller(
			FunctionDecl * try_wrapper,
			FunctionDecl * terminate_catch,
			FunctionDecl * terminate_match ) {
		// { __cfaehm_try_terminate(`try`, `catch`, `match`); }

		UntypedExpr * caller = new UntypedExpr( new NameExpr(
			"__cfaehm_try_terminate" ) );
		std::list<Expression *>& args = caller->get_args();
		args.push_back( nameOf( try_wrapper ) );
		args.push_back( nameOf( terminate_catch ) );
		args.push_back( nameOf( terminate_match ) );

		CompoundStmt * callStmt = new CompoundStmt();
		callStmt->push_back( new ExprStmt( caller ) );
		return callStmt;
	}

	FunctionDecl * TryMutatorCore::create_resume_handler(
			CatchList &handlers ) {
		// bool handle(exception * except) {
		//     HANDLER WRAPPERS { `hander->body`; return true; }
		// }
		CompoundStmt * body = new CompoundStmt();

		FunctionType * func_type = handle_func_t.clone();
		DeclarationWithType * except_obj = func_type->get_parameters().back();

		CatchList::iterator it;
		for ( it = handlers.begin() ; it != handlers.end() ; ++it ) {
			CatchStmt * handler = *it;

			// Modifiy body.
			CompoundStmt * handling_code =
				dynamic_cast<CompoundStmt*>( handler->get_body() );
			if ( ! handling_code ) {
				handling_code = new CompoundStmt();
				handling_code->push_back( handler->get_body() );
			}
			handling_code->push_back( new ReturnStmt(
				new ConstantExpr( Constant::from_bool( true ) ) ) );
			handler->set_body( handling_code );

			// Create the handler.
			body->push_back( create_single_matcher( except_obj, handler ) );
			*it = nullptr;
		}

		body->push_back( new ReturnStmt(
			new ConstantExpr( Constant::from_bool( false ) ) ) );

		return new FunctionDecl("handle", Type::StorageClasses(),
			LinkageSpec::Cforall, func_type, body);
	}

	CompoundStmt * TryMutatorCore::create_resume_wrapper(
			Statement * wraps,
			FunctionDecl * resume_handler ) {
		CompoundStmt * body = new CompoundStmt();

		// struct __try_resume_node __resume_node
		//  	__attribute__((cleanup( __cfaehm_try_resume_cleanup )));
		// ** unwinding of the stack here could cause problems **
		// ** however I don't think that can happen currently **
		// __cfaehm_try_resume_setup( &__resume_node, resume_handler );

		std::list< Attribute * > attributes;
		{
			std::list< Expression * > attr_params;
			attr_params.push_back( new NameExpr(
				"__cfaehm_try_resume_cleanup" ) );
			attributes.push_back( new Attribute( "cleanup", attr_params ) );
		}

		ObjectDecl * obj = new ObjectDecl(
			"__resume_node",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			nullptr,
			new StructInstType(
				Type::Qualifiers(),
				node_decl
				),
			nullptr,
			attributes
			);
		appendDeclStmt( body, obj );

		UntypedExpr *setup = new UntypedExpr( new NameExpr(
			"__cfaehm_try_resume_setup" ) );
		setup->get_args().push_back( new AddressExpr( nameOf( obj ) ) );
		setup->get_args().push_back( nameOf( resume_handler ) );

		body->push_back( new ExprStmt( setup ) );

		body->push_back( wraps );
		return body;
	}

	FunctionDecl * TryMutatorCore::create_finally_wrapper(
			TryStmt * tryStmt ) {
		// void finally() { `finally->block` }
		FinallyStmt * finally = tryStmt->get_finally();
		CompoundStmt * body = finally->get_block();
		finally->set_block( nullptr );
		delete finally;
		tryStmt->set_finally( nullptr );

		return new FunctionDecl("finally", Type::StorageClasses(),
			LinkageSpec::Cforall, finally_func_t.clone(), body);
	}

	ObjectDecl * TryMutatorCore::create_finally_hook(
			FunctionDecl * finally_wrapper ) {
		// struct __cfaehm_cleanup_hook __finally_hook
		//   	__attribute__((cleanup( `finally_wrapper` )));

		// Make Cleanup Attribute.
		std::list< Attribute * > attributes;
		{
			std::list< Expression * > attr_params;
			attr_params.push_back( nameOf( finally_wrapper ) );
			attributes.push_back( new Attribute( "cleanup", attr_params ) );
		}

		return new ObjectDecl(
			"__finally_hook",
			Type::StorageClasses(),
			LinkageSpec::Cforall,
			nullptr,
			new StructInstType(
				noQualifiers,
				hook_decl
				),
			nullptr,
			attributes
			);
	}

	Statement * TryMutatorCore::create_resume_rethrow( ThrowStmt *throwStmt ) {
		// return false;
		Statement * result = new ReturnStmt(
			new ConstantExpr( Constant::from_bool( false ) )
			);
		result->labels = throwStmt->labels;
		delete throwStmt;
		return result;
	}

	// Visiting/Mutating Functions
	void TryMutatorCore::premutate( StructDecl *structDecl ) {
		if ( !structDecl->has_body() ) {
			// Skip children?
			return;
		} else if ( structDecl->get_name() == "__cfaehm_base_exception_t" ) {
			assert( nullptr == except_decl );
			except_decl = structDecl;
			init_func_types();
		} else if ( structDecl->get_name() == "__cfaehm_try_resume_node" ) {
			assert( nullptr == node_decl );
			node_decl = structDecl;
		} else if ( structDecl->get_name() == "__cfaehm_cleanup_hook" ) {
			assert( nullptr == hook_decl );
			hook_decl = structDecl;
		}
	}

	Statement * TryMutatorCore::postmutate( TryStmt *tryStmt ) {
		assert( except_decl );
		assert( node_decl );
		assert( hook_decl );

		// Generate a prefix for the function names?

		CompoundStmt * block = new CompoundStmt();
		CompoundStmt * inner = take_try_block( tryStmt );

		if ( tryStmt->get_finally() ) {
			// Define the helper function.
			FunctionDecl * finally_block =
				create_finally_wrapper( tryStmt );
			appendDeclStmt( block, finally_block );
			// Create and add the finally cleanup hook.
			appendDeclStmt( block, create_finally_hook( finally_block ) );
		}

		CatchList termination_handlers;
		CatchList resumption_handlers;
		split( tryStmt->get_catchers(),
			   termination_handlers, resumption_handlers );

		if ( resumption_handlers.size() ) {
			// Define the helper function.
			FunctionDecl * resume_handler =
				create_resume_handler( resumption_handlers );
			appendDeclStmt( block, resume_handler );
			// Prepare hooks
			inner = create_resume_wrapper( inner, resume_handler );
		}

		if ( termination_handlers.size() ) {
			// Define the three helper functions.
			FunctionDecl * try_wrapper = create_try_wrapper( inner );
			appendDeclStmt( block, try_wrapper );
			FunctionDecl * terminate_catch =
				create_terminate_catch( termination_handlers );
			appendDeclStmt( block, terminate_catch );
			FunctionDecl * terminate_match =
				create_terminate_match( termination_handlers );
			appendDeclStmt( block, terminate_match );
			// Build the call to the try wrapper.
			inner = create_terminate_caller(
				try_wrapper, terminate_catch, terminate_match );
		}

		// Embed the try block.
		block->push_back( inner );

		return block;
	}

	Statement * TryMutatorCore::postmutate( ThrowStmt *throwStmt ) {
		// Only valid `throwResume;` statements should remain. (2/3 checks)
		assert( ThrowStmt::Resume == throwStmt->kind && ! throwStmt->expr );
		return create_resume_rethrow( throwStmt );
	}

	void translateThrows( std::list< Declaration *> & translationUnit ) {
		PassVisitor<ThrowMutatorCore> translator;
		mutateAll( translationUnit, translator );
	}

	void translateTries( std::list< Declaration *> & translationUnit ) {
		PassVisitor<TryMutatorCore> translator;
		mutateAll( translationUnit, translator );
	}
}
