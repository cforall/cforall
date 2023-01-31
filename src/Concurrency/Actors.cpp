//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Actors.cpp -- generate code needed by the actor system
//
// Author           : Colby Parsons
// Created On       : Thurs Jan  19 15:34:00 2023
// Last Modified By : Colby Parsons
// Last Modified On : Thurs Jan  19 15:34:00 2023
// Update Count     : 0
//

#include "AST/Print.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Expr.hpp"
using namespace ast;

namespace Concurrency {

struct CollectactorStructDecls : public ast::WithGuards {
    std::map<const StructDecl *, int> & actorStructDecls;
    std::map<const StructDecl *, int>  & messageStructDecls;
    const StructDecl ** requestDecl;
    const EnumDecl ** allocationDecl;
    const StructDecl ** actorDecl;
    const StructDecl ** msgDecl;
    StructDecl * parentDecl;
    bool insideStruct = false;

    void previsit( const EnumDecl * decl ) {
        if( decl->name == "Allocation" ) *allocationDecl = decl;
    }

    void previsit( const StructDecl * decl ) {
        GuardValue(insideStruct);
        insideStruct = true;
        parentDecl = mutate( decl );
        if( decl->name == "actor" ) *actorDecl = decl;
        if( decl->name == "message" ) *msgDecl = decl;
        if( decl->name == "request" ) *requestDecl = decl;        
	}

    void postvisit( const StructInstType * node ) {
        if ( ! *actorDecl || ! *msgDecl ) return;
        if ( insideStruct ) {
            if ( node->aggr() == *actorDecl ) {
                actorStructDecls.insert({parentDecl, 1});
            } else if ( node->aggr() == *msgDecl ) {
                messageStructDecls.insert({parentDecl, 1});
            }
        }
	}

  public:
    CollectactorStructDecls( std::map<const StructDecl *, int> & actorStructDecls, std::map<const StructDecl *, int> & messageStructDecls,
        const StructDecl ** requestDecl, const EnumDecl ** allocationDecl, const StructDecl ** actorDecl, const StructDecl ** msgDecl ) 
        : actorStructDecls( actorStructDecls ), messageStructDecls( messageStructDecls ), requestDecl( requestDecl ), 
        allocationDecl( allocationDecl ), actorDecl(actorDecl), msgDecl(msgDecl) {}
};

struct GenReceiveDecls : public ast::WithDeclsToAdd<> {
    std::map<const StructDecl *, int> & actorStructDecls;
    std::map<const StructDecl *, int>  & messageStructDecls;
    const StructDecl ** requestDecl;
    const EnumDecl ** allocationDecl;
    const StructDecl ** actorDecl;
    const StructDecl ** msgDecl;
    std::vector<FunctionDecl *> & forwardDecls;

	void postvisit( const FunctionDecl * decl ) {
        // return if not of the form receive( param1, param2 ) or if it is a forward decl
        if ( decl->name != "receive" || decl->params.size() != 2 || !decl->stmts ) return;

        // the params should be references
        const ReferenceType * derivedActorRef = dynamic_cast<const ReferenceType *>(decl->params.at(0)->get_type());
        const ReferenceType * derivedMsgRef = dynamic_cast<const ReferenceType *>(decl->params.at(1)->get_type());
        if ( !derivedActorRef || !derivedMsgRef ) return;

        // the references should be to struct instances
        const StructInstType * arg1InstType = dynamic_cast<const StructInstType *>(derivedActorRef->base.get());
        const StructInstType * arg2InstType = dynamic_cast<const StructInstType *>(derivedMsgRef->base.get());
        if ( !arg1InstType || !arg2InstType ) return;

        // If the struct instances are derived actor and message types then generate the message send routine
        if ( actorStructDecls.count( arg1InstType->aggr() ) && messageStructDecls.count( arg2InstType->aggr() ) ) {

            // check that we have found all the decls we need from <actor.hfa>
            if ( !*allocationDecl || !*requestDecl ) 
                SemanticError( decl->location, "using actors requires a header, add #include <actor.hfa>\n" );

            //////////////////////////////////////////////////////////////////////
            // The following generates this send message operator routine for all receive(derived_actor &, derived_msg &) functions
            /*
                static inline derived_actor & ?|?( derived_actor & receiver, derived_msg & msg ) {
                    request * new_req = alloc();
                    Allocation (*my_work_fn)( derived_actor &, derived_msg & ) = receive;
                    __receive_fn fn = (__receive_fn)my_work_fn;
                    (*new_req){ &receiver, &msg, fn };
                    send( receiver, *new_req );
                    return receiver;
                }
            */
            CompoundStmt * sendBody = new CompoundStmt( decl->location );

            // Generates: request * new_req = alloc();
            sendBody->push_back( new DeclStmt(
                decl->location,
                new ObjectDecl(
                    decl->location,
                    "new_req",
                    new PointerType( new StructInstType( *requestDecl ) ),
                    new SingleInit( decl->location, new UntypedExpr( decl->location, new NameExpr( decl->location, "alloc" ), {} ) )
                )
            ));
            
            // Function type is: Allocation (*)( derived_actor &, derived_msg & )
            FunctionType * derivedReceive = new FunctionType();
            derivedReceive->params.push_back( ast::deepCopy( derivedActorRef ) );
            derivedReceive->params.push_back( ast::deepCopy( derivedMsgRef ) );
            derivedReceive->returns.push_back( new EnumInstType( *allocationDecl ) );

            // Generates: Allocation (*my_work_fn)( derived_actor &, derived_msg & ) = receive;
            sendBody->push_back( new DeclStmt(
                decl->location,
                new ObjectDecl(
                    decl->location,
                    "my_work_fn",
                    new PointerType( derivedReceive ),
                    new SingleInit( decl->location, new NameExpr( decl->location, "receive" ) )
                )
            ));

            // Function type is: Allocation (*)( actor &, messsage & )
            FunctionType * genericReceive = new FunctionType();
            genericReceive->params.push_back( new ReferenceType( new StructInstType( *actorDecl ) ) );
            genericReceive->params.push_back( new ReferenceType( new StructInstType( *msgDecl ) ) );
            genericReceive->returns.push_back( new EnumInstType( *allocationDecl ) );

            // Generates: Allocation (*fn)( actor &, messsage & ) = (Allocation (*)( actor &, messsage & ))my_work_fn;
            // More readable synonymous code: 
            //     typedef Allocation (*__receive_fn)(actor &, message &);
            //     __receive_fn fn = (__receive_fn)my_work_fn;
            sendBody->push_back( new DeclStmt(
                decl->location,
                new ObjectDecl(
                    decl->location,
                    "fn",
                    new PointerType( genericReceive ),
                    new SingleInit( decl->location, 
                        new CastExpr( decl->location, new NameExpr( decl->location, "my_work_fn" ), new PointerType( genericReceive ), ExplicitCast )
                    )
                )
            ));

            // Generates: (*new_req){ &receiver, &msg, fn };
            sendBody->push_back( new ExprStmt(
                decl->location,
				new UntypedExpr (
                    decl->location, 
					new NameExpr( decl->location, "?{}" ),
					{
						new UntypedExpr( decl->location, new NameExpr( decl->location, "*?" ), {  new NameExpr( decl->location, "new_req" ) } ),
                        new AddressExpr( new NameExpr( decl->location, "receiver" ) ),
                        new AddressExpr( new NameExpr( decl->location, "msg" ) ),
                        new NameExpr( decl->location, "fn" )
					}
				)
			));

            // Generates: send( receiver, *new_req );
            sendBody->push_back( new ExprStmt(
                decl->location,
				new UntypedExpr (
                    decl->location,
					new NameExpr( decl->location, "send" ),
					{
						{
                            new NameExpr( decl->location, "receiver" ),
                            new UntypedExpr( decl->location, new NameExpr( decl->location, "*?" ), {  new NameExpr( decl->location, "new_req" ) } )
                        }
					}
				)
			));
            
            // Generates: return receiver;
            sendBody->push_back( new ReturnStmt( decl->location, new NameExpr( decl->location, "receiver" ) ) );

            // put it all together into the complete function decl from above
            FunctionDecl * sendOperatorFunction = new FunctionDecl(
                decl->location,
                "?|?",
                {},                     // forall
                {
                    new ObjectDecl(
                        decl->location,
                        "receiver",
                        ast::deepCopy( derivedActorRef )
                    ),
                    new ObjectDecl(
                        decl->location,
                        "msg",
                        ast::deepCopy( derivedMsgRef )
                    )
                },                      // params
                { 
                    new ObjectDecl(
                        decl->location,
                        "receiver_ret",
                        ast::deepCopy( derivedActorRef )
                    )
                },
                nullptr,               // body
                { Storage::Static },    // storage
                Linkage::Cforall,       // linkage
                {},                     // attributes
                { Function::Inline }
            );
            
            // forward decls to resolve use before decl problem for '|' routines
            forwardDecls.push_back( ast::deepCopy( sendOperatorFunction ) );

            sendOperatorFunction->stmts = sendBody;
            declsToAddAfter.push_back( sendOperatorFunction );
        }
	}

  public:
    GenReceiveDecls( std::map<const StructDecl *, int> & actorStructDecls, std::map<const StructDecl *, int> & messageStructDecls,
        const StructDecl ** requestDecl, const EnumDecl ** allocationDecl, const StructDecl ** actorDecl, const StructDecl ** msgDecl, 
        std::vector<FunctionDecl *> & forwardDecls ) : actorStructDecls(actorStructDecls), messageStructDecls(messageStructDecls), 
        requestDecl(requestDecl), allocationDecl(allocationDecl), actorDecl(actorDecl), msgDecl(msgDecl), forwardDecls(forwardDecls) {}
};

struct GenFwdDecls : public ast::WithDeclsToAdd<> {
    std::map<const StructDecl *, int> & actorStructDecls;
    std::map<const StructDecl *, int>  & messageStructDecls;
    std::vector<FunctionDecl *> & forwardDecls;
    bool done;

    void postvisit( const FunctionDecl * decl ) {
        if ( done ) return;
        // return if not of the form receive( param1, param2 ) or if it is a forward decl
        if ( decl->name != "receive" || decl->params.size() != 2 || !decl->stmts ) return;

        // the params should be references
        const ReferenceType * derivedActorRef = dynamic_cast<const ReferenceType *>(decl->params.at(0)->get_type());
        const ReferenceType * derivedMsgRef = dynamic_cast<const ReferenceType *>(decl->params.at(1)->get_type());
        if ( !derivedActorRef || !derivedMsgRef ) return;

        // the references should be to struct instances
        const StructInstType * arg1InstType = dynamic_cast<const StructInstType *>(derivedActorRef->base.get());
        const StructInstType * arg2InstType = dynamic_cast<const StructInstType *>(derivedMsgRef->base.get());
        if ( !arg1InstType || !arg2InstType ) return;

        // If the struct instances are derived actor and message types then generate the message send routine
        if ( actorStructDecls.count( arg1InstType->aggr() ) && messageStructDecls.count( arg2InstType->aggr() ) ) {
            done = true;
            for ( const auto & func : forwardDecls ) {
                declsToAddBefore.push_back( func );
            }
        }
    }

  public:
    GenFwdDecls( std::map<const StructDecl *, int> & actorStructDecls, std::map<const StructDecl *, int> & messageStructDecls, 
        std::vector<FunctionDecl *> & forwardDecls ) : actorStructDecls(actorStructDecls), messageStructDecls(messageStructDecls),
        forwardDecls(forwardDecls), done(false) {}
};

void implementActors( TranslationUnit & translationUnit ) {
    // maps to collect all derived actor and message types
    std::map<const StructDecl *, int> actorStructDecls;
    std::map<const StructDecl *, int> messageStructDecls;
    std::vector<FunctionDecl *> forwardDecls;

    // for storing through the passes
    // these are populated with various important struct decls
    const StructDecl * requestDeclPtr = nullptr;
    const EnumDecl * allocationDeclPtr = nullptr;
    const StructDecl * actorDeclPtr = nullptr;
    const StructDecl * msgDeclPtr = nullptr;

    // double pointer to modify local ptrs above
    const StructDecl ** requestDecl = &requestDeclPtr;
    const EnumDecl ** allocationDecl = &allocationDeclPtr;
    const StructDecl ** actorDecl = &actorDeclPtr;
    const StructDecl ** msgDecl = &msgDeclPtr;

    // first pass collects ptrs to Allocation enum, request type, and generic receive fn typedef
    // also populates maps of all derived actors and messages
    Pass<CollectactorStructDecls>::run( translationUnit, actorStructDecls, messageStructDecls, requestDecl, 
        allocationDecl, actorDecl, msgDecl );
	
    // second pass locates all receive() routines that overload the generic receive fn
    // it then generates the appropriate operator '|' send routines for the receive routines
    Pass<GenReceiveDecls>::run( translationUnit, actorStructDecls, messageStructDecls, requestDecl, 
        allocationDecl, actorDecl, msgDecl, forwardDecls );

    // The third pass forward declares operator '|' send routines
    Pass<GenFwdDecls>::run( translationUnit, actorStructDecls, messageStructDecls, forwardDecls );
}


} // namespace Concurrency

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

