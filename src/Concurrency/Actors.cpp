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
#include <algorithm>
using namespace ast;
using namespace std;

namespace Concurrency {

struct CollectactorStructDecls : public ast::WithGuards {
    unordered_set<const StructDecl *> & actorStructDecls;
    unordered_set<const StructDecl *>  & messageStructDecls;
    const StructDecl ** requestDecl;
    const EnumDecl ** allocationDecl;
    const StructDecl ** actorDecl;
    const StructDecl ** msgDecl;
    StructDecl * parentDecl;
    bool insideStruct = false;
    bool namedDecl = false;

    // finds and sets a ptr to the allocation enum, which is needed in the next pass
    void previsit( const EnumDecl * decl ) {
        if( decl->name == "allocation" ) *allocationDecl = decl;
    }

    // finds and sets a ptr to the actor, message, and request structs, which are needed in the next pass
    void previsit( const StructDecl * decl ) {
        if ( !decl->body ) return;
        if ( decl->name == "actor" ) {
            actorStructDecls.insert( decl ); // skip inserting fwd decl
            *actorDecl = decl;
        } else if( decl->name == "message" ) {
            messageStructDecls.insert( decl ); // skip inserting fwd decl
            *msgDecl = decl;
        } else if( decl->name == "request" ) *requestDecl = decl;
        else {
            GuardValue(insideStruct);
            insideStruct = true;
            parentDecl = mutate( decl );
        }
	}

    // this catches structs of the form:
    //     struct dummy_actor { actor a; };
    // since they should be:
    //     struct dummy_actor { inline actor; };
    void previsit ( const ObjectDecl * decl ) {
        if ( insideStruct && ! decl->name.empty() ) {
            GuardValue(namedDecl);
            namedDecl = true;
        }
    }

    // this collects the derived actor and message struct decl ptrs
    void postvisit( const StructInstType * node ) {
        if ( ! *actorDecl || ! *msgDecl ) return;
        if ( insideStruct && !namedDecl ) {
            auto actorIter = actorStructDecls.find( node->aggr() );    
            if ( actorIter != actorStructDecls.end() ) {
                actorStructDecls.insert( parentDecl );
                return;
            }
            auto messageIter = messageStructDecls.find( node->aggr() );
            if ( messageIter != messageStructDecls.end() ) {
                messageStructDecls.insert( parentDecl );
            }
        }
	}

  public:
    CollectactorStructDecls( unordered_set<const StructDecl *> & actorStructDecls, unordered_set<const StructDecl *> & messageStructDecls,
        const StructDecl ** requestDecl, const EnumDecl ** allocationDecl, const StructDecl ** actorDecl, const StructDecl ** msgDecl ) 
        : actorStructDecls( actorStructDecls ), messageStructDecls( messageStructDecls ), requestDecl( requestDecl ), 
        allocationDecl( allocationDecl ), actorDecl(actorDecl), msgDecl(msgDecl) {}
};

// keeps track of all fwdDecls of message routines so that we can hoist them to right after the appropriate decls
class FwdDeclTable {

    // tracks which decls we have seen so that we can hoist the FunctionDecl to the highest point possible
    struct FwdDeclData { 
        const StructDecl * actorDecl;
        const StructDecl * msgDecl;
        FunctionDecl * fwdDecl;
        bool actorFound;
        bool msgFound;

        bool readyToInsert() { return actorFound && msgFound; }
        bool foundActor() { actorFound = true; return readyToInsert(); }
        bool foundMsg() { msgFound = true; return readyToInsert(); }

        FwdDeclData( const StructDecl * actorDecl, const StructDecl * msgDecl, FunctionDecl * fwdDecl ) :
            actorDecl(actorDecl), msgDecl(msgDecl), fwdDecl(fwdDecl), actorFound(false), msgFound(false) {}
    };

    // map indexed by actor struct ptr
    // value is map of all FwdDeclData that contains said actor struct ptr
    // inner map is indexed by the message struct ptr of FwdDeclData
    unordered_map<const StructDecl *, unordered_map<const StructDecl *, FwdDeclData *>> actorMap;

    // this map is the same except the outer map is indexed by message ptr and the inner is indexed by actor ptr
    unordered_map<const StructDecl *, unordered_map<const StructDecl *, FwdDeclData *>> msgMap;

    void insert( const StructDecl * decl, const StructDecl * otherDecl, unordered_map<const StructDecl *, unordered_map<const StructDecl *, FwdDeclData *>> & map, FwdDeclData * data ) {
        auto iter = map.find( decl );
        if ( iter != map.end() ) { // if decl exists in map append data to existing inner map
            iter->second.emplace( make_pair( otherDecl, data ) );
        } else { // else create inner map for key
            map.emplace( make_pair( decl, unordered_map<const StructDecl *, FwdDeclData *>( { make_pair( otherDecl, data ) } ) ) );
        }
    }

  public:
    // insert decl into table so that we can fwd declare it later (average cost: O(1))
    void insertDecl( const StructDecl * actorDecl, const StructDecl * msgDecl, FunctionDecl * fwdDecl ) {
        FwdDeclData * declToInsert = new FwdDeclData( actorDecl, msgDecl, fwdDecl );
        insert( actorDecl, msgDecl, actorMap, declToInsert );
        insert( msgDecl, actorDecl, msgMap, declToInsert );
    }

    // returns list of decls to insert after current struct decl
    // Over the entire pass the runtime of this routine is O(r) where r is the # of receive routines
    list<FunctionDecl *> updateDecl( const StructDecl * decl, bool isMsg ) {
        unordered_map<const StructDecl *, unordered_map<const StructDecl *, FwdDeclData *>> & map = isMsg ? msgMap : actorMap;
        unordered_map<const StructDecl *, unordered_map<const StructDecl *, FwdDeclData *>> & otherMap =  isMsg ? actorMap : msgMap;
        auto iter = map.find( decl );
        list<FunctionDecl *> toInsertAfter; // this is populated with decls that are ready to insert
        if ( iter == map.end() ) return toInsertAfter;
        
        // iterate over inner map
        unordered_map<const StructDecl *, FwdDeclData *> & currInnerMap = iter->second;
        for ( auto innerIter = currInnerMap.begin(); innerIter != currInnerMap.end(); ) {
            FwdDeclData * currentDatum = innerIter->second;
            bool readyToInsert = isMsg ? currentDatum->foundMsg() : currentDatum->foundActor();
            if ( ! readyToInsert ) { ++innerIter; continue; }
            
            // readyToInsert is true so we are good to insert the forward decl of the message fn
            toInsertAfter.push_back( currentDatum->fwdDecl );

            // need to remove from other map before deleting
            // find inner map in other map ( other map is actor map if original is msg map and vice versa )
            const StructDecl * otherDecl = isMsg ? currentDatum->actorDecl : currentDatum->msgDecl;
            auto otherMapIter = otherMap.find( otherDecl );

            unordered_map<const StructDecl *, FwdDeclData *> & otherInnerMap = otherMapIter->second;

            // find the FwdDeclData we need to remove in the other inner map
            auto otherInnerIter = otherInnerMap.find( decl );

            // remove references to deleted FwdDeclData from current inner map
            innerIter = currInnerMap.erase( innerIter ); // this does the increment so no explicit inc needed

            // remove references to deleted FwdDeclData from other inner map
            otherInnerMap.erase( otherInnerIter );
            
            // if other inner map is now empty, remove key from other outer map
            if ( otherInnerMap.empty() )
                otherMap.erase( otherDecl );

            // now we are safe to delete the FwdDeclData since we are done with it
            // and we have removed all references to it from our data structures
            delete currentDatum;
        }

        // if current inner map is now empty, remove key from outer map.
        // Have to do this after iterating for safety
        if ( currInnerMap.empty() )
            map.erase( decl );

        return toInsertAfter;
    }
};

// generates the definitions of send operators for actors
// collects data needed for next pass that does the circular defn resolution 
//     for message send operators (via table above)
struct GenFuncsCreateTables : public ast::WithDeclsToAdd<> {
    unordered_set<const StructDecl *> & actorStructDecls;
    unordered_set<const StructDecl *>  & messageStructDecls;
    const StructDecl ** requestDecl;
    const EnumDecl ** allocationDecl;
    const StructDecl ** actorDecl;
    const StructDecl ** msgDecl;
    FwdDeclTable & forwardDecls;

    // generates the operator for actor message sends
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
        auto actorIter = actorStructDecls.find( arg1InstType->aggr() );
        auto messageIter = messageStructDecls.find( arg2InstType->aggr() );
        if ( actorIter != actorStructDecls.end() && messageIter != messageStructDecls.end() ) {
            //////////////////////////////////////////////////////////////////////
            // The following generates this send message operator routine for all receive(derived_actor &, derived_msg &) functions
            /*
                static inline derived_actor & ?|?( derived_actor & receiver, derived_msg & msg ) {
                    request new_req;
                    allocation (*my_work_fn)( derived_actor &, derived_msg & ) = receive;
                    __receive_fn fn = (__receive_fn)my_work_fn;
                    new_req{ &receiver, &msg, fn };
                    send( receiver, new_req );
                    return receiver;
                }
            */ 
            CompoundStmt * sendBody = new CompoundStmt( decl->location );

            // Generates: request new_req;
            sendBody->push_back( new DeclStmt(
                decl->location,
                new ObjectDecl(
                    decl->location,
                    "new_req",
                    new StructInstType( *requestDecl )
                )
            ));
            
            // Function type is: allocation (*)( derived_actor &, derived_msg & )
            FunctionType * derivedReceive = new FunctionType();
            derivedReceive->params.push_back( ast::deepCopy( derivedActorRef ) );
            derivedReceive->params.push_back( ast::deepCopy( derivedMsgRef ) );
            derivedReceive->returns.push_back( new EnumInstType( *allocationDecl ) );

            // Generates: allocation (*my_work_fn)( derived_actor &, derived_msg & ) = receive;
            sendBody->push_back( new DeclStmt(
                decl->location,
                new ObjectDecl(
                    decl->location,
                    "my_work_fn",
                    new PointerType( derivedReceive ),
                    new SingleInit( decl->location, new NameExpr( decl->location, "receive" ) )
                )
            ));

            // Function type is: allocation (*)( actor &, message & )
            FunctionType * genericReceive = new FunctionType();
            genericReceive->params.push_back( new ReferenceType( new StructInstType( *actorDecl ) ) );
            genericReceive->params.push_back( new ReferenceType( new StructInstType( *msgDecl ) ) );
            genericReceive->returns.push_back( new EnumInstType( *allocationDecl ) );

            // Generates: allocation (*fn)( actor &, message & ) = (allocation (*)( actor &, message & ))my_work_fn;
            // More readable synonymous code: 
            //     typedef allocation (*__receive_fn)(actor &, message &);
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

            // Generates: new_req{ &receiver, (actor *)&receiver, &msg, (message *)&msg, fn };
            sendBody->push_back( new ExprStmt(
                decl->location,
				new UntypedExpr (
                    decl->location, 
					new NameExpr( decl->location, "?{}" ),
					{
						new NameExpr( decl->location, "new_req" ),
                        new AddressExpr( new NameExpr( decl->location, "receiver" ) ),
                        new CastExpr( decl->location, new AddressExpr( new NameExpr( decl->location, "receiver" ) ), new PointerType( new StructInstType( *actorDecl ) ), ExplicitCast ),
                        new AddressExpr( new NameExpr( decl->location, "msg" ) ),
                        new CastExpr( decl->location, new AddressExpr( new NameExpr( decl->location, "msg" ) ), new PointerType( new StructInstType( *msgDecl ) ), ExplicitCast ),
                        new NameExpr( decl->location, "fn" )
					}
				)
			));

            // Generates: send( receiver, new_req );
            sendBody->push_back( new ExprStmt(
                decl->location,
				new UntypedExpr (
                    decl->location,
					new NameExpr( decl->location, "send" ),
					{
						{
                            new NameExpr( decl->location, "receiver" ),
                            new NameExpr( decl->location, "new_req" )
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
            forwardDecls.insertDecl( *actorIter, *messageIter , ast::deepCopy( sendOperatorFunction ) );

            sendOperatorFunction->stmts = sendBody;
            declsToAddAfter.push_back( sendOperatorFunction );
        }
	}

  public:
    GenFuncsCreateTables( unordered_set<const StructDecl *> & actorStructDecls, unordered_set<const StructDecl *> & messageStructDecls,
        const StructDecl ** requestDecl, const EnumDecl ** allocationDecl, const StructDecl ** actorDecl, const StructDecl ** msgDecl, 
        FwdDeclTable & forwardDecls ) : actorStructDecls(actorStructDecls), messageStructDecls(messageStructDecls), 
        requestDecl(requestDecl), allocationDecl(allocationDecl), actorDecl(actorDecl), msgDecl(msgDecl), forwardDecls(forwardDecls) {}
};


// separate pass is needed since this pass resolves circular defn issues
// generates the forward declarations of the send operator for actor routines
struct FwdDeclOperator : public ast::WithDeclsToAdd<> {
    unordered_set<const StructDecl *> & actorStructDecls;
    unordered_set<const StructDecl *>  & messageStructDecls;
    FwdDeclTable & forwardDecls;

    // handles forward declaring the message operator
    void postvisit( const StructDecl * decl ) {
        list<FunctionDecl *> toAddAfter;
        auto actorIter = actorStructDecls.find( decl );
        if ( actorIter != actorStructDecls.end() ) { // this is a derived actor decl
            // get list of fwd decls that we can now insert
            toAddAfter = forwardDecls.updateDecl( decl, false );

            // get rid of decl from actorStructDecls since we no longer need it
            actorStructDecls.erase( actorIter );
        } else {
            auto messageIter = messageStructDecls.find( decl );
            if ( messageIter == messageStructDecls.end() ) return;

            toAddAfter = forwardDecls.updateDecl( decl, true );

            // get rid of decl from messageStructDecls since we no longer need it
            messageStructDecls.erase( messageIter );
        }

        // add the fwd decls to declsToAddAfter
        for ( FunctionDecl * func : toAddAfter ) {
            declsToAddAfter.push_back( func );
        }
    }

  public:
    FwdDeclOperator( unordered_set<const StructDecl *> & actorStructDecls, unordered_set<const StructDecl *> & messageStructDecls, 
        FwdDeclTable & forwardDecls ) : actorStructDecls(actorStructDecls), messageStructDecls(messageStructDecls), forwardDecls(forwardDecls) {}
};

void implementActors( TranslationUnit & translationUnit ) {
    // unordered_maps to collect all derived actor and message types
    unordered_set<const StructDecl *> actorStructDecls;
    unordered_set<const StructDecl *> messageStructDecls;
    FwdDeclTable forwardDecls;

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

    // first pass collects ptrs to allocation enum, request type, and generic receive fn typedef
    // also populates maps of all derived actors and messages
    Pass<CollectactorStructDecls>::run( translationUnit, actorStructDecls, messageStructDecls, requestDecl, 
        allocationDecl, actorDecl, msgDecl );

    // check that we have found all the decls we need from <actor.hfa>, if not no need to run the rest of this pass
    if ( !allocationDeclPtr || !requestDeclPtr || !actorDeclPtr || !msgDeclPtr ) 
        return;

    // second pass locates all receive() routines that overload the generic receive fn
    // it then generates the appropriate operator '|' send routines for the receive routines
    Pass<GenFuncsCreateTables>::run( translationUnit, actorStructDecls, messageStructDecls, requestDecl, 
        allocationDecl, actorDecl, msgDecl, forwardDecls );

    // The third pass forward declares operator '|' send routines
    Pass<FwdDeclOperator>::run( translationUnit, actorStructDecls, messageStructDecls, forwardDecls );
}


} // namespace Concurrency

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

