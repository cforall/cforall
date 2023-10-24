//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Corun.cpp -- generate code needed by the actor system
//
// Author           : Colby Parsons
// Created On       : Monday October 9 15:16:42 2023
// Last Modified By : Colby Parsons
// Last Modified On : Monday October 9 15:16:42 2023
// Update Count     : 0
//

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "Common/UniqueName.h"
using namespace ast;
using namespace std;

namespace Concurrency {

struct CorunKeyword : public WithDeclsToAdd<>, public WithStmtsToAdd<> {
    UniqueName CorunFnNamer = "__CFA_corun_lambda_"s;
    UniqueName RunnerBlockNamer = "__CFA_corun_block_"s;

    const StructDecl * runnerBlockDecl = nullptr;

    // Finds select_node decl
    void previsit( const StructDecl * decl ) {
        if ( !decl->body ) {
            return;
        } else if ( "runner_block" == decl->name ) {
            assert( !runnerBlockDecl );
            runnerBlockDecl = decl;
        }
    }

    Stmt * postvisit( const CorunStmt * stmt ) {
        if ( !runnerBlockDecl )
            SemanticError( stmt->location, "To use corun statements add #include <cofor.hfa>\n" );

        if ( !stmt->stmt )
            return nullptr;

        const CodeLocation & loc = stmt->location;
        const string fnName = CorunFnNamer.newName();
        const string objName = RunnerBlockNamer.newName();

        // Generates:
        // void __CFA_corun_lambda_() { ... stmt->stmt ... }
        Stmt * runnerLambda = new DeclStmt( loc,
            new FunctionDecl( loc,
                fnName,                                             // name
                {},                                                 // forall
                {},                                                 // params
                {},                                                 // return
                new CompoundStmt( loc, { deepCopy(stmt->stmt) } )   // body
            )
        );

        // Generates:
        // runner_block __CFA_corun_block_;
        Stmt * objDecl = new DeclStmt( loc,
            new ObjectDecl( loc,
                objName,
                new StructInstType( runnerBlockDecl )
            )
        );

        // Generates:
        // __CFA_corun_block_{ __CFA_corun_lambda_ };
        Stmt * threadStart = new ExprStmt( loc,
            new UntypedExpr ( loc,
                new NameExpr( loc, "?{}" ),
                {
                    new NameExpr( loc, objName ),
                    new NameExpr( loc, fnName )
                }
            )
        );

        stmtsToAddBefore.push_back( runnerLambda );
        stmtsToAddBefore.push_back( objDecl );

        return threadStart;
    }
};

void implementCorun( TranslationUnit & translationUnit ) {
    Pass<CorunKeyword>::run( translationUnit );
}

} // namespace Concurrency
