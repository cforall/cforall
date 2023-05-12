//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// WaitforNew.cpp -- Expand waitfor clauses into code.
//
// Author           : Andrew Beach
// Created On       : Fri May 27 10:31:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 13 13:30:00 2022
// Update Count     : 0
//

#include <string>

#include "Waituntil.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "Common/UniqueName.h"

using namespace ast;
using namespace std;

/* So this is what this pass dones:
{
    when ( condA ) waituntil( A ){ doA(); } 
    or when ( condB ) waituntil( B ){ doB(); } 
    and when ( condC ) waituntil( C ) { doC(); }
}
		 ||
		 ||
		\||/
		 \/

Generates these two routines:
static inline bool is_full_sat_1( int * clause_statuses ) {
    return clause_statuses[0] 
        || clause_statuses[1]
        && clause_statuses[2];
}

static inline bool is_done_sat_1( int * clause_statuses ) {
    return has_run(clause_statuses[0])
        || has_run(clause_statuses[1])
        && has_run(clause_statuses[2]);
}

Replaces the waituntil statement above with the following code:
{
    // used with atomic_dec/inc to get binary semaphore behaviour
    int park_counter = 0;

    // status (one for each clause)
    int clause_statuses[3] = { 0 };

    bool whenA = condA;
    bool whenB = condB;
    bool whenC = condC;

    if ( !whenB ) clause_statuses[1] = __SELECT_RUN;
    if ( !whenC ) clause_statuses[2] = __SELECT_RUN;

    // some other conditional settors for clause_statuses are set here, see genSubtreeAssign and related routines

    // three blocks
    // for each block, create, setup, then register select_node
    select_node clause1;
    select_node clause2;
    select_node clause3;

    try {
        if ( whenA ) { register_select(A, clause1); setup_clause( clause1, &clause_statuses[0], &park_counter ); }
        ... repeat ^ for B and C ... 

        // if else clause is defined a separate branch can occur here to set initial values, see genWhenStateConditions

        // loop & park until done
        while( !is_full_sat_1( clause_statuses ) ) {
            
            // binary sem P();
            if ( __atomic_sub_fetch( &park_counter, 1, __ATOMIC_SEQ_CST) < 0 )
                park();
            
            // execute any blocks available with status set to 0
            for ( int i = 0; i < 3; i++ ) {
                if (clause_statuses[i] == __SELECT_SAT) {
                    switch (i) {
                        case 0:
                            try {
                                if (on_selected( A, clause1 ))
                                    doA();
                            }
                            finally { clause_statuses[i] = __SELECT_RUN; unregister_select(A, clause1); }
                            break;
                        case 1:
                            ... same gen as A but for B and clause2 ...
                            break;
                        case 2:
                            ... same gen as A but for C and clause3 ...
                            break;
                    }
                }
            }
        }

        // ensure that the blocks that triggered is_full_sat_1 are run
        // by running every un-run block that is SAT from the start until
        // the predicate is SAT when considering RUN status = true
        for ( int i = 0; i < 3; i++ ) {
            if (is_done_sat_1( clause_statuses )) break;
            if (clause_statuses[i] == __SELECT_SAT)
                ... Same if body here as in loop above ...
        }
    } finally {
        // the unregister and on_selected calls are needed to support primitives where the acquire has side effects
        // so the corresponding block MUST be run for those primitives to not lose state (example is channels)
        if ( ! has_run(clause_statuses[0]) && whenA && unregister_select(A, clause1) && on_selected( A, clause1 ) )
            doA(); 
        ... repeat if above for B and C ...
    }
}

*/

namespace Concurrency {

class GenerateWaitUntilCore final {
    vector<FunctionDecl *> & satFns;
	UniqueName namer_sat = "__is_full_sat_"s;
    UniqueName namer_run = "__is_run_sat_"s;
	UniqueName namer_park = "__park_counter_"s;
	UniqueName namer_status = "__clause_statuses_"s;
	UniqueName namer_node = "__clause_"s;
    UniqueName namer_target = "__clause_target_"s;
    UniqueName namer_when = "__when_cond_"s;

    string idxName = "__CFA_clause_idx_";

    struct ClauseData {
        string nodeName;
        string targetName;
        string whenName;
        int index;
        string & statusName;
        ClauseData( int index, string & statusName ) : index(index), statusName(statusName) {}
    };

    const StructDecl * selectNodeDecl = nullptr;

    // This first set of routines are all used to do the complicated job of 
    //    dealing with how to set predicate statuses with certain when_conds T/F
    //    so that the when_cond == F effectively makes that clause "disappear"
    void updateAmbiguousWhen( WaitUntilStmt::ClauseNode * currNode, bool andAbove, bool orAbove, bool andBelow, bool orBelow );
    void paintWhenTree( WaitUntilStmt::ClauseNode * currNode, bool andAbove, bool orAbove, bool & andBelow, bool & orBelow );
    bool paintWhenTree( WaitUntilStmt::ClauseNode * currNode );
    void collectWhens( WaitUntilStmt::ClauseNode * currNode, vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigIdxs, vector<int> & andIdxs, int & index, bool parentAmbig, bool parentAnd );
    void collectWhens( WaitUntilStmt::ClauseNode * currNode, vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigIdxs, vector<int> & andIdxs );
    void updateWhenState( WaitUntilStmt::ClauseNode * currNode );
    void genSubtreeAssign( const WaitUntilStmt * stmt, WaitUntilStmt::ClauseNode * currNode, bool status, int & idx, CompoundStmt * retStmt, vector<ClauseData *> & clauseData );
    void genStatusAssign( const WaitUntilStmt * stmt, WaitUntilStmt::ClauseNode * currNode, int & idx, CompoundStmt * retStmt, vector<ClauseData *> & clauseData );
    CompoundStmt * getStatusAssignment( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData );
    Stmt * genWhenStateConditions( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigClauses, vector<pair<int, WaitUntilStmt::ClauseNode *>>::size_type ambigIdx );

    // These routines are just code-gen helpers
    void addPredicates( const WaitUntilStmt * stmt, string & satName, string & runName );
    void setUpClause( const WhenClause * clause, ClauseData * data, string & pCountName, CompoundStmt * body );
    ForStmt * genStatusCheckFor( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, string & predName );
    Expr * genSelectTraitCall( const WhenClause * clause, const ClauseData * data, string fnName );
    CompoundStmt * genStmtBlock( const WhenClause * clause, const ClauseData * data );
    Stmt * genElseClauseBranch( const WaitUntilStmt * stmt, string & runName, string & arrName, vector<ClauseData *> & clauseData );
    Stmt * genNoElseClauseBranch( const WaitUntilStmt * stmt, string & satName, string & runName, string & arrName, string & pCountName, vector<ClauseData *> & clauseData );
    void genClauseInits( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, CompoundStmt * body, string & statusName, string & elseWhenName );
    Stmt * recursiveOrIfGen( const WaitUntilStmt * stmt, vector<ClauseData *> & data, vector<ClauseData*>::size_type idx, string & elseWhenName );
    Stmt * buildOrCaseSwitch( const WaitUntilStmt * stmt, string & statusName, vector<ClauseData *> & data );
    Stmt * genAllOr( const WaitUntilStmt * stmt );

  public:
    void previsit( const StructDecl * decl );
	Stmt * postvisit( const WaitUntilStmt * stmt );
    GenerateWaitUntilCore( vector<FunctionDecl *> & satFns ): satFns(satFns) {}
};

// Finds select_node decl
void GenerateWaitUntilCore::previsit( const StructDecl * decl ) {
    if ( !decl->body ) {
		return;
	} else if ( "select_node" == decl->name ) {
		assert( !selectNodeDecl );
		selectNodeDecl = decl;
	}
}

void GenerateWaitUntilCore::updateAmbiguousWhen( WaitUntilStmt::ClauseNode * currNode, bool andAbove, bool orAbove, bool andBelow, bool orBelow ) {
    // all children when-ambiguous
    if ( currNode->left->ambiguousWhen && currNode->right->ambiguousWhen )
        // true iff an ancestor/descendant has a different operation
        currNode->ambiguousWhen = (orAbove || orBelow) && (andBelow || andAbove);
    // ambiguousWhen is initially false so theres no need to set it here
}

// Traverses ClauseNode tree and paints each AND/OR node as when-ambiguous true or false
// This tree painting is needed to generate the if statements that set the initial state
//    of the clause statuses when some clauses are turned off via when_cond
// An internal AND/OR node is when-ambiguous if it satisfies all of the following:
// - It has an ancestor or descendant that is a different operation, i.e. (AND has an OR ancestor or vice versa)
// - All of its descendent clauses are optional, i.e. they have a when_cond defined on the WhenClause
void GenerateWaitUntilCore::paintWhenTree( WaitUntilStmt::ClauseNode * currNode, bool andAbove, bool orAbove, bool & andBelow, bool & orBelow ) {
    bool aBelow = false; // updated by child nodes
    bool oBelow = false; // updated by child nodes
    switch (currNode->op) {
        case WaitUntilStmt::ClauseNode::AND:
            paintWhenTree( currNode->left, true, orAbove, aBelow, oBelow );
            paintWhenTree( currNode->right, true, orAbove, aBelow, oBelow );

            // update currNode's when flag based on conditions listed in fn signature comment above
            updateAmbiguousWhen(currNode, true, orAbove, aBelow, oBelow );

            // set return flags to tell parents which decendant ops have been seen
            andBelow = true;
            orBelow = oBelow;
            return;
        case WaitUntilStmt::ClauseNode::OR:
            paintWhenTree( currNode->left, andAbove, true, aBelow, oBelow );
            paintWhenTree( currNode->right, andAbove, true, aBelow, oBelow );

            // update currNode's when flag based on conditions listed in fn signature comment above
            updateAmbiguousWhen(currNode, andAbove, true, aBelow, oBelow );

            // set return flags to tell parents which decendant ops have been seen
            andBelow = aBelow;
            orBelow = true;
            return;
        case WaitUntilStmt::ClauseNode::LEAF:
            if ( currNode->leaf->when_cond )
                currNode->ambiguousWhen = true;
            return;
        default:
            assertf(false, "Unreachable waituntil clause node type. How did you get here???");
    }
}

// overloaded wrapper for paintWhenTree that sets initial values
// returns true if entire tree is OR's (special case)
bool GenerateWaitUntilCore::paintWhenTree( WaitUntilStmt::ClauseNode * currNode ) {
    bool aBelow = false, oBelow = false; // unused by initial call
    paintWhenTree( currNode, false, false, aBelow, oBelow );
    return !aBelow;
}

// Helper: returns Expr that represents arrName[index]
Expr * genArrAccessExpr( const CodeLocation & loc, int index, string arrName ) {
    return new UntypedExpr ( loc, 
        new NameExpr( loc, "?[?]" ),
        {
            new NameExpr( loc, arrName ),
            ConstantExpr::from_int( loc, index )
        }
    );
}

// After the ClauseNode AND/OR nodes are painted this routine is called to traverses the tree and does the following:
// - collects a set of indices in the clause arr that refer whenclauses that can have ambiguous status assignments (ambigIdxs)
// - collects a set of indices in the clause arr that refer whenclauses that have a when() defined and an AND node as a parent (andIdxs)
// - updates LEAF nodes to be when-ambiguous if their direct parent is when-ambiguous.
void GenerateWaitUntilCore::collectWhens( WaitUntilStmt::ClauseNode * currNode, vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigIdxs, vector<int> & andIdxs, int & index, bool parentAmbig, bool parentAnd ) {
    switch (currNode->op) {
        case WaitUntilStmt::ClauseNode::AND:
            collectWhens( currNode->left, ambigIdxs, andIdxs, index, currNode->ambiguousWhen, true );
            collectWhens( currNode->right,  ambigIdxs, andIdxs, index, currNode->ambiguousWhen, true );
            return;
        case WaitUntilStmt::ClauseNode::OR:
            collectWhens( currNode->left,  ambigIdxs, andIdxs, index, currNode->ambiguousWhen, false );
            collectWhens( currNode->right,  ambigIdxs, andIdxs, index, currNode->ambiguousWhen, false );
            return;
        case WaitUntilStmt::ClauseNode::LEAF:
            if ( parentAmbig ) {
                ambigIdxs.push_back(make_pair(index, currNode));
            }
            if ( parentAnd && currNode->leaf->when_cond ) {
                currNode->childOfAnd = true;
                andIdxs.push_back(index);
            }
            index++;
            return;
        default:
            assertf(false, "Unreachable waituntil clause node type. How did you get here???");
    }
}

// overloaded wrapper for collectWhens that sets initial values
void GenerateWaitUntilCore::collectWhens( WaitUntilStmt::ClauseNode * currNode, vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigIdxs, vector<int> & andIdxs ) {
    int idx = 0;
    collectWhens( currNode, ambigIdxs, andIdxs, idx, false, false );
}

// recursively updates ClauseNode whenState on internal nodes so that next pass can see which 
//    subtrees are "turned off"
// sets whenState = false iff both children have whenState == false.
// similar to paintWhenTree except since paintWhenTree also filtered out clauses we don't need to consider based on op
// since the ambiguous clauses were filtered in paintWhenTree we don't need to worry about that here
void GenerateWaitUntilCore::updateWhenState( WaitUntilStmt::ClauseNode * currNode ) {
    if ( currNode->op == WaitUntilStmt::ClauseNode::LEAF ) return;
    updateWhenState( currNode->left );
    updateWhenState( currNode->right );
    if ( !currNode->left->whenState && !currNode->right->whenState )
        currNode->whenState = false;
    else 
        currNode->whenState = true;
}

// generates the minimal set of status assignments to ensure predicate subtree passed as currNode evaluates to status
// assumes that this will only be called on subtrees that are entirely whenState == false
void GenerateWaitUntilCore::genSubtreeAssign( const WaitUntilStmt * stmt, WaitUntilStmt::ClauseNode * currNode, bool status, int & idx, CompoundStmt * retStmt, vector<ClauseData *> & clauseData ) {
    if ( ( currNode->op == WaitUntilStmt::ClauseNode::AND && status )
        || ( currNode->op == WaitUntilStmt::ClauseNode::OR && !status ) ) {
        // need to recurse on both subtrees if && subtree needs to be true or || subtree needs to be false
        genSubtreeAssign( stmt, currNode->left, status, idx, retStmt, clauseData );
        genSubtreeAssign( stmt, currNode->right, status, idx, retStmt, clauseData );
    } else if ( ( currNode->op == WaitUntilStmt::ClauseNode::OR && status )
        || ( currNode->op == WaitUntilStmt::ClauseNode::AND && !status ) ) {
        // only one subtree needs to evaluate to status if && subtree needs to be true or || subtree needs to be false
        CompoundStmt * leftStmt = new CompoundStmt( stmt->location );
        CompoundStmt * rightStmt = new CompoundStmt( stmt->location );

        // only one side needs to evaluate to status so we recurse on both subtrees
        //    but only keep the statements from the subtree with minimal statements
        genSubtreeAssign( stmt, currNode->left, status, idx, leftStmt, clauseData );
        genSubtreeAssign( stmt, currNode->right, status, idx, rightStmt, clauseData );
        
        // append minimal statements to retStmt
        if ( leftStmt->kids.size() < rightStmt->kids.size() ) {
            retStmt->kids.splice( retStmt->kids.end(), leftStmt->kids );
        } else {
            retStmt->kids.splice( retStmt->kids.end(), rightStmt->kids );
        }
        
        delete leftStmt;
        delete rightStmt;
    } else if ( currNode->op == WaitUntilStmt::ClauseNode::LEAF ) {
        const CodeLocation & loc = stmt->location;
        if ( status && !currNode->childOfAnd ) {
            retStmt->push_back(
                new ExprStmt( loc, 
                    UntypedExpr::createAssign( loc,
                        genArrAccessExpr( loc, idx, clauseData.at(idx)->statusName ),
                        new NameExpr( loc, "__SELECT_RUN" )
                    )
                )
            );
        } else if ( !status && currNode->childOfAnd ) {
            retStmt->push_back(
                new ExprStmt( loc, 
                    UntypedExpr::createAssign( loc,
                        genArrAccessExpr( loc, idx, clauseData.at(idx)->statusName ),
                        new NameExpr( loc, "__SELECT_UNSAT" )
                    )
                )
            );
        }

        // No need to generate statements for the following cases since childOfAnd are always set to true
        //    and !childOfAnd are always false
        // - status && currNode->childOfAnd
        // - !status && !currNode->childOfAnd
        idx++;
    }
}

void GenerateWaitUntilCore::genStatusAssign( const WaitUntilStmt * stmt, WaitUntilStmt::ClauseNode * currNode, int & idx, CompoundStmt * retStmt, vector<ClauseData *> & clauseData ) {
    switch (currNode->op) {
        case WaitUntilStmt::ClauseNode::AND:
            // check which subtrees have all whenState == false (disabled)
            if (!currNode->left->whenState && !currNode->right->whenState) {
                // this case can only occur when whole tree is disabled since otherwise 
                //    genStatusAssign( ... ) isn't called on nodes with whenState == false
                assert( !currNode->whenState ); // paranoidWWW
                // whole tree disabled so pass true so that select is SAT vacuously
                genSubtreeAssign( stmt, currNode, true, idx, retStmt, clauseData );
            } else if ( !currNode->left->whenState ) {
                // pass true since x && true === x
                genSubtreeAssign( stmt, currNode->left, true, idx, retStmt, clauseData );
                genStatusAssign( stmt, currNode->right, idx, retStmt, clauseData );
            } else if ( !currNode->right->whenState ) {
                genStatusAssign( stmt, currNode->left, idx, retStmt, clauseData );
                genSubtreeAssign( stmt, currNode->right, true, idx, retStmt, clauseData );
            } else { 
                // if no children with whenState == false recurse normally via break
                break;
            }
            return;
        case WaitUntilStmt::ClauseNode::OR:
            if (!currNode->left->whenState && !currNode->right->whenState) {
                assert( !currNode->whenState ); // paranoid
                genSubtreeAssign( stmt, currNode, true, idx, retStmt, clauseData );
            } else if ( !currNode->left->whenState ) {
                // pass false since x || false === x
                genSubtreeAssign( stmt, currNode->left, false, idx, retStmt, clauseData );
                genStatusAssign( stmt, currNode->right, idx, retStmt, clauseData );
            } else if ( !currNode->right->whenState ) {
                genStatusAssign( stmt, currNode->left, idx, retStmt, clauseData );
                genSubtreeAssign( stmt, currNode->right, false, idx, retStmt, clauseData );
            } else { 
                break;
            }
            return;
        case WaitUntilStmt::ClauseNode::LEAF:
            idx++;
            return;
        default:
            assertf(false, "Unreachable waituntil clause node type. How did you get here???");
    }
    genStatusAssign( stmt, currNode->left, idx, retStmt, clauseData );
    genStatusAssign( stmt, currNode->right, idx, retStmt, clauseData );
}

// generates a minimal set of assignments for status arr based on which whens are toggled on/off
CompoundStmt * GenerateWaitUntilCore::getStatusAssignment( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData ) {
    updateWhenState( stmt->predicateTree );
    CompoundStmt * retval = new CompoundStmt( stmt->location );
    int idx = 0;
    genStatusAssign( stmt, stmt->predicateTree, idx, retval, clauseData );
    return retval;
}

// generates nested if/elses for all possible assignments of ambiguous when_conds
// exponential size of code gen but linear runtime O(n), where n is number of ambiguous whens()
Stmt * GenerateWaitUntilCore::genWhenStateConditions( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, 
    vector<pair<int, WaitUntilStmt::ClauseNode *>> & ambigClauses, vector<pair<int, WaitUntilStmt::ClauseNode *>>::size_type ambigIdx ) {
    // I hate C++ sometimes, using vector<pair<int, WaitUntilStmt::ClauseNode *>>::size_type for size() comparison seems silly.
    //    Why is size_type parameterized on the type stored in the vector?????

    const CodeLocation & loc = stmt->location;
    int clauseIdx = ambigClauses.at(ambigIdx).first;
    WaitUntilStmt::ClauseNode * currNode = ambigClauses.at(ambigIdx).second;
    Stmt * thenStmt;
    Stmt * elseStmt;
    
    if ( ambigIdx == ambigClauses.size() - 1 ) { // base case
        currNode->whenState = true;
        thenStmt = getStatusAssignment( stmt, clauseData );
        currNode->whenState = false;
        elseStmt = getStatusAssignment( stmt, clauseData );
    } else {
        // recurse both with when enabled and disabled to generate all possible cases
        currNode->whenState = true;
        thenStmt = genWhenStateConditions( stmt, clauseData, ambigClauses, ambigIdx + 1 );
        currNode->whenState = false;
        elseStmt = genWhenStateConditions( stmt, clauseData, ambigClauses, ambigIdx + 1 );
    }

    // insert first recursion result in if ( __when_cond_ ) { ... }
    // insert second recursion result in else { ... }
    return new CompoundStmt ( loc,
        {
            new IfStmt( loc,
                new NameExpr( loc, clauseData.at(clauseIdx)->whenName ),
                thenStmt,
                elseStmt
            )
        }
    );
}

// typedef a fn ptr so that we can reuse genPredExpr
// genLeafExpr is used to refer to one of the following two routines
typedef Expr * (*GenLeafExpr)( const CodeLocation & loc, int & index );

// return Expr that represents clause_statuses[index]
// mutates index to be index + 1
Expr * genSatExpr( const CodeLocation & loc, int & index ) {
    return genArrAccessExpr( loc, index++, "clause_statuses" );
}

// return Expr that represents has_run(clause_statuses[index])
Expr * genRunExpr( const CodeLocation & loc, int & index ) {
    return new UntypedExpr ( loc, 
        new NameExpr( loc, "__CFA_has_clause_run" ),
        { genSatExpr( loc, index ) }
    );
}

// Takes in the ClauseNode tree and recursively generates
// the predicate expr used inside the predicate functions
Expr * genPredExpr( const CodeLocation & loc, WaitUntilStmt::ClauseNode * currNode, int & idx, GenLeafExpr genLeaf ) {
    switch (currNode->op) {
        case WaitUntilStmt::ClauseNode::AND:
            return new LogicalExpr( loc, 
                new CastExpr( loc, genPredExpr( loc, currNode->left, idx, genLeaf ), new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast ),
                new CastExpr( loc, genPredExpr( loc, currNode->right, idx, genLeaf ), new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast ), 
                LogicalFlag::AndExpr 
            );
        case WaitUntilStmt::ClauseNode::OR:
            return new LogicalExpr( loc,
                new CastExpr( loc, genPredExpr( loc, currNode->left, idx, genLeaf ), new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast ),
                new CastExpr( loc, genPredExpr( loc, currNode->right, idx, genLeaf ), new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast ), 
                LogicalFlag::OrExpr );
        case WaitUntilStmt::ClauseNode::LEAF:
            return genLeaf( loc, idx );
        default:
            assertf(false, "Unreachable waituntil clause node type. How did you get here???");
    }
}


// Builds the predicate functions used to check the status of the waituntil statement
/* Ex:
{
    waituntil( A ){ doA(); } 
    or waituntil( B ){ doB(); } 
    and waituntil( C ) { doC(); }
}
generates =>
static inline bool is_full_sat_1( int * clause_statuses ) {
    return clause_statuses[0] 
        || clause_statuses[1]
        && clause_statuses[2];
}

static inline bool is_done_sat_1( int * clause_statuses ) {
    return has_run(clause_statuses[0])
        || has_run(clause_statuses[1])
        && has_run(clause_statuses[2]);
}
*/
// Returns a predicate function decl
// predName and genLeaf determine if this generates an is_done or an is_full predicate
FunctionDecl * buildPredicate( const WaitUntilStmt * stmt, GenLeafExpr genLeaf, string & predName ) {
    int arrIdx = 0;
    const CodeLocation & loc = stmt->location;
    CompoundStmt * body = new CompoundStmt( loc );
    body->push_back( new ReturnStmt( loc, genPredExpr( loc,  stmt->predicateTree, arrIdx, genLeaf ) ) );

    return new FunctionDecl( loc,
        predName,
        {},                     // forall
        {
            new ObjectDecl( loc,
                "clause_statuses",
                new PointerType( new BasicType( BasicType::Kind::LongUnsignedInt ) )
            )
        },
        { 
            new ObjectDecl( loc,
                "sat_ret",
                new BasicType( BasicType::Kind::Bool )
            )
        },
        body,               // body
        { Storage::Static },    // storage
        Linkage::Cforall,       // linkage
        {},                     // attributes
        { Function::Inline }
    );
}

// Creates is_done and is_full predicates
void GenerateWaitUntilCore::addPredicates( const WaitUntilStmt * stmt, string & satName, string & runName ) {
    if ( !stmt->else_stmt || stmt->else_cond ) // don't need SAT predicate when else variation with no else_cond
        satFns.push_back( Concurrency::buildPredicate( stmt, genSatExpr, satName ) ); 
    satFns.push_back( Concurrency::buildPredicate( stmt, genRunExpr, runName ) );
}

// Adds the following to body:
// if ( when_cond ) { // this if is omitted if no when() condition
//      setup_clause( clause1, &clause_statuses[0], &park_counter );
//      register_select(A, clause1);
// }
void GenerateWaitUntilCore::setUpClause( const WhenClause * clause, ClauseData * data, string & pCountName, CompoundStmt * body ) {    
    CompoundStmt * currBody = body;
    const CodeLocation & loc = clause->location;

    // If we have a when_cond make the initialization conditional
    if ( clause->when_cond )
        currBody = new CompoundStmt( loc );

    // Generates: setup_clause( clause1, &clause_statuses[0], &park_counter );
    currBody->push_back( new ExprStmt( loc,
        new UntypedExpr ( loc,
            new NameExpr( loc, "setup_clause" ),
            {
                new NameExpr( loc, data->nodeName ),
                new AddressExpr( loc, genArrAccessExpr( loc, data->index, data->statusName ) ),
                new AddressExpr( loc, new NameExpr( loc, pCountName ) )
            }
        )
    ));

    // Generates: register_select(A, clause1);
    currBody->push_back( new ExprStmt( loc, genSelectTraitCall( clause, data, "register_select" ) ) );

    // generates: if ( when_cond ) { ... currBody ... }
    if ( clause->when_cond )
        body->push_back( 
            new IfStmt( loc,
                new NameExpr( loc, data->whenName ),
                currBody
            )
        );
}

// Used to generate a call to one of the select trait routines
Expr * GenerateWaitUntilCore::genSelectTraitCall( const WhenClause * clause, const ClauseData * data, string fnName ) {
    const CodeLocation & loc = clause->location;
    return new UntypedExpr ( loc,
        new NameExpr( loc, fnName ),
        {
            new NameExpr( loc, data->targetName ),
            new NameExpr( loc, data->nodeName )
        }
    );
}

// Generates:
/* if ( on_selected( target_1, node_1 )) ... corresponding body of target_1 ... 
*/
CompoundStmt * GenerateWaitUntilCore::genStmtBlock( const WhenClause * clause, const ClauseData * data ) {
    const CodeLocation & cLoc = clause->location;
    return new CompoundStmt( cLoc,
        {
            new IfStmt( cLoc,
                genSelectTraitCall( clause, data, "on_selected" ),
                new CompoundStmt( cLoc,
                    {
                        ast::deepCopy( clause->stmt )
                    }
                )
            )
        }
    );
}

// this routine generates and returns the following
/*for ( int i = 0; i < numClauses; i++ ) {
    if ( predName(clause_statuses) ) break;
    if (clause_statuses[i] == __SELECT_SAT) {
        switch (i) {
            case 0:
                try {
                    if (on_selected( target1, clause1 ))
                        dotarget1stmt();
                }
                finally { clause_statuses[i] = __SELECT_RUN; unregister_select(target1, clause1); }
                break;
            ...
            case N:
                ...
                break;
        }
    }
}*/
ForStmt * GenerateWaitUntilCore::genStatusCheckFor( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, string & predName ) {
    CompoundStmt * ifBody = new CompoundStmt( stmt->location );
    const CodeLocation & loc = stmt->location;

    /* generates:
    switch (i) {
        case 0:
            try {
                if (on_selected( target1, clause1 ))
                    dotarget1stmt();
            }
            finally { clause_statuses[i] = __SELECT_RUN; unregister_select(target1, clause1); }
            break;
            ...
        case N:
            ...
            break;
    }*/
    std::vector<ptr<CaseClause>> switchCases;
    int idx = 0;
    for ( const auto & clause: stmt->clauses ) {
        const CodeLocation & cLoc = clause->location;
        switchCases.push_back(
            new CaseClause( cLoc,
                ConstantExpr::from_int( cLoc, idx ),
                {
                    new CompoundStmt( cLoc,
                        {
                            new ast::TryStmt( cLoc,
                                genStmtBlock( clause, clauseData.at(idx) ),
                                {},
                                new ast::FinallyClause( cLoc, 
                                    new CompoundStmt( cLoc,
                                        {
                                            new ExprStmt( loc,
                                                new UntypedExpr ( loc,
                                                    new NameExpr( loc, "?=?" ),
                                                    {
                                                        new UntypedExpr ( loc, 
                                                            new NameExpr( loc, "?[?]" ),
                                                            {
                                                                new NameExpr( loc, clauseData.at(0)->statusName ),
                                                                new NameExpr( loc, idxName )
                                                            }
                                                        ),
                                                        new NameExpr( loc, "__SELECT_RUN" )
                                                    }
                                                )
                                            ),
                                            new ExprStmt( loc, genSelectTraitCall( clause, clauseData.at(idx), "unregister_select" ) )
                                        }
                                    )
                                )
                            ),
                            new BranchStmt( cLoc, BranchStmt::Kind::Break, Label( cLoc ) )
                        }
                    )
                }
            )
        );
        idx++;
    }

    ifBody->push_back(
        new SwitchStmt( loc,
            new NameExpr( loc, idxName ),
            std::move( switchCases )
        )
    );

    // gens:
    // if (clause_statuses[i] == __SELECT_SAT) {
    //      ... ifBody  ...
    // }
    IfStmt * ifSwitch = new IfStmt( loc,
        new UntypedExpr ( loc,
            new NameExpr( loc, "?==?" ),
            {
                new UntypedExpr ( loc, 
                    new NameExpr( loc, "?[?]" ),
                    {
                        new NameExpr( loc, clauseData.at(0)->statusName ),
                        new NameExpr( loc, idxName )
                    }
                ),
                new NameExpr( loc, "__SELECT_SAT" )
            }
        ),      // condition
        ifBody  // body
    );

    return new ForStmt( loc,
        {
            new DeclStmt( loc,
                new ObjectDecl( loc,
                    idxName,
                    new BasicType( BasicType::Kind::SignedInt ),
                    new SingleInit( loc, ConstantExpr::from_int( loc, 0 ) )
                )
            )
        },  // inits
        new UntypedExpr ( loc,
            new NameExpr( loc, "?<?" ),
            {
                new NameExpr( loc, idxName ),
                ConstantExpr::from_int( loc, stmt->clauses.size() )
            }
        ),  // cond
        new UntypedExpr ( loc,
            new NameExpr( loc, "?++" ),
            { new NameExpr( loc, idxName ) }
        ),  // inc
        new CompoundStmt( loc,
            {
                new IfStmt( loc,
                    new UntypedExpr ( loc,
                        new NameExpr( loc, predName ),
                        { new NameExpr( loc, clauseData.at(0)->statusName ) }
                    ),
                    new BranchStmt( loc, BranchStmt::Kind::Break, Label( loc ) )
                ),
                ifSwitch
            }
        )   // body
    );
}

// Generates: !is_full_sat_n() / !is_run_sat_n()
Expr * genNotSatExpr( const WaitUntilStmt * stmt, string & satName, string & arrName ) {
    const CodeLocation & loc = stmt->location;
    return new UntypedExpr ( loc,
        new NameExpr( loc, "!?" ),
        {
            new UntypedExpr ( loc,
                new NameExpr( loc, satName ),
                { new NameExpr( loc, arrName ) }
            )
        }
    );
}

// Generates the code needed for waituntils with an else ( ... )
// Checks clauses once after registering for completion and runs them if completes
// If not enough have run to satisfy predicate after one pass then the else is run
Stmt * GenerateWaitUntilCore::genElseClauseBranch( const WaitUntilStmt * stmt, string & runName, string & arrName, vector<ClauseData *> & clauseData ) {
    return new CompoundStmt( stmt->else_stmt->location,
        {
            genStatusCheckFor( stmt, clauseData, runName ),
            new IfStmt( stmt->else_stmt->location,
                genNotSatExpr( stmt, runName, arrName ),
                ast::deepCopy( stmt->else_stmt )
            )
        }
    );
}

Stmt * GenerateWaitUntilCore::genNoElseClauseBranch( const WaitUntilStmt * stmt, string & satName, string & runName, string & arrName, string & pCountName, vector<ClauseData *> & clauseData ) {
    CompoundStmt * whileBody = new CompoundStmt( stmt->location );
    const CodeLocation & loc = stmt->location;

    // generates: __CFA_maybe_park( &park_counter );
    whileBody->push_back(
        new ExprStmt( loc,
            new UntypedExpr ( loc,
                new NameExpr( loc, "__CFA_maybe_park" ),
                { new AddressExpr( loc, new NameExpr( loc, pCountName ) ) }
            )
        )
    );

    whileBody->push_back( genStatusCheckFor( stmt, clauseData, satName ) );

    return new CompoundStmt( loc,
        {
            new WhileDoStmt( loc,
                genNotSatExpr( stmt, satName, arrName ),
                whileBody,  // body
                {}          // no inits
            ),
            genStatusCheckFor( stmt, clauseData, runName )
        }
    );
}

// generates the following decls for each clause to ensure the target expr and when_cond is only evaluated once
// typeof(target) & __clause_target_0 = target;
// bool __when_cond_0 = when_cond; // only generated if when_cond defined
// select_node clause1;
void GenerateWaitUntilCore::genClauseInits( const WaitUntilStmt * stmt, vector<ClauseData *> & clauseData, CompoundStmt * body, string & statusName, string & elseWhenName ) {
    ClauseData * currClause;
    for ( vector<ClauseData*>::size_type i = 0; i < stmt->clauses.size(); i++ ) {
        currClause = new ClauseData( i, statusName );
        currClause->nodeName = namer_node.newName();
        currClause->targetName = namer_target.newName();
        currClause->whenName = namer_when.newName();
        clauseData.push_back(currClause);
        const CodeLocation & cLoc = stmt->clauses.at(i)->location;

        // typeof(target) & __clause_target_0 = target;
        body->push_back(
            new DeclStmt( cLoc,
                new ObjectDecl( cLoc,
                    currClause->targetName,
                    new ReferenceType( new TypeofType( ast::deepCopy( stmt->clauses.at(i)->target ) ) ),
                    new SingleInit( cLoc, ast::deepCopy( stmt->clauses.at(i)->target ) )
                )
            )
        );

        // bool __when_cond_0 = when_cond; // only generated if when_cond defined
        if ( stmt->clauses.at(i)->when_cond )
            body->push_back(
                new DeclStmt( cLoc,
                    new ObjectDecl( cLoc,
                        currClause->whenName,
                        new BasicType( BasicType::Kind::Bool ),
                        new SingleInit( cLoc, ast::deepCopy( stmt->clauses.at(i)->when_cond ) )
                    )
                )
            );
        
        // select_node clause1;
        body->push_back(
            new DeclStmt( cLoc,
                new ObjectDecl( cLoc,
                    currClause->nodeName,
                    new StructInstType( selectNodeDecl )
                )
            )
        );
    }

    if ( stmt->else_stmt && stmt->else_cond ) {
        body->push_back(
            new DeclStmt( stmt->else_cond->location,
                new ObjectDecl( stmt->else_cond->location,
                    elseWhenName,
                    new BasicType( BasicType::Kind::Bool ),
                    new SingleInit( stmt->else_cond->location, ast::deepCopy( stmt->else_cond ) )
                )
            )
        );
    }
}

/*
if ( clause_status == &clause1 ) ... clause 1 body ...
...
elif ( clause_status == &clausen ) ... clause n body ...
*/
Stmt * GenerateWaitUntilCore::buildOrCaseSwitch( const WaitUntilStmt * stmt, string & statusName, vector<ClauseData *> & data ) {
    const CodeLocation & loc = stmt->location;

    IfStmt * outerIf = nullptr;
	IfStmt * lastIf = nullptr;

	//adds an if/elif clause for each select clause address to run the corresponding clause stmt
	for ( long unsigned int i = 0; i < data.size(); i++ ) {
        const CodeLocation & cLoc = stmt->clauses.at(i)->location;

		IfStmt * currIf = new IfStmt( cLoc,
			new UntypedExpr( cLoc, 
                new NameExpr( cLoc, "?==?" ), 
                {
                    new NameExpr( cLoc, statusName ),
                    new CastExpr( cLoc, 
                        new AddressExpr( cLoc, new NameExpr( cLoc, data.at(i)->nodeName ) ),
                        new BasicType( BasicType::Kind::LongUnsignedInt ), GeneratedFlag::ExplicitCast 
                    )
                }
            ),
            genStmtBlock( stmt->clauses.at(i), data.at(i) )
		);
		
		if ( i == 0 ) {
			outerIf = currIf;
		} else {
			// add ifstmt to else of previous stmt
			lastIf->else_ = currIf;
		}

		lastIf = currIf;
	}

    // C_TODO: will remove this commented code later. Currently it isn't needed but may switch to a modified version of this later if it has better performance
    // std::vector<ptr<CaseClause>> switchCases;

    // int idx = 0;
    // for ( const auto & clause: stmt->clauses ) {
    //     const CodeLocation & cLoc = clause->location;
    //     switchCases.push_back(
    //         new CaseClause( cLoc,
    //             new CastExpr( cLoc, 
    //                 new AddressExpr( cLoc, new NameExpr( cLoc, data.at(idx)->targetName ) ),
    //                 new BasicType( BasicType::Kind::LongUnsignedInt ), GeneratedFlag::ExplicitCast 
    //             ),
    //             {
    //                 new CompoundStmt( cLoc,
    //                     {
    //                         ast::deepCopy( clause->stmt ),
    //                         new BranchStmt( cLoc, BranchStmt::Kind::Break, Label( cLoc ) )
    //                     }
    //                 )
    //             }
    //         )
    //     );
    //     idx++;
    // }

    return new CompoundStmt( loc,
        {
            new ExprStmt( loc, new UntypedExpr( loc, new NameExpr( loc, "park" ) ) ),
            outerIf
            // new SwitchStmt( loc,
            //     new NameExpr( loc, statusName ),
            //     std::move( switchCases )
            // )
        }
    );
}

Stmt * GenerateWaitUntilCore::recursiveOrIfGen( const WaitUntilStmt * stmt, vector<ClauseData *> & data, vector<ClauseData*>::size_type idx, string & elseWhenName ) {
    if ( idx == data.size() ) {   // base case, gen last else
        const CodeLocation & cLoc = stmt->else_stmt->location;
        if ( !stmt->else_stmt ) // normal non-else gen
            return buildOrCaseSwitch( stmt, data.at(0)->statusName, data );

        Expr * raceFnCall = new UntypedExpr( stmt->location,
            new NameExpr( stmt->location, "__select_node_else_race" ),
            { new NameExpr( stmt->location, data.at(0)->nodeName ) }
        );

        if ( stmt->else_stmt && stmt->else_cond ) { // return else conditional on both when and race
            return new IfStmt( cLoc,
                new LogicalExpr( cLoc,
                    new CastExpr( cLoc,
                        new NameExpr( cLoc, elseWhenName ),
                        new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                    ),
                    new CastExpr( cLoc,
                        raceFnCall,
                        new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                    ),
                    LogicalFlag::AndExpr
                ),
                ast::deepCopy( stmt->else_stmt ),
                buildOrCaseSwitch( stmt, data.at(0)->statusName, data )
            );
        }

        // return else conditional on race
        return new IfStmt( stmt->else_stmt->location,
            raceFnCall,
            ast::deepCopy( stmt->else_stmt ),
            buildOrCaseSwitch( stmt, data.at(0)->statusName, data )
        );
    }
    const CodeLocation & cLoc = stmt->clauses.at(idx)->location;

    Expr * ifCond;

    // If we have a when_cond make the register call conditional on it
    if ( stmt->clauses.at(idx)->when_cond ) {
        ifCond = new LogicalExpr( cLoc,
            new CastExpr( cLoc,
                new NameExpr( cLoc, data.at(idx)->whenName ), 
                new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
            ),
            new CastExpr( cLoc,
                genSelectTraitCall( stmt->clauses.at(idx), data.at(idx), "register_select" ),
                new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
            ),
            LogicalFlag::AndExpr
        );
    } else ifCond = genSelectTraitCall( stmt->clauses.at(idx), data.at(idx), "register_select" );

    return new CompoundStmt( cLoc,
        {   // gens: setup_clause( clause1, &status, 0p );
            new ExprStmt( cLoc,
                new UntypedExpr ( cLoc,
                    new NameExpr( cLoc, "setup_clause" ),
                    {
                        new NameExpr( cLoc, data.at(idx)->nodeName ),
                        new AddressExpr( cLoc, new NameExpr( cLoc, data.at(idx)->statusName ) ),
                        ConstantExpr::null( cLoc, new PointerType( new BasicType( BasicType::Kind::SignedInt ) ) )
                    }
                )
            ),
            // gens: if (__when_cond && register_select()) { clause body } else { ... recursiveOrIfGen ... }
            new IfStmt( cLoc,
                ifCond,
                genStmtBlock( stmt->clauses.at(idx), data.at(idx) ),
                // ast::deepCopy( stmt->clauses.at(idx)->stmt ),
                recursiveOrIfGen( stmt, data, idx + 1, elseWhenName )
            )
        }
    );
}

// This gens the special case of an all OR waituntil:
/* 
int status = 0;

typeof(target) & __clause_target_0 = target;
bool __when_cond_0 = when_cond; // only generated if when_cond defined
select_node clause1;
... generate above for rest of clauses ...

try {
    setup_clause( clause1, &status, 0p );
    if ( __when_cond_0 && register_select( 1 ) ) {
        ... clause 1 body ...
    } else {
        ... recursively gen for each of n clauses ...
        setup_clause( clausen, &status, 0p );
        if ( __when_cond_n-1 && register_select( n ) ) {
            ... clause n body ...
        } else {
            if ( else_when ) ... else clause body ...
            else {
                park();

                // after winning the race and before unpark() clause_status is set to be the winning clause index + 1 
                if ( clause_status == &clause1) ... clause 1 body ...
                ...
                elif ( clause_status == &clausen ) ... clause n body ...
            }
        }
    }
}
finally { 
    if ( __when_cond_1 && clause1.status != 0p) unregister_select( 1 ); // if registered unregister
    ...
    if ( __when_cond_n && clausen.status != 0p) unregister_select( n );
}
*/
Stmt * GenerateWaitUntilCore::genAllOr( const WaitUntilStmt * stmt ) {
    const CodeLocation & loc = stmt->location;
    string statusName = namer_status.newName();
    string elseWhenName = namer_when.newName();
    int numClauses = stmt->clauses.size();
    CompoundStmt * body = new CompoundStmt( stmt->location );

    // Generates: unsigned long int status = 0;
    body->push_back( new DeclStmt( loc,
        new ObjectDecl( loc,
            statusName,
            new BasicType( BasicType::Kind::LongUnsignedInt ),
            new SingleInit( loc, ConstantExpr::from_int( loc, 0 ) )
        )
    ));

    vector<ClauseData *> clauseData;
    genClauseInits( stmt, clauseData, body, statusName, elseWhenName );

    vector<int> whenIndices; // track which clauses have whens

    CompoundStmt * unregisters = new CompoundStmt( loc );
    Expr * ifCond;
    for ( int i = 0; i < numClauses; i++ ) {
        const CodeLocation & cLoc = stmt->clauses.at(i)->location;
        // Gens: node.status != 0p
        UntypedExpr * statusPtrCheck = new UntypedExpr( cLoc, 
            new NameExpr( cLoc, "?!=?" ), 
            {
                ConstantExpr::null( cLoc, new PointerType( new BasicType( BasicType::Kind::LongUnsignedInt ) ) ),
                new UntypedExpr( cLoc, 
                    new NameExpr( cLoc, "__get_clause_status" ), 
                    { new NameExpr( cLoc, clauseData.at(i)->nodeName ) } 
                ) 
            }
        );

        // If we have a when_cond make the unregister call conditional on it
        if ( stmt->clauses.at(i)->when_cond ) {
            whenIndices.push_back(i);
            ifCond = new LogicalExpr( cLoc,
                new CastExpr( cLoc,
                    new NameExpr( cLoc, clauseData.at(i)->whenName ), 
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                new CastExpr( cLoc,
                    statusPtrCheck,
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                LogicalFlag::AndExpr
            );
        } else ifCond = statusPtrCheck;
        
        unregisters->push_back(
            new IfStmt( cLoc,
                ifCond,
                new ExprStmt( cLoc, genSelectTraitCall( stmt->clauses.at(i), clauseData.at(i), "unregister_select" ) ) 
            )
        );
    }

    if ( whenIndices.empty() || whenIndices.size() != stmt->clauses.size() ) {
        body->push_back(
                new ast::TryStmt( loc,
                new CompoundStmt( loc, { recursiveOrIfGen( stmt, clauseData, 0, elseWhenName ) } ),
                {},
                new ast::FinallyClause( loc, unregisters )
            )
        );
    } else { // If all clauses have whens, we need to skip the waituntil if they are all false
        Expr * outerIfCond = new NameExpr( loc, clauseData.at( whenIndices.at(0) )->whenName );
        Expr * lastExpr = outerIfCond;

        for ( vector<int>::size_type i = 1; i < whenIndices.size(); i++ ) {
            outerIfCond = new LogicalExpr( loc,
                new CastExpr( loc,
                    new NameExpr( loc, clauseData.at( whenIndices.at(i) )->whenName ), 
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                new CastExpr( loc,
                    lastExpr,
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                LogicalFlag::OrExpr
            );
            lastExpr = outerIfCond;
        }

        body->push_back(
                new ast::TryStmt( loc,
                new CompoundStmt( loc, 
                    {
                        new IfStmt( loc,
                            outerIfCond,
                            recursiveOrIfGen( stmt, clauseData, 0, elseWhenName )
                        )
                    }
                ),
                {},
                new ast::FinallyClause( loc, unregisters )
            )
        );
    }

    for ( ClauseData * datum : clauseData )
        delete datum;

    return body;
}

Stmt * GenerateWaitUntilCore::postvisit( const WaitUntilStmt * stmt ) {
    if ( !selectNodeDecl )
        SemanticError( stmt, "waituntil statement requires #include <waituntil.hfa>" );

    // Prep clause tree to figure out how to set initial statuses
    // setTreeSizes( stmt->predicateTree );
    if ( paintWhenTree( stmt->predicateTree ) ) // if this returns true we can special case since tree is all OR's
        return genAllOr( stmt );

    CompoundStmt * tryBody = new CompoundStmt( stmt->location );
    CompoundStmt * body = new CompoundStmt( stmt->location );
    string statusArrName = namer_status.newName();
    string pCountName = namer_park.newName();
    string satName = namer_sat.newName();
    string runName = namer_run.newName();
    string elseWhenName = namer_when.newName();
    int numClauses = stmt->clauses.size();
    addPredicates( stmt, satName, runName );

    const CodeLocation & loc = stmt->location;

    // Generates: int park_counter = 0;
    body->push_back( new DeclStmt( loc,
        new ObjectDecl( loc,
            pCountName,
            new BasicType( BasicType::Kind::SignedInt ),
            new SingleInit( loc, ConstantExpr::from_int( loc, 0 ) )
        )
    ));

    // Generates: int clause_statuses[3] = { 0 };
    body->push_back( new DeclStmt( loc,
        new ObjectDecl( loc,
            statusArrName,
            new ArrayType( new BasicType( BasicType::Kind::LongUnsignedInt ), ConstantExpr::from_int( loc, numClauses ), LengthFlag::FixedLen, DimensionFlag::DynamicDim ),
            new ListInit( loc,
                {
                    new SingleInit( loc, ConstantExpr::from_int( loc, 0 ) )
                }
            )
        )
    ));

    vector<ClauseData *> clauseData;
    genClauseInits( stmt, clauseData, body, statusArrName, elseWhenName );

    vector<pair<int, WaitUntilStmt::ClauseNode *>> ambiguousClauses;       // list of ambiguous clauses
    vector<int> andWhenClauses;    // list of clauses that have an AND op as a direct parent and when_cond defined

    collectWhens( stmt->predicateTree, ambiguousClauses, andWhenClauses );

    // This is only needed for clauses that have AND as a parent and a when_cond defined
    // generates: if ( ! when_cond_0 ) clause_statuses_0 = __SELECT_RUN;
    for ( int idx : andWhenClauses ) {
        const CodeLocation & cLoc = stmt->clauses.at(idx)->location;
        body->push_back( 
            new IfStmt( cLoc,
                new UntypedExpr ( cLoc,
                    new NameExpr( cLoc, "!?" ),
                    { new NameExpr( cLoc, clauseData.at(idx)->whenName ) }
                ),  // IfStmt cond
                new ExprStmt( cLoc,
                    new UntypedExpr ( cLoc,
                        new NameExpr( cLoc, "?=?" ),
                        {
                            new UntypedExpr ( cLoc, 
                                new NameExpr( cLoc, "?[?]" ),
                                {
                                    new NameExpr( cLoc, statusArrName ),
                                    ConstantExpr::from_int( cLoc, idx )
                                }
                            ),
                            new NameExpr( cLoc, "__SELECT_RUN" )
                        }
                    )
                )  // IfStmt then
            )
        );
    }

    // Only need to generate conditional initial state setting for ambiguous when clauses
    if ( !ambiguousClauses.empty() ) {
        body->push_back( genWhenStateConditions( stmt, clauseData, ambiguousClauses, 0 ) );
    }

    // generates the following for each clause:
    // setup_clause( clause1, &clause_statuses[0], &park_counter );
    // register_select(A, clause1);
    for ( int i = 0; i < numClauses; i++ ) {
        setUpClause( stmt->clauses.at(i), clauseData.at(i), pCountName, tryBody );
    }

    // generate satisfy logic based on if there is an else clause and if it is conditional
    if ( stmt->else_stmt && stmt->else_cond ) { // gen both else/non else branches
        tryBody->push_back(
            new IfStmt( stmt->else_cond->location,
                new NameExpr( stmt->else_cond->location, elseWhenName ),
                genElseClauseBranch( stmt, runName, statusArrName, clauseData ),
                genNoElseClauseBranch( stmt, satName, runName, statusArrName, pCountName, clauseData )
            )
        );
    } else if ( !stmt->else_stmt ) { // normal gen
        tryBody->push_back( genNoElseClauseBranch( stmt, satName, runName, statusArrName, pCountName, clauseData ) );
    } else { // generate just else
        tryBody->push_back( genElseClauseBranch( stmt, runName, statusArrName, clauseData ) );
    }

    CompoundStmt * unregisters = new CompoundStmt( loc );
    // generates for each clause: 
    // if ( !has_run( clause_statuses[i] ) ) 
    // OR if when_cond defined
    // if ( when_cond_i && !has_run( clause_statuses[i] ) )
    // body of if is:
    // { if (unregister_select(A, clause1) && on_selected(A, clause1)) clause1->stmt; } // this conditionally runs the block unregister_select returns true (needed by some primitives)
    Expr * ifCond;
    UntypedExpr * statusExpr; // !clause_statuses[i]
    for ( int i = 0; i < numClauses; i++ ) {
        const CodeLocation & cLoc = stmt->clauses.at(i)->location;

        statusExpr = new UntypedExpr ( cLoc,
            new NameExpr( cLoc, "!?" ),
            {
                new UntypedExpr ( cLoc, 
                    new NameExpr( cLoc, "__CFA_has_clause_run" ),
                    {
                        genArrAccessExpr( cLoc, i, statusArrName )
                    }
                )
            }
        );

        if ( stmt->clauses.at(i)->when_cond ) {
            // generates: if( when_cond_i && !has_run(clause_statuses[i]) )
            ifCond = new LogicalExpr( cLoc,
                new CastExpr( cLoc,
                    new NameExpr( cLoc, clauseData.at(i)->whenName ), 
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                new CastExpr( cLoc,
                    statusExpr,
                    new BasicType( BasicType::Kind::Bool ), GeneratedFlag::ExplicitCast 
                ),
                LogicalFlag::AndExpr
            );
        } else // generates: if( !clause_statuses[i] )
            ifCond = statusExpr;
        
        unregisters->push_back( 
            new IfStmt( cLoc,
                ifCond,
                new CompoundStmt( cLoc,
                    {
                        new IfStmt( cLoc,
                            genSelectTraitCall( stmt->clauses.at(i), clauseData.at(i), "unregister_select" ),
                            // ast::deepCopy( stmt->clauses.at(i)->stmt )
                            genStmtBlock( stmt->clauses.at(i), clauseData.at(i) )
                        )
                    }
                )
                
            )
        );
    }

    body->push_back( 
        new ast::TryStmt(
            loc,
            tryBody,
            {},
            new ast::FinallyClause( loc, unregisters )
        )
    );

    for ( ClauseData * datum : clauseData )
        delete datum;

    return body;
}

// To add the predicates at global scope we need to do it in a second pass
// Predicates are added after "struct select_node { ... };"
class AddPredicateDecls final : public WithDeclsToAdd<> {
    vector<FunctionDecl *> & satFns;
    const StructDecl * selectNodeDecl = nullptr;

  public:
    void previsit( const StructDecl * decl ) {
        if ( !decl->body ) {
            return;
        } else if ( "select_node" == decl->name ) {
            assert( !selectNodeDecl );
            selectNodeDecl = decl;
            for ( FunctionDecl * fn : satFns )
                declsToAddAfter.push_back(fn);            
        }
    }
    AddPredicateDecls( vector<FunctionDecl *> & satFns ): satFns(satFns) {}
};

void generateWaitUntil( TranslationUnit & translationUnit ) {
    vector<FunctionDecl *> satFns;
	Pass<GenerateWaitUntilCore>::run( translationUnit, satFns );
    Pass<AddPredicateDecls>::run( translationUnit, satFns );
}

} // namespace Concurrency

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
