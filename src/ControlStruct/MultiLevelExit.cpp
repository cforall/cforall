//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MultiLevelExit.cpp -- Replaces CFA's local control flow with C's versions.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  1 13:48:00 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 11 13:44:45 2023
// Update Count     : 38
//

#include "MultiLevelExit.hpp"

#include <set>

#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "LabelGenerator.hpp"

using namespace std;
using namespace ast;

namespace ControlStruct {

namespace {

/// The return context is used to remember if returns are allowed and if
/// not, why not. It is the nearest local control flow blocking construct.
enum ReturnContext {
	MayReturn,
	InTryWithHandler,
	InResumeHandler,
	InTerminateHandler,
	InFinally,
};

class Entry {
  public:
	const Stmt * stmt;
  private:
	// Organized like a manual ADT. Avoids creating a bunch of dead data.
	struct Target {
		Label label;
		bool used = false;
		Target( const Label & label ) : label( label ) {}
		Target() : label( CodeLocation(), "" ) {}
	};
	Target firstTarget;
	Target secondTarget;

	enum Kind {
		ForStmtK, WhileDoStmtK, CompoundStmtK, IfStmtK, CaseClauseK, SwitchStmtK, TryStmtK
	} kind;

	bool fallDefaultValid = true;

	static Label & useTarget( Target & target ) {
		target.used = true;
		return target.label;
	}
  public:
	Entry( const ForStmt * stmt, Label breakExit, Label contExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( contExit ), kind( ForStmtK ) {}
	Entry( const WhileDoStmt * stmt, Label breakExit, Label contExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( contExit ), kind( WhileDoStmtK ) {}
	Entry( const CompoundStmt *stmt, Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( CompoundStmtK ) {}
	Entry( const IfStmt *stmt, Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( IfStmtK ) {}
	Entry( const CaseClause *, const CompoundStmt *stmt, Label fallExit ) :
		stmt( stmt ), firstTarget( fallExit ), secondTarget(), kind( CaseClauseK ) {}
	Entry( const SwitchStmt *stmt, Label breakExit, Label fallDefaultExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( fallDefaultExit ), kind( SwitchStmtK ) {}
	Entry( const TryStmt *stmt, Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( TryStmtK ) {}

	bool isContTarget() const { return kind <= WhileDoStmtK; }
	bool isBreakTarget() const { return kind != CaseClauseK; }
	bool isFallTarget() const { return kind == CaseClauseK; }
	bool isFallDefaultTarget() const { return kind == SwitchStmtK; }

	// These routines set a target as being "used" by a BranchStmt
	Label useContExit() { assert( kind <= WhileDoStmtK ); return useTarget(secondTarget); }
	Label useBreakExit() { assert( kind != CaseClauseK ); return useTarget(firstTarget); }
	Label useFallExit() { assert( kind == CaseClauseK );  return useTarget(firstTarget); }
	Label useFallDefaultExit() { assert( kind == SwitchStmtK ); return useTarget(secondTarget); }

	// These routines check if a specific label for a statement is used by a BranchStmt
	bool isContUsed() const { assert( kind <= WhileDoStmtK ); return secondTarget.used; }
	bool isBreakUsed() const { assert( kind != CaseClauseK ); return firstTarget.used; }
	bool isFallUsed() const { assert( kind == CaseClauseK ); return firstTarget.used; }
	bool isFallDefaultUsed() const { assert( kind == SwitchStmtK ); return secondTarget.used; }
	void seenDefault() { fallDefaultValid = false; }
	bool isFallDefaultValid() const { return fallDefaultValid; }
};

// Helper predicates used in find_if calls (it doesn't take methods):
bool isBreakTarget( const Entry & entry ) {
	return entry.isBreakTarget();
}

bool isContinueTarget( const Entry & entry ) {
	return entry.isContTarget();
}

bool isFallthroughTarget( const Entry & entry ) {
	return entry.isFallTarget();
}

bool isFallthroughDefaultTarget( const Entry & entry ) {
	return entry.isFallDefaultTarget();
}

struct MultiLevelExitCore final :
	public WithVisitorRef<MultiLevelExitCore>,
	public WithShortCircuiting, public WithGuards {
	MultiLevelExitCore( const LabelToStmt & lt );

	void previsit( const FunctionDecl * );

	const CompoundStmt * previsit( const CompoundStmt * );
	const BranchStmt * postvisit( const BranchStmt * );
	void previsit( const WhileDoStmt * );
	const WhileDoStmt * postvisit( const WhileDoStmt * );
	void previsit( const ForStmt * );
	const ForStmt * postvisit( const ForStmt * );
	const CaseClause * previsit( const CaseClause * );
	void previsit( const IfStmt * );
	const IfStmt * postvisit( const IfStmt * );
	void previsit( const SwitchStmt * );
	const SwitchStmt * postvisit( const SwitchStmt * );
	void previsit( const ReturnStmt * );
	void previsit( const TryStmt * );
	void postvisit( const TryStmt * );
	void previsit( const CatchClause * );
	void previsit( const FinallyClause * );

	const Stmt * mutateLoop( const Stmt * body, Entry& );

	const LabelToStmt & target_table;
	set<Label> fallthrough_labels;
	vector<Entry> enclosing_control_structures;
	Label break_label;
	ReturnContext ret_context;

	template<typename LoopNode>
	void prehandleLoopStmt( const LoopNode * loopStmt );
	template<typename LoopNode>
	const LoopNode * posthandleLoopStmt( const LoopNode * loopStmt );

	list<ptr<Stmt>> fixBlock(
		const list<ptr<Stmt>> & kids, bool caseClause );

	void enterSealedContext( ReturnContext );

	template<typename UnaryPredicate>
	auto findEnclosingControlStructure( UnaryPredicate pred ) {
		return find_if( enclosing_control_structures.rbegin(),
						enclosing_control_structures.rend(), pred );
	}
};

NullStmt * labelledNullStmt( const CodeLocation & cl, const Label & label ) {
	return new NullStmt( cl, vector<Label>{ label } );
}

MultiLevelExitCore::MultiLevelExitCore( const LabelToStmt & lt ) :
	target_table( lt ), break_label( CodeLocation(), "" ),
	ret_context( ReturnContext::MayReturn )
{}

void MultiLevelExitCore::previsit( const FunctionDecl * ) {
	visit_children = false;
}

const CompoundStmt * MultiLevelExitCore::previsit(
		const CompoundStmt * stmt ) {
	visit_children = false;

	// if the stmt is labelled then generate a label to check in postvisit if the label is used
	bool isLabeled = ! stmt->labels.empty();
	if ( isLabeled ) {
		Label breakLabel = newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction( [this]() { enclosing_control_structures.pop_back(); } );
	}

	auto mutStmt = mutate( stmt );
	// A child statement may set the break label.
	mutStmt->kids = fixBlock( stmt->kids, false );

	if ( isLabeled ) {
		assert( ! enclosing_control_structures.empty() );
		Entry & entry = enclosing_control_structures.back();
		if ( ! entry.useBreakExit().empty() ) {
			break_label = entry.useBreakExit();
		}
	}
	return mutStmt;
}

size_t getUnusedIndex( const Stmt * stmt, const Label & originalTarget ) {
	const size_t size = stmt->labels.size();

	// If the label is empty, do not add unused attribute.
  if ( originalTarget.empty() ) return size;

	// Search for a label that matches the originalTarget.
	for ( size_t i = 0 ; i < size ; ++i ) {
		const Label & label = stmt->labels[i];
		if ( label == originalTarget ) {
			for ( const Attribute * attr : label.attributes ) {
				if ( attr->name == "unused" ) return size;
			}
			return i;
		}
	}
	assertf( false, "CFA internal error: could not find label '%s' on statement %s",
			 originalTarget.name.c_str(), toString( stmt ).c_str() );
}

const Stmt * addUnused( const Stmt * stmt, const Label & originalTarget ) {
	size_t i = getUnusedIndex( stmt, originalTarget );
	if ( i == stmt->labels.size() ) {
		return stmt;
	}
	Stmt * mutStmt = mutate( stmt );
	mutStmt->labels[i].attributes.push_back( new Attribute( "unused" ) );
	return mutStmt;
}

// This routine updates targets on enclosing control structures to indicate which
//     label is used by the BranchStmt that is passed
const BranchStmt * MultiLevelExitCore::postvisit( const BranchStmt * stmt ) {
	vector<Entry>::reverse_iterator targetEntry =
		enclosing_control_structures.rend();

	// Labels on different stmts require different approaches to access
	switch ( stmt->kind ) {
	case BranchStmt::Goto:
		return stmt;
	case BranchStmt::Continue:
	case BranchStmt::Break: {
		bool isContinue = stmt->kind == BranchStmt::Continue;
		// Handle unlabeled break and continue.
		if ( stmt->target.empty() ) {
			if ( isContinue ) {
				targetEntry = findEnclosingControlStructure( isContinueTarget );
			} else {
				if ( enclosing_control_structures.empty() ) {
					  SemanticError( stmt->location,
									 "'break' outside a loop, 'switch', or labelled block" );
				}
				targetEntry = findEnclosingControlStructure( isBreakTarget );
			}
			// Handle labeled break and continue.
		} else {
			// Lookup label in table to find attached control structure.
			targetEntry = findEnclosingControlStructure(
				[ targetStmt = target_table.at(stmt->target) ](auto entry){
					  return entry.stmt == targetStmt;
				} );
		}
		// Ensure that selected target is valid.
		if ( targetEntry == enclosing_control_structures.rend() || ( isContinue && ! isContinueTarget( *targetEntry ) ) ) {
			SemanticError( stmt->location, toString( (isContinue ? "'continue'" : "'break'"),
							" target must be an enclosing ", (isContinue ? "loop: " : "control structure: "),
							stmt->originalTarget ) );
		}
		break;
	}
	// handle fallthrough in case/switch stmts
	case BranchStmt::FallThrough: {
		targetEntry = findEnclosingControlStructure( isFallthroughTarget );
		// Check that target is valid.
		if ( targetEntry == enclosing_control_structures.rend() ) {
			SemanticError( stmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
		}
		if ( ! stmt->target.empty() ) {
			// Labelled fallthrough: target must be a valid fallthough label.
			if ( ! fallthrough_labels.count( stmt->target ) ) {
				SemanticError( stmt->location, toString( "'fallthrough' target must be a later case statement: ",
														   stmt->originalTarget ) );
			}
			return new BranchStmt( stmt->location, BranchStmt::Goto, stmt->originalTarget );
		}
		break;
	}
	case BranchStmt::FallThroughDefault: {
		targetEntry = findEnclosingControlStructure( isFallthroughDefaultTarget );

		// Check if in switch or choose statement.
		if ( targetEntry == enclosing_control_structures.rend() ) {
			SemanticError( stmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
		}

		// Check if switch or choose has default clause.
		auto switchStmt = strict_dynamic_cast< const SwitchStmt * >( targetEntry->stmt );
		bool foundDefault = false;
		for ( auto caseStmt : switchStmt->cases ) {
			if ( caseStmt->isDefault() ) {
				foundDefault = true;
				break;
			}
		}
		if ( ! foundDefault ) {
			SemanticError( stmt->location, "'fallthrough default' must be enclosed in a 'switch' or 'choose'"
						   "control structure with a 'default' clause" );
		}
		break;
	}
	default:
		assert( false );
	}

	// Branch error checks: get the appropriate label name, which is always replaced.
	Label exitLabel( CodeLocation(), "" );
	switch ( stmt->kind ) {
	case BranchStmt::Break:
		assert( ! targetEntry->useBreakExit().empty() );
		exitLabel = targetEntry->useBreakExit();
		break;
	case BranchStmt::Continue:
		assert( ! targetEntry->useContExit().empty() );
		exitLabel = targetEntry->useContExit();
		break;
	case BranchStmt::FallThrough:
		assert( ! targetEntry->useFallExit().empty() );
		exitLabel = targetEntry->useFallExit();
		break;
	case BranchStmt::FallThroughDefault:
		assert( ! targetEntry->useFallDefaultExit().empty() );
		exitLabel = targetEntry->useFallDefaultExit();
		// Check that fallthrough default comes before the default clause.
		if ( ! targetEntry->isFallDefaultValid() ) {
			SemanticError( stmt->location, "'fallthrough default' must precede the 'default' clause" );
		}
		break;
	default:
		assert(0);
	}

	// Add unused attribute to silence warnings.
	targetEntry->stmt = addUnused( targetEntry->stmt, stmt->originalTarget );

	// Replace with goto to make later passes more uniform.
	return new BranchStmt( stmt->location, BranchStmt::Goto, exitLabel );
}

void MultiLevelExitCore::previsit( const WhileDoStmt * stmt ) {
	return prehandleLoopStmt( stmt );
}

const WhileDoStmt * MultiLevelExitCore::postvisit( const WhileDoStmt * stmt ) {
	return posthandleLoopStmt( stmt );
}

void MultiLevelExitCore::previsit( const ForStmt * stmt ) {
	return prehandleLoopStmt( stmt );
}

const ForStmt * MultiLevelExitCore::postvisit( const ForStmt * stmt ) {
	return posthandleLoopStmt( stmt );
}

// Mimic what the built-in push_front would do anyways. It is O(n).
void push_front( vector<ptr<Stmt>> & vec, const Stmt * element ) {
	vec.emplace_back( nullptr );
	for ( size_t i = vec.size() - 1 ; 0 < i ; --i ) {
		vec[ i ] = std::move( vec[ i - 1 ] );
	}
	vec[ 0 ] = element;
}

const CaseClause * MultiLevelExitCore::previsit( const CaseClause * stmt ) {
	visit_children = false;

	// If default, mark seen.
	if ( stmt->isDefault() ) {
		assert( ! enclosing_control_structures.empty() );
		enclosing_control_structures.back().seenDefault();
	}

	// The cond may not exist, but if it does update it now.
	visitor->maybe_accept( stmt, &CaseClause::cond );

	// Just save the mutated node for simplicity.
	CaseClause * mutStmt = mutate( stmt );

	Label fallLabel = newLabel( "fallThrough", stmt->location );
	if ( ! mutStmt->stmts.empty() ) {
		// These should already be in a block.
		auto first = mutStmt->stmts.front().get_and_mutate();
		auto block = strict_dynamic_cast<CompoundStmt *>( first );

		// Ensure that the stack isn't corrupted by exceptions in fixBlock.
		auto guard = makeFuncGuard(
			[&](){ enclosing_control_structures.emplace_back( mutStmt, block, fallLabel ); },
			[this](){ enclosing_control_structures.pop_back(); }
			);

		block->kids = fixBlock( block->kids, true );

		// Add fallthrough label if necessary.
		assert( ! enclosing_control_structures.empty() );
		Entry & entry = enclosing_control_structures.back();
		if ( entry.isFallUsed() ) {
			mutStmt->stmts.push_back( labelledNullStmt( block->location, entry.useFallExit() ) );
		}
	}
	assert( ! enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assertf( dynamic_cast< const SwitchStmt * >( entry.stmt ),
			 "CFA internal error: control structure enclosing a case clause must be a switch, but is: %s",
			 toString( entry.stmt ).c_str() );
	if ( mutStmt->isDefault() ) {
		if ( entry.isFallDefaultUsed() ) {
			// Add fallthrough default label if necessary.
			push_front( mutStmt->stmts, labelledNullStmt( stmt->location, entry.useFallDefaultExit() ) );
		}
	}
	return mutStmt;
}

void MultiLevelExitCore::previsit( const IfStmt * stmt ) {
	bool labeledBlock = ! stmt->labels.empty();
	if ( labeledBlock ) {
		Label breakLabel = newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction( [this](){ enclosing_control_structures.pop_back(); } );
	}
}

const IfStmt * MultiLevelExitCore::postvisit( const IfStmt * stmt ) {
	bool labeledBlock = ! stmt->labels.empty();
	if ( labeledBlock ) {
		auto this_label = enclosing_control_structures.back().useBreakExit();
		if ( ! this_label.empty() ) {
			break_label = this_label;
		}
	}
	return stmt;
}

static bool isDefaultCase( const ptr<CaseClause> & caseClause ) {
	return caseClause->isDefault();
}

void MultiLevelExitCore::previsit( const SwitchStmt * stmt ) {
	Label label = newLabel( "switchBreak", stmt );
	auto it = find_if( stmt->cases.rbegin(), stmt->cases.rend(), isDefaultCase );

	const CaseClause * defaultCase = it != stmt->cases.rend() ? (*it) : nullptr;
	Label defaultLabel = defaultCase ? newLabel( "fallThroughDefault", defaultCase->location ) : Label( stmt->location, "" );
	enclosing_control_structures.emplace_back( stmt, label, defaultLabel );
	GuardAction( [this]() { enclosing_control_structures.pop_back(); } );

	// Collect valid labels for fallthrough. It starts with all labels at this level, then remove as each is seen during
	// traversal.
	for ( const CaseClause * caseStmt : stmt->cases ) {
		if ( caseStmt->stmts.empty() ) continue;
		auto block = caseStmt->stmts.front().strict_as<CompoundStmt>();
		for ( const Stmt * stmt : block->kids ) {
			for ( const Label & l : stmt->labels ) {
				fallthrough_labels.insert( l );
			}
		}
	}
}

const SwitchStmt * MultiLevelExitCore::postvisit( const SwitchStmt * stmt ) {
	assert( ! enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assert( entry.stmt == stmt );

	// Only run to generate the break label.
	if ( entry.isBreakUsed() ) {
		// To keep the switch statements uniform (all direct children of a SwitchStmt should be CastStmts), append the
		// exit label and break to the last case, create a default case if no cases.
		SwitchStmt * mutStmt = mutate( stmt );
		if ( mutStmt->cases.empty() ) {
			mutStmt->cases.push_back( new CaseClause( mutStmt->location, nullptr, {} ) );
		}

		auto caseStmt = mutStmt->cases.back().get();
		auto mutCase = mutate( caseStmt );
		mutStmt->cases.back() = mutCase;

		Label label( mutCase->location, "breakLabel" );
		auto branch = new BranchStmt( mutCase->location, BranchStmt::Break, label );
		branch->labels.push_back( entry.useBreakExit() );
		mutCase->stmts.push_back( branch );

		return mutStmt;
	}
	return stmt;
}

void MultiLevelExitCore::previsit( const ReturnStmt * stmt ) {
	char const * context;
	switch ( ret_context ) {
	case ReturnContext::MayReturn:
		return;
	case ReturnContext::InTryWithHandler:
		context = "try statement with a catch clause";
		break;
	case ReturnContext::InResumeHandler:
		context = "catchResume clause";
		break;
	case ReturnContext::InTerminateHandler:
		context = "catch clause";
		break;
	case ReturnContext::InFinally:
		context = "finally clause";
		break;
	default:
		assert(0);
	}
	SemanticError( stmt->location, "'return' may not appear in a %s", context );
}

void MultiLevelExitCore::previsit( const TryStmt * stmt ) {
	bool isLabeled = ! stmt->labels.empty();
	if ( isLabeled ) {
		Label breakLabel = newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction([this](){ enclosing_control_structures.pop_back(); } );
	}

	// Try statements/try blocks are only sealed with a termination handler.
	for ( auto clause : stmt->handlers ) {
		if ( ast::Terminate == clause->kind ) {
			return enterSealedContext( ReturnContext::InTryWithHandler );
		}
	}
}

void MultiLevelExitCore::postvisit( const TryStmt * stmt ) {
	bool isLabeled = ! stmt->labels.empty();
	if ( isLabeled ) {
		auto this_label = enclosing_control_structures.back().useBreakExit();
		if ( ! this_label.empty() ) {
			break_label = this_label;
		}
	}
}

void MultiLevelExitCore::previsit( const CatchClause * clause ) {
	ReturnContext context = ( ast::Terminate == clause->kind )
		? ReturnContext::InTerminateHandler : ReturnContext::InResumeHandler;
	enterSealedContext( context );
}

void MultiLevelExitCore::previsit( const FinallyClause * ) {
	enterSealedContext( ReturnContext::InFinally );
}

const Stmt * MultiLevelExitCore::mutateLoop(
	const Stmt * body, Entry & entry ) {
	if ( entry.isBreakUsed() ) {
		break_label = entry.useBreakExit();
	}

	// if continue is used insert a continue label into the back of the body of the loop
	if ( entry.isContUsed() ) {
		// {
		//  body
		//  ContinueLabel: ;
		// }
		return new CompoundStmt( body->location, {
			body,
			labelledNullStmt( body->location, entry.useContExit() ),
		} );
	}

	return body;
}

template<typename LoopNode>
void MultiLevelExitCore::prehandleLoopStmt( const LoopNode * loopStmt ) {
	// Remember is loop before going onto mutate the body.
	// The labels will be folded in if they are used.
	Label breakLabel = newLabel( "loopBreak", loopStmt );
	Label contLabel = newLabel( "loopContinue", loopStmt );
	enclosing_control_structures.emplace_back( loopStmt, breakLabel, contLabel );
	// labels are added temporarily to see if they are used and then added permanently in postvisit if ther are used
	// children will tag labels as being used during their traversal which occurs before postvisit

	// GuardAction calls the lambda after the node is done being visited
	GuardAction( [this](){ enclosing_control_structures.pop_back(); } );
}

template<typename LoopNode>
const LoopNode * MultiLevelExitCore::posthandleLoopStmt( const LoopNode * loopStmt ) {
	assert( ! enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assert( entry.stmt == loopStmt );

	// Now check if the labels are used and add them if so.
	return mutate_field( loopStmt, &LoopNode::body, mutateLoop( loopStmt->body, entry ) );
	// this call to mutate_field compares loopStmt->body and the result of mutateLoop
	// 		if they are the same the node isn't mutated, if they differ then the new mutated node is returned
	// 		the stmts will only differ if a label is used
}

list<ptr<Stmt>> MultiLevelExitCore::fixBlock(
	const list<ptr<Stmt>> & kids, bool is_case_clause ) {
	// Unfortunately cannot use automatic error collection.
	SemanticErrorException errors;

	list<ptr<Stmt>> ret;

	// Manually visit each child.
	for ( const ptr<Stmt> & kid : kids ) {
		if ( is_case_clause ) {
			// Once a label is seen, it's no longer a valid for fallthrough.
			for ( const Label & l : kid->labels ) {
				fallthrough_labels.erase( l );
			}
		}

		ptr<Stmt> else_stmt = nullptr;
		const Stmt * loop_kid = nullptr;
		// check if loop node and if so add else clause if it exists
		const WhileDoStmt * whilePtr = kid.as<WhileDoStmt>();
		if ( whilePtr && whilePtr->else_ ) {
			else_stmt = whilePtr->else_;
			loop_kid = mutate_field( whilePtr, &WhileDoStmt::else_, nullptr );
		}
		const ForStmt * forPtr = kid.as<ForStmt>();
		if ( forPtr && forPtr->else_ ) {
			else_stmt = forPtr->else_;
			loop_kid = mutate_field( forPtr, &ForStmt::else_, nullptr );
		}

		try {
			if (else_stmt) ret.push_back( loop_kid->accept( *visitor ) );
			else ret.push_back( kid->accept( *visitor ) );
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		}

		if (else_stmt) ret.push_back(else_stmt);

		if ( ! break_label.empty() ) {
			ret.push_back( labelledNullStmt( ret.back()->location, break_label ) );
			break_label = Label( CodeLocation(), "" );
		}
	}

	if ( ! errors.isEmpty() ) {
		throw errors;
	}
	return ret;
}

void MultiLevelExitCore::enterSealedContext( ReturnContext enter_context ) {
	GuardAction([this, old = std::move(enclosing_control_structures)](){ enclosing_control_structures = std::move(old); });
	enclosing_control_structures = vector<Entry>();
	GuardValue( ret_context ) = enter_context;
}

} // namespace

const CompoundStmt * multiLevelExitUpdate(
		const CompoundStmt * stmt, const LabelToStmt & labelTable ) {
	// Must start in the body, so FunctionDecls can be a stopping point.
	Pass<MultiLevelExitCore> visitor( labelTable );
	return stmt->accept( visitor );
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
