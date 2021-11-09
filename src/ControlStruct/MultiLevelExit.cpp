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
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 10:56:00 2021
// Update Count     : 2
//

#include "MultiLevelExit.hpp"

#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "ControlStruct/LabelGenerator.h"

#include <set>

namespace ControlStruct {

namespace {

class Entry {
public:
	const ast::Stmt * stmt;
private:
	// Organized like a manual ADT. Avoids creating a bunch of dead data.
	struct Target {
		ast::Label label;
		bool used = false;
		Target( const ast::Label & label ) : label( label ) {}
		Target() : label( CodeLocation() ) {}
	};
	Target firstTarget;
	Target secondTarget;

	enum Kind {
		ForStmt, WhileStmt, CompoundStmt, IfStmt, CaseStmt, SwitchStmt, TryStmt
	} kind;

	bool fallDefaultValid = true;

	static ast::Label & useTarget( Target & target ) {
		target.used = true;
		return target.label;
	}

public:
	Entry( const ast::ForStmt * stmt, ast::Label breakExit, ast::Label contExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( contExit ), kind( ForStmt ) {}
	Entry( const ast::WhileStmt * stmt, ast::Label breakExit, ast::Label contExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( contExit ), kind( WhileStmt ) {}
	Entry( const ast::CompoundStmt *stmt, ast::Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( CompoundStmt ) {}
	Entry( const ast::IfStmt *stmt, ast::Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( IfStmt ) {}
	Entry( const ast::CaseStmt *stmt, ast::Label fallExit ) :
		stmt( stmt ), firstTarget( fallExit ), secondTarget(), kind( CaseStmt ) {}
	Entry( const ast::SwitchStmt *stmt, ast::Label breakExit, ast::Label fallDefaultExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget( fallDefaultExit ), kind( SwitchStmt ) {}
	Entry( const ast::TryStmt *stmt, ast::Label breakExit ) :
		stmt( stmt ), firstTarget( breakExit ), secondTarget(), kind( TryStmt ) {}

	bool isContTarget() const { return kind <= WhileStmt; }
	bool isBreakTarget() const { return CaseStmt != kind; }
	bool isFallTarget() const { return CaseStmt == kind; }
	bool isFallDefaultTarget() const { return SwitchStmt == kind; }

	ast::Label useContExit() { assert( kind <= WhileStmt ); return useTarget(secondTarget); }
	ast::Label useBreakExit() { assert( CaseStmt != kind ); return useTarget(firstTarget); }
	ast::Label useFallExit() { assert( CaseStmt == kind );  return useTarget(firstTarget); }
	ast::Label useFallDefaultExit() { assert( SwitchStmt == kind ); return useTarget(secondTarget); }

	bool isContUsed() const { assert( kind <= WhileStmt ); return secondTarget.used; }
	bool isBreakUsed() const { assert( CaseStmt != kind ); return firstTarget.used; }
	bool isFallUsed() const { assert( CaseStmt == kind ); return firstTarget.used; }
	bool isFallDefaultUsed() const { assert( SwitchStmt == kind ); return secondTarget.used; }
	void seenDefault() { fallDefaultValid = false; }
	bool isFallDefaultValid() const { return fallDefaultValid; }
};

// Helper predicates used in std::find_if calls (it doesn't take methods):
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
		public ast::WithVisitorRef<MultiLevelExitCore>,
		public ast::WithShortCircuiting, public ast::WithGuards {
	MultiLevelExitCore( const LabelToStmt & lt );

	void previsit( const ast::FunctionDecl * );

	const ast::CompoundStmt * previsit( const ast::CompoundStmt * );
	const ast::BranchStmt * postvisit( const ast::BranchStmt * );
	void previsit( const ast::WhileStmt * );
	const ast::WhileStmt * postvisit( const ast::WhileStmt * );
	void previsit( const ast::ForStmt * );
	const ast::ForStmt * postvisit( const ast::ForStmt * );
	const ast::CaseStmt * previsit( const ast::CaseStmt * );
	void previsit( const ast::IfStmt * );
	const ast::IfStmt * postvisit( const ast::IfStmt * );
	void previsit( const ast::SwitchStmt * );
	const ast::SwitchStmt * postvisit( const ast::SwitchStmt * );
	void previsit( const ast::ReturnStmt * );
	void previsit( const ast::TryStmt * );
	void postvisit( const ast::TryStmt * );
	void previsit( const ast::FinallyStmt * );

	const ast::Stmt * mutateLoop( const ast::Stmt * body, Entry& );

	const LabelToStmt & target_table;
	std::set<ast::Label> fallthrough_labels;
	std::vector<Entry> enclosing_control_structures;
	ast::Label break_label;
	bool inFinally;

	template<typename LoopNode>
	void prehandleLoopStmt( const LoopNode * loopStmt );
	template<typename LoopNode>
	const LoopNode * posthandleLoopStmt( const LoopNode * loopStmt );

	std::list<ast::ptr<ast::Stmt>> fixBlock(
		const std::list<ast::ptr<ast::Stmt>> & kids, bool caseClause );

	template<typename UnaryPredicate>
	auto findEnclosingControlStructure( UnaryPredicate pred ) {
		return std::find_if( enclosing_control_structures.rbegin(),
			enclosing_control_structures.rend(), pred );
	}
};

ast::NullStmt * labelledNullStmt(
		const CodeLocation & cl, const ast::Label & label ) {
	return new ast::NullStmt( cl, std::vector<ast::Label>{ label } );
}

MultiLevelExitCore::MultiLevelExitCore( const LabelToStmt & lt ) :
	target_table( lt ), break_label( CodeLocation(), "" ),
	inFinally( false )
{}

void MultiLevelExitCore::previsit( const ast::FunctionDecl * ) {
	visit_children = false;
}

const ast::CompoundStmt * MultiLevelExitCore::previsit(
		const ast::CompoundStmt * stmt ) {
	visit_children = false;
	bool isLabeled = !stmt->labels.empty();
	if ( isLabeled ) {
		ast::Label breakLabel = LabelGenerator::newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction( [this]() { enclosing_control_structures.pop_back(); } );
	}

	auto mutStmt = ast::mutate( stmt );
	// A child statement may set the break label.
	mutStmt->kids = std::move( fixBlock( stmt->kids, false ) );

	if ( isLabeled ) {
		assert( !enclosing_control_structures.empty() );
		Entry & entry = enclosing_control_structures.back();
		if ( !entry.useBreakExit().empty() ) {
			break_label = entry.useBreakExit();
		}
	}
	return mutStmt;
}

size_t getUnusedIndex(
		const ast::Stmt * stmt, const ast::Label & originalTarget ) {
	const size_t size = stmt->labels.size();

	// If the label is empty, we can skip adding the unused attribute:
	if ( originalTarget.empty() ) return size;

	// Search for a label that matches the originalTarget.
	for ( size_t i = 0 ; i < size ; ++i ) {
		const ast::Label & label = stmt->labels[i];
		if ( label == originalTarget ) {
			for ( const ast::Attribute * attr : label.attributes ) {
				if ( attr->name == "unused" ) return size;
			}
			return i;
		}
	}
	assertf( false, "Could not find label '%s' on statement %s",
		originalTarget.name.c_str(), toString( stmt ).c_str() );
}

const ast::Stmt * addUnused(
		const ast::Stmt * stmt, const ast::Label & originalTarget ) {
	size_t i = getUnusedIndex( stmt, originalTarget );
	if ( i == stmt->labels.size() ) {
		return stmt;
	}
	ast::Stmt * mutStmt = ast::mutate( stmt );
	mutStmt->labels[i].attributes.push_back( new ast::Attribute( "unused" ) );
	return mutStmt;
}

const ast::BranchStmt * MultiLevelExitCore::postvisit( const ast::BranchStmt * stmt ) {
	std::vector<Entry>::reverse_iterator targetEntry =
		enclosing_control_structures.rend();
	switch ( stmt->kind ) {
	case ast::BranchStmt::Goto:
		return stmt;
	case ast::BranchStmt::Continue:
	case ast::BranchStmt::Break: {
		bool isContinue = stmt->kind == ast::BranchStmt::Continue;
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
		if ( targetEntry == enclosing_control_structures.rend() || ( isContinue && !isContinueTarget( *targetEntry ) ) ) {
			SemanticError(
				stmt->location,
				toString( (isContinue ? "'continue'" : "'break'"),
					" target must be an enclosing ",
					(isContinue ? "loop: " : "control structure: "),
					stmt->originalTarget ) );
		}
		break;
	}
	case ast::BranchStmt::FallThrough: {
		targetEntry = findEnclosingControlStructure( isFallthroughTarget );
		// Check that target is valid.
		if ( targetEntry == enclosing_control_structures.rend() ) {
			SemanticError( stmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
		}
		if ( !stmt->target.empty() ) {
			// Labelled fallthrough: target must be a valid fallthough label.
			if ( !fallthrough_labels.count( stmt->target ) ) {
				SemanticError( stmt->location, toString( "'fallthrough' target must be a later case statement: ", stmt->originalTarget ) );
			}
			return new ast::BranchStmt(
				stmt->location, ast::BranchStmt::Goto, stmt->originalTarget );
		}
		break;
	}
	case ast::BranchStmt::FallThroughDefault: {
		targetEntry = findEnclosingControlStructure( isFallthroughDefaultTarget );

		// Check that this is in a switch or choose statement.
		if ( targetEntry == enclosing_control_structures.rend() ) {
			SemanticError( stmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
		}

		// Check that the switch or choose has a default clause.
		auto switchStmt = strict_dynamic_cast< const ast::SwitchStmt * >(
			targetEntry->stmt );
		bool foundDefault = false;
		for ( auto subStmt : switchStmt->stmts ) {
			const ast::CaseStmt * caseStmt = subStmt.strict_as<ast::CaseStmt>();
			if ( caseStmt->isDefault() ) {
				foundDefault = true;
				break;
			}
		}
		if ( !foundDefault ) {
			SemanticError( stmt->location, "'fallthrough default' must be enclosed in a 'switch' or 'choose' control structure with a 'default' clause" );
		}
		break;
	}
	default:
		assert( false );
	}

	// Branch error checks: get the appropriate label name:
	// (This label will always be replaced.)
	ast::Label exitLabel( CodeLocation(), "" );
	switch ( stmt->kind ) {
	case ast::BranchStmt::Break:
		assert( !targetEntry->useBreakExit().empty() );
		exitLabel = targetEntry->useBreakExit();
		break;
	case ast::BranchStmt::Continue:
		assert( !targetEntry->useContExit().empty() );
		exitLabel = targetEntry->useContExit();
		break;
	case ast::BranchStmt::FallThrough:
		assert( !targetEntry->useFallExit().empty() );
		exitLabel = targetEntry->useFallExit();
		break;
	case ast::BranchStmt::FallThroughDefault:
		assert( !targetEntry->useFallDefaultExit().empty() );
		exitLabel = targetEntry->useFallDefaultExit();
		// Check that fallthrough default comes before the default clause.
		if ( !targetEntry->isFallDefaultValid() ) {
			SemanticError( stmt->location,
				"'fallthrough default' must precede the 'default' clause" );
		}
		break;
	default:
		assert(0);
	}

	// Add unused attribute to silence warnings.
	targetEntry->stmt = addUnused( targetEntry->stmt, stmt->originalTarget );

	// Replace this with a goto to make later passes more uniform.
	return new ast::BranchStmt( stmt->location, ast::BranchStmt::Goto, exitLabel );
}

void MultiLevelExitCore::previsit( const ast::WhileStmt * stmt ) {
	return prehandleLoopStmt( stmt );
}

const ast::WhileStmt * MultiLevelExitCore::postvisit( const ast::WhileStmt * stmt ) {
	return posthandleLoopStmt( stmt );
}

void MultiLevelExitCore::previsit( const ast::ForStmt * stmt ) {
	return prehandleLoopStmt( stmt );
}

const ast::ForStmt * MultiLevelExitCore::postvisit( const ast::ForStmt * stmt ) {
	return posthandleLoopStmt( stmt );
}

// Mimic what the built-in push_front would do anyways. It is O(n).
void push_front(
		std::vector<ast::ptr<ast::Stmt>> & vec, const ast::Stmt * element ) {
	vec.emplace_back( nullptr );
	for ( size_t i = vec.size() - 1 ; 0 < i ; --i ) {
		vec[ i ] = std::move( vec[ i - 1 ] );
	}
	vec[ 0 ] = element;
}

const ast::CaseStmt * MultiLevelExitCore::previsit( const ast::CaseStmt * stmt ) {
	visit_children = false;

	// If it is the default, mark the default as seen.
	if ( stmt->isDefault() ) {
		assert( !enclosing_control_structures.empty() );
		enclosing_control_structures.back().seenDefault();
	}

	// The cond may not exist, but if it does update it now.
	visitor->maybe_accept( stmt, &ast::CaseStmt::cond );

	// Just save the mutated node for simplicity.
	ast::CaseStmt * mutStmt = ast::mutate( stmt );

	ast::Label fallLabel = LabelGenerator::newLabel( "fallThrough", stmt );
	if ( !mutStmt->stmts.empty() ) {
		// Ensure that the stack isn't corrupted by exceptions in fixBlock.
		auto guard = makeFuncGuard(
			[&](){ enclosing_control_structures.emplace_back( mutStmt, fallLabel ); },
			[this](){ enclosing_control_structures.pop_back(); }
		);

		// These should already be in a block.
		auto block = ast::mutate( mutStmt->stmts.front().strict_as<ast::CompoundStmt>() );
		block->kids = fixBlock( block->kids, true );

		// Add fallthrough label if necessary.
		assert( !enclosing_control_structures.empty() );
		Entry & entry = enclosing_control_structures.back();
		if ( entry.isFallUsed() ) {
			mutStmt->stmts.push_back(
				labelledNullStmt( mutStmt->location, entry.useFallExit() ) );
		}
	}
	assert( !enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assertf( dynamic_cast< const ast::SwitchStmt * >( entry.stmt ),
		"Control structure enclosing a case clause must be a switch, but is: %s",
		toString( entry.stmt ).c_str() );
	if ( mutStmt->isDefault() ) {
		if ( entry.isFallDefaultUsed() ) {
			// Add fallthrough default label if necessary.
			push_front( mutStmt->stmts, labelledNullStmt(
				stmt->location, entry.useFallDefaultExit()
			) );
		}
	}
	return mutStmt;
}

void MultiLevelExitCore::previsit( const ast::IfStmt * stmt ) {
	bool labeledBlock = !stmt->labels.empty();
	if ( labeledBlock ) {
		ast::Label breakLabel = LabelGenerator::newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction( [this](){ enclosing_control_structures.pop_back(); } );
	}
}

const ast::IfStmt * MultiLevelExitCore::postvisit( const ast::IfStmt * stmt ) {
	bool labeledBlock = !stmt->labels.empty();
	if ( labeledBlock ) {
		auto this_label = enclosing_control_structures.back().useBreakExit();
		if ( !this_label.empty() ) {
			break_label = this_label;
		}
	}
	return stmt;
}

bool isDefaultCase( const ast::ptr<ast::Stmt> & stmt ) {
	const ast::CaseStmt * caseStmt = stmt.strict_as<ast::CaseStmt>();
	return caseStmt->isDefault();
}

void MultiLevelExitCore::previsit( const ast::SwitchStmt * stmt ) {
	ast::Label label = LabelGenerator::newLabel( "switchBreak", stmt );
	auto it = std::find_if( stmt->stmts.rbegin(), stmt->stmts.rend(), isDefaultCase );

	const ast::CaseStmt * defaultCase = it != stmt->stmts.rend()
		? (it)->strict_as<ast::CaseStmt>() : nullptr;
	ast::Label defaultLabel = defaultCase
		? LabelGenerator::newLabel( "fallThroughDefault", defaultCase )
		: ast::Label( stmt->location, "" );
	enclosing_control_structures.emplace_back( stmt, label, defaultLabel );
	GuardAction( [this]() { enclosing_control_structures.pop_back(); } );

	// Collect valid labels for fallthrough. It starts with all labels at
	// this level, then removed as we see them in traversal.
	for ( const ast::Stmt * stmt : stmt->stmts ) {
		auto * caseStmt = strict_dynamic_cast< const ast::CaseStmt * >( stmt );
		if ( caseStmt->stmts.empty() ) continue;
		auto block = caseStmt->stmts.front().strict_as<ast::CompoundStmt>();
		for ( const ast::Stmt * stmt : block->kids ) {
			for ( const ast::Label & l : stmt->labels ) {
				fallthrough_labels.insert( l );
			}
		}
	}
}

const ast::SwitchStmt * MultiLevelExitCore::postvisit( const ast::SwitchStmt * stmt ) {
	assert( !enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assert( entry.stmt == stmt );

	// Only run if we need to generate the break label.
	if ( entry.isBreakUsed() ) {
		// To keep the switch statements uniform (all direct children of a
		// SwitchStmt should be CastStmts), append the exit label and break
		// to the last case, create a default case is there are no cases.
		ast::SwitchStmt * mutStmt = ast::mutate( stmt );
		if ( mutStmt->stmts.empty() ) {
			mutStmt->stmts.push_back( new ast::CaseStmt(
				mutStmt->location, nullptr, {} ));
		}

		auto caseStmt = mutStmt->stmts.back().strict_as<ast::CaseStmt>();
		auto mutCase = ast::mutate( caseStmt );
		mutStmt->stmts.back() = mutCase;

		ast::Label label( mutCase->location, "breakLabel" );
		auto branch = new ast::BranchStmt( mutCase->location, ast::BranchStmt::Break, label );
		branch->labels.push_back( entry.useBreakExit() );
		mutCase->stmts.push_back( branch );

		return mutStmt;
	}
	return stmt;
}

void MultiLevelExitCore::previsit( const ast::ReturnStmt * stmt ) {
	if ( inFinally ) {
		SemanticError( stmt->location, "'return' may not appear in a finally clause" );
	}
}

void MultiLevelExitCore::previsit( const ast::TryStmt * stmt ) {
	bool isLabeled = !stmt->labels.empty();
	if ( isLabeled ) {
		ast::Label breakLabel = LabelGenerator::newLabel( "blockBreak", stmt );
		enclosing_control_structures.emplace_back( stmt, breakLabel );
		GuardAction([this](){ enclosing_control_structures.pop_back(); } );
	}
}

void MultiLevelExitCore::postvisit( const ast::TryStmt * stmt ) {
	bool isLabeled = !stmt->labels.empty();
	if ( isLabeled ) {
		auto this_label = enclosing_control_structures.back().useBreakExit();
		if ( !this_label.empty() ) {
			break_label = this_label;
		}
	}
}

void MultiLevelExitCore::previsit( const ast::FinallyStmt * ) {
	GuardAction([this, old = std::move(enclosing_control_structures)](){
		enclosing_control_structures = std::move(old);
	});
	enclosing_control_structures = std::vector<Entry>();
	GuardValue( inFinally ) = true;
}

const ast::Stmt * MultiLevelExitCore::mutateLoop(
		const ast::Stmt * body, Entry & entry ) {
	if ( entry.isBreakUsed() ) {
		break_label = entry.useBreakExit();
	}

	if ( entry.isContUsed() ) {
		ast::CompoundStmt * new_body = new ast::CompoundStmt( body->location );
		new_body->kids.push_back( body );
		new_body->kids.push_back(
			labelledNullStmt( body->location, entry.useContExit() ) );
		return new_body;
	}

	return body;
}

template<typename LoopNode>
void MultiLevelExitCore::prehandleLoopStmt( const LoopNode * loopStmt ) {
	// Remember is loop before going onto mutate the body.
	// The labels will be folded in if they are used.
	ast::Label breakLabel = LabelGenerator::newLabel( "loopBreak", loopStmt );
	ast::Label contLabel = LabelGenerator::newLabel( "loopContinue", loopStmt );
	enclosing_control_structures.emplace_back( loopStmt, breakLabel, contLabel );
	GuardAction( [this](){ enclosing_control_structures.pop_back(); } );
}

template<typename LoopNode>
const LoopNode * MultiLevelExitCore::posthandleLoopStmt( const LoopNode * loopStmt ) {
	assert( !enclosing_control_structures.empty() );
	Entry & entry = enclosing_control_structures.back();
	assert( entry.stmt == loopStmt );

	// Now we check if the labels are used and add them if so.
	return ast::mutate_field(
		loopStmt, &LoopNode::body, mutateLoop( loopStmt->body, entry ) );
}

std::list<ast::ptr<ast::Stmt>> MultiLevelExitCore::fixBlock(
		const std::list<ast::ptr<ast::Stmt>> & kids, bool is_case_clause ) {
	// Unfortunately we can't use the automatic error collection.
	SemanticErrorException errors;

	std::list<ast::ptr<ast::Stmt>> ret;

	// Manually visit each child.
	for ( const ast::ptr<ast::Stmt> & kid : kids ) {
		if ( is_case_clause ) {
			// Once a label is seen, it's no longer a valid for fallthrough.
			for ( const ast::Label & l : kid->labels ) {
				fallthrough_labels.erase( l );
			}
		}

		try {
			ret.push_back( kid->accept( *visitor ) );
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		}

		if ( !break_label.empty() ) {
			ret.push_back(
				labelledNullStmt( ret.back()->location, break_label ) );
			break_label = ast::Label( CodeLocation(), "" );
		}
	}

	if ( !errors.isEmpty() ) {
		throw errors;
	}
	return ret;
}

} // namespace

const ast::CompoundStmt * multiLevelExitUpdate(
    	const ast::CompoundStmt * stmt,
		const LabelToStmt & labelTable ) {
	// Must start in the body, so FunctionDecls can be a stopping point.
	ast::Pass<MultiLevelExitCore> visitor( labelTable );
	const ast::CompoundStmt * ret = stmt->accept( visitor );
	return ret;
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
