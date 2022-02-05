//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MLEMutator.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  2 20:18:57 2022
// Update Count     : 227
//

// NOTE: There are two known subtle differences from the code that uC++ generates for the same input
//   -CFA puts the break label inside at the end of a switch, uC++ puts it after
//   -CFA puts the break label after a block, uC++ puts it inside at the end
// It is unclear if these differences are important, but if they are, then the fix would go in this file, since this is
// where these labels are generated.

#include <ext/alloc_traits.h>              // for __alloc_traits<>::value_type
#include <algorithm>                       // for find, find_if
#include <cassert>                         // for assert, assertf
#include <memory>                          // for allocator_traits<>::value_...

#include "Common/utility.h"                // for toString, operator+
#include "ControlStruct/LabelGenerator.h"  // for LabelGenerator
#include "MLEMutator.h"
#include "SynTree/Attribute.h"             // for Attribute
#include "SynTree/Expression.h"            // for Expression
#include "SynTree/Statement.h"             // for BranchStmt, CompoundStmt

namespace ControlStruct {
	MultiLevelExitMutator::~MultiLevelExitMutator() {
		delete targetTable;
		targetTable = 0;
	}
	namespace {
		bool isLoop( const MultiLevelExitMutator::Entry & e ) {
			return dynamic_cast< WhileDoStmt * >( e.get_controlStructure() )
				|| dynamic_cast< ForStmt * >( e.get_controlStructure() );
		}
		bool isSwitch( const MultiLevelExitMutator::Entry & e ) {
			return dynamic_cast< SwitchStmt *>( e.get_controlStructure() );
		}

		bool isBreakTarget( const MultiLevelExitMutator::Entry & e ) {
			return isLoop( e ) || isSwitch( e )
				|| dynamic_cast< CompoundStmt *>( e.get_controlStructure() );
		}
		bool isContinueTarget( const MultiLevelExitMutator::Entry & e ) {
			return isLoop( e );
		}
		bool isFallthroughTarget( const MultiLevelExitMutator::Entry & e ) {
			return dynamic_cast< CaseStmt *>( e.get_controlStructure() );
		}
		bool isFallthroughDefaultTarget( const MultiLevelExitMutator::Entry & e ) {
			return isSwitch( e );
		}
	} // namespace

	void MultiLevelExitMutator::premutate( FunctionDecl * ) {
		visit_children = false;
	}

	// break labels have to come after the statement they break out of, so mutate a statement, then if they inform us
	// through the breakLabel field tha they need a place to jump to on a break statement, add the break label to the
	// body of statements
	void MultiLevelExitMutator::fixBlock( std::list< Statement * > &kids, bool caseClause ) {
		SemanticErrorException errors;

		for ( std::list< Statement * >::iterator k = kids.begin(); k != kids.end(); k++ ) {
			if ( caseClause ) {
				// once a label is seen, it's no longer a valid fallthrough target
				for ( Label & l : (*k)->labels ) {
					fallthroughLabels.erase( l );
				}
			}

			// aggregate errors since the PassVisitor mutate loop was unrollled
			try {
				*k = (*k)->acceptMutator(*visitor);
			} catch( SemanticErrorException &e ) {
				errors.append( e );
			}

			if ( ! get_breakLabel().empty() ) {
				std::list< Statement * >::iterator next = k+1;
				std::list<Label> ls; ls.push_back( get_breakLabel() );
				kids.insert( next, new NullStmt( ls ) );
				set_breakLabel("");
			} // if
		} // for

		if ( ! errors.isEmpty() ) {
			throw errors;
		}
	}

	void MultiLevelExitMutator::premutate( CompoundStmt *cmpndStmt ) {
		visit_children = false;
		bool labeledBlock = !(cmpndStmt->labels.empty());
		if ( labeledBlock ) {
			Label brkLabel = generator->newLabel("blockBreak", cmpndStmt);
			enclosingControlStructures.push_back( Entry( cmpndStmt, brkLabel ) );
			GuardAction( [this]() { enclosingControlStructures.pop_back(); } );
		} // if

		// a child statement may set the break label - if they do, attach it to the next statement
		std::list< Statement * > &kids = cmpndStmt->kids;
		fixBlock( kids );

		if ( labeledBlock ) {
			assert( ! enclosingControlStructures.empty() );
			if ( ! enclosingControlStructures.back().useBreakExit().empty() ) {
				set_breakLabel( enclosingControlStructures.back().useBreakExit() );
			} // if
		} // if
	}


	void addUnused( Statement * stmt, const Label & originalTarget ) {
		// break/continue without a label doesn't need unused attribute
		if ( originalTarget == "" ) return;
		// add unused attribute to the originalTarget of a labelled break/continue
		for ( Label & l : stmt->get_labels() ) {
			// find the label to add unused attribute to
			if ( l == originalTarget ) {
				for ( Attribute * attr : l.get_attributes() ) {
					// ensure attribute isn't added twice
					if ( attr->get_name() == "unused" ) return;
				}
				l.get_attributes().push_back( new Attribute( "unused" ) );
				return;
			}
		}
		assertf( false, "CFA internal error: could not find label '%s' on statement %s",
			originalTarget.get_name().c_str(), toString( stmt ).c_str() );
	}


	Statement *MultiLevelExitMutator::postmutate( BranchStmt *branchStmt )
			throw ( SemanticErrorException ) {
		std::string originalTarget = branchStmt->originalTarget;

		std::list< Entry >::reverse_iterator targetEntry;
		switch ( branchStmt->get_type() ) {
			case BranchStmt::Goto:
				return branchStmt;
			case BranchStmt::Continue:
			case BranchStmt::Break: {
				bool isContinue = branchStmt->get_type() == BranchStmt::Continue;
				// unlabeled break/continue
				if ( branchStmt->get_target() == "" ) {
					if ( isContinue ) {
						// continue target is outermost loop
						targetEntry = std::find_if( enclosingControlStructures.rbegin(), enclosingControlStructures.rend(), isContinueTarget );
					} else {
						// break target is outermost loop, switch, or block control structure
						if ( enclosingControlStructures.empty() ) SemanticError( branchStmt->location, "'break' outside a loop, 'switch', or labelled block" );
						targetEntry = std::find_if( enclosingControlStructures.rbegin(), enclosingControlStructures.rend(), isBreakTarget );
					} // if
				} else {
					// labeled break/continue - lookup label in table to find attached control structure
					targetEntry = std::find( enclosingControlStructures.rbegin(), enclosingControlStructures.rend(), (*targetTable)[branchStmt->get_target()] );
				} // if
				// ensure that selected target is valid
				if ( targetEntry == enclosingControlStructures.rend() || (isContinue && ! isContinueTarget( *targetEntry ) ) ) {
					SemanticError( branchStmt->location, toString( (isContinue ? "'continue'" : "'break'"), " target must be an enclosing ", (isContinue ? "loop: " : "control structure: "), originalTarget ) );
				} // if
				break;
			}
			case BranchStmt::FallThrough:
				targetEntry = std::find_if( enclosingControlStructures.rbegin(), enclosingControlStructures.rend(), isFallthroughTarget );
				// ensure that selected target is valid
				if ( targetEntry == enclosingControlStructures.rend() ) {
					SemanticError( branchStmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
				} // if
				if ( branchStmt->get_target() != "" ) {
					// labelled fallthrough
					// target must be in the set of valid fallthrough labels
					if ( ! fallthroughLabels.count( branchStmt->get_target() ) ) {
						SemanticError( branchStmt->location, toString( "'fallthrough' target must be a later case statement: ", originalTarget ) );
					}
					return new BranchStmt( originalTarget, BranchStmt::Goto );
				}
				break;
			case BranchStmt::FallThroughDefault: {
				// fallthrough default
				targetEntry = std::find_if( enclosingControlStructures.rbegin(), enclosingControlStructures.rend(), isFallthroughDefaultTarget );

				// ensure that fallthrough is within a switch or choose
				if ( targetEntry == enclosingControlStructures.rend() ) {
					SemanticError( branchStmt->location, "'fallthrough' must be enclosed in a 'switch' or 'choose'" );
				} // if

				// ensure that switch or choose has a default clause
				SwitchStmt * switchStmt = strict_dynamic_cast< SwitchStmt * >( targetEntry->get_controlStructure() );
				bool foundDefault = false;
				for ( Statement * stmt : switchStmt->statements ) {
					CaseStmt * caseStmt = strict_dynamic_cast< CaseStmt * >( stmt );
					if ( caseStmt->isDefault() ) {
						foundDefault = true;
					} // if
				} // for
				if ( ! foundDefault ) {
					SemanticError( branchStmt->location, "'fallthrough default' must be enclosed in a 'switch' or 'choose' control structure with a 'default' clause" );
				}
				break;
			}

			default:
				assert( false );
		} // switch

		// branch error checks, get the appropriate label name and create a goto
		Label exitLabel;
		switch ( branchStmt->type ) {
		  case BranchStmt::Break:
				assert( targetEntry->useBreakExit() != "");
				exitLabel = targetEntry->useBreakExit();
				break;
		  case BranchStmt::Continue:
				assert( targetEntry->useContExit() != "");
				exitLabel = targetEntry->useContExit();
				break;
		  case BranchStmt::FallThrough:
				assert( targetEntry->useFallExit() != "");
				exitLabel = targetEntry->useFallExit();
				break;
		  case BranchStmt::FallThroughDefault:
				assert( targetEntry->useFallDefaultExit() != "");
				exitLabel = targetEntry->useFallDefaultExit();
				// check that fallthrough default comes before the default clause
				if ( ! targetEntry->isFallDefaultValid() ) {
					SemanticError( branchStmt->location, "'fallthrough default' must precede the 'default' clause" );
				}
				break;
		  default:
				assert(0);					// shouldn't be here
		} // switch

		// add unused attribute to label to silence warnings
		addUnused( targetEntry->get_controlStructure(), branchStmt->originalTarget );

		// transform break/continue statements into goto to simplify later handling of branches
		delete branchStmt;
		return new BranchStmt( exitLabel, BranchStmt::Goto );
	}

	Statement *MultiLevelExitMutator::mutateLoop( Statement *bodyLoop, Entry &e ) {
		// only generate these when needed
		if( !e.isContUsed() && !e.isBreakUsed() ) return bodyLoop;

		// ensure loop body is a block
		CompoundStmt * newBody = new CompoundStmt();
		newBody->get_kids().push_back( bodyLoop );

		if ( e.isContUsed() ) {
			// continue label goes in the body as the last statement
			std::list< Label > labels; labels.push_back( e.useContExit() );
			newBody->get_kids().push_back( new NullStmt( labels ) );
		} // if

		if ( e.isBreakUsed() ) {
			// break label goes after the loop -- it'll get set by the outer mutator if we do this
			set_breakLabel( e.useBreakExit() );
		} // if

		return newBody;
	}

	template< typename LoopClass >
	void MultiLevelExitMutator::prehandleLoopStmt( LoopClass * loopStmt ) {
		// remember this as the most recent enclosing loop, then mutate the body of the loop -- this will determine
		// whether brkLabel and contLabel are used with branch statements and will recursively do the same to nested
		// loops
		Label brkLabel = generator->newLabel("loopBreak", loopStmt);
		Label contLabel = generator->newLabel("loopContinue", loopStmt);
		enclosingControlStructures.push_back( Entry( loopStmt, brkLabel, contLabel ) );
		GuardAction( [this]() { enclosingControlStructures.pop_back(); } );
	}

	template< typename LoopClass >
	Statement * MultiLevelExitMutator::posthandleLoopStmt( LoopClass * loopStmt ) {
		assert( ! enclosingControlStructures.empty() );
		Entry &e = enclosingControlStructures.back();
		// sanity check that the enclosing loops have been popped correctly
		assert ( e == loopStmt );

		// this will take the necessary steps to add definitions of the previous two labels, if they are used.
		loopStmt->body = mutateLoop( loopStmt->get_body(), e );
		return loopStmt;
	}

	void MultiLevelExitMutator::premutate( WhileDoStmt * whileDoStmt ) {
		return prehandleLoopStmt( whileDoStmt );
	}

	void MultiLevelExitMutator::premutate( ForStmt * forStmt ) {
		return prehandleLoopStmt( forStmt );
	}

	Statement * MultiLevelExitMutator::postmutate( WhileDoStmt * whileDoStmt ) {
		return posthandleLoopStmt( whileDoStmt );
	}

	Statement * MultiLevelExitMutator::postmutate( ForStmt * forStmt ) {
		return posthandleLoopStmt( forStmt );
	}

	void MultiLevelExitMutator::premutate( IfStmt * ifStmt ) {
		// generate a label for breaking out of a labeled if
		bool labeledBlock = !(ifStmt->get_labels().empty());
		if ( labeledBlock ) {
			Label brkLabel = generator->newLabel("blockBreak", ifStmt);
			enclosingControlStructures.push_back( Entry( ifStmt, brkLabel ) );
			GuardAction( [this]() { enclosingControlStructures.pop_back(); } );
		} // if
	}

	Statement * MultiLevelExitMutator::postmutate( IfStmt * ifStmt ) {
		bool labeledBlock = !(ifStmt->get_labels().empty());
		if ( labeledBlock ) {
			if ( ! enclosingControlStructures.back().useBreakExit().empty() ) {
				set_breakLabel( enclosingControlStructures.back().useBreakExit() );
			} // if
		} // if
		return ifStmt;
	}

	void MultiLevelExitMutator::premutate( TryStmt * tryStmt ) {
		// generate a label for breaking out of a labeled if
		bool labeledBlock = !(tryStmt->get_labels().empty());
		if ( labeledBlock ) {
			Label brkLabel = generator->newLabel("blockBreak", tryStmt);
			enclosingControlStructures.push_back( Entry( tryStmt, brkLabel ) );
			GuardAction( [this]() { enclosingControlStructures.pop_back(); } );
		} // if
	}

	Statement * MultiLevelExitMutator::postmutate( TryStmt * tryStmt ) {
		bool labeledBlock = !(tryStmt->get_labels().empty());
		if ( labeledBlock ) {
			if ( ! enclosingControlStructures.back().useBreakExit().empty() ) {
				set_breakLabel( enclosingControlStructures.back().useBreakExit() );
			} // if
		} // if
		return tryStmt;
	}

	void MultiLevelExitMutator::premutate( FinallyStmt * ) {
		GuardAction([this, old = std::move(enclosingControlStructures)]() {
			enclosingControlStructures = std::move(old);
		});
		enclosingControlStructures = std::list<Entry>();
		GuardValue( inFinally );
		inFinally = true;
	}

	void MultiLevelExitMutator::premutate( ReturnStmt *returnStmt ) {
		if ( inFinally ) {
			SemanticError( returnStmt->location, "'return' may not appear in a finally clause" );
		}
	}

	void MultiLevelExitMutator::premutate( CaseStmt *caseStmt ) {
		visit_children = false;

		// mark default as seen before visiting its statements to catch default loops
		if ( caseStmt->isDefault() ) {
			enclosingControlStructures.back().seenDefault();
		} // if

		caseStmt->condition = maybeMutate( caseStmt->condition, *visitor );
		Label fallLabel = generator->newLabel( "fallThrough", caseStmt );
		{
			// ensure that stack isn't corrupted by exceptions in fixBlock
			auto guard = makeFuncGuard( [&]() { enclosingControlStructures.push_back( Entry( caseStmt, fallLabel ) ); }, [this]() { enclosingControlStructures.pop_back(); } );

			// empty case statement
			if( ! caseStmt->stmts.empty() ) {
				// the parser ensures that all statements in a case are grouped into a block
				CompoundStmt * block = strict_dynamic_cast< CompoundStmt * >( caseStmt->stmts.front() );
				fixBlock( block->kids, true );

				// add fallthrough label if necessary
				assert( ! enclosingControlStructures.empty() );
				if ( enclosingControlStructures.back().isFallUsed() ) {
					std::list<Label> ls{ enclosingControlStructures.back().useFallExit() };
					caseStmt->stmts.push_back( new NullStmt( ls ) );
				} // if
			} // if
		}
		assert( ! enclosingControlStructures.empty() );
		assertf( dynamic_cast<SwitchStmt *>( enclosingControlStructures.back().get_controlStructure() ),
				 "CFA internal error: control structure enclosing a case clause must be a switch, but is: %s",
				 toCString( enclosingControlStructures.back().get_controlStructure() ) );
		if ( caseStmt->isDefault() ) {
			if ( enclosingControlStructures.back().isFallDefaultUsed() ) {
				// add fallthrough default label if necessary
				std::list<Label> ls{ enclosingControlStructures.back().useFallDefaultExit() };
				caseStmt->stmts.push_front( new NullStmt( ls ) );
			} // if
		} // if
	}

	void MultiLevelExitMutator::premutate( SwitchStmt *switchStmt ) {
		// generate a label for breaking out of a labeled switch
		Label brkLabel = generator->newLabel("switchBreak", switchStmt);
		auto it = std::find_if( switchStmt->statements.rbegin(), switchStmt->statements.rend(), [](Statement * stmt) {
			CaseStmt * caseStmt = strict_dynamic_cast< CaseStmt * >( stmt );
			return caseStmt->isDefault();
		});
		CaseStmt * defaultCase = it != switchStmt->statements.rend() ? strict_dynamic_cast<CaseStmt *>( *it ) : nullptr;
		Label fallDefaultLabel = defaultCase ? generator->newLabel( "fallThroughDefault", defaultCase ) : "";
		enclosingControlStructures.push_back( Entry(switchStmt, brkLabel, fallDefaultLabel) );
		GuardAction( [this]() { enclosingControlStructures.pop_back(); } );

		// Collect valid labels for fallthrough. This is initially all labels at the same level as a case statement.
		// As labels are seen during traversal, they are removed, since fallthrough is not allowed to jump backwards.
		for ( Statement * stmt : switchStmt->statements ) {
			CaseStmt * caseStmt = strict_dynamic_cast< CaseStmt * >( stmt );
			if ( caseStmt->stmts.empty() ) continue;
			CompoundStmt * block = dynamic_cast< CompoundStmt * >( caseStmt->stmts.front() );
			for ( Statement * stmt : block->kids ) {
				for ( Label & l : stmt->labels ) {
					fallthroughLabels.insert( l );
				}
			}
		}
	}

	Statement * MultiLevelExitMutator::postmutate( SwitchStmt * switchStmt ) {
		Entry &e = enclosingControlStructures.back();
		assert ( e == switchStmt );

		// only generate break label if labeled break is used
		if ( e.isBreakUsed() ) {
			// for the purposes of keeping switch statements uniform (i.e. all statements that are direct children of a
			// switch should be CastStmts), append the exit label + break to the last case statement; create a default
			// case if there are no cases
			std::list< Statement * > &statements = switchStmt->statements;
			if ( statements.empty() ) {
				statements.push_back( CaseStmt::makeDefault() );
			} // if

			if ( CaseStmt * c = dynamic_cast< CaseStmt * >( statements.back() ) ) {
				Statement * stmt = new BranchStmt( Label("brkLabel"), BranchStmt::Break );
				stmt->labels.push_back( e.useBreakExit() );
				c->stmts.push_back( stmt );
			} else assert(0); // as of this point, all statements of a switch are still CaseStmts
		} // if

		assert ( enclosingControlStructures.back() == switchStmt );
		return switchStmt;
	}
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
