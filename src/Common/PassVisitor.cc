//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PassVisitor.cc --
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 03 14:53:53 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "Common/PassVisitor.h"

PassVisitorStats pass_visitor_stats;
Stats::Counters::SimpleCounter* BaseSyntaxNode::new_nodes = nullptr;
