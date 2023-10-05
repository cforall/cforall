//
// Cforall Version 1.0.0 Copyright (C) 2021 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkOnce.h -- Translate the cfa_linkonce attribute.
//
// Author           : Andrew Beach
// Created On       : Thur May 13 10:06:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct  4 10:52:00 2023
// Update Count     : 1
//

#pragma once

// This could either be an early step in code-generation or a step of the
// Cforall to C lowering. It could also be part of attribute handling but
// for now its almost the only attribute we handle.

#include <list>  // for list

class Declaration;
namespace ast {
	class TranslationUnit;
}

namespace CodeGen {

void translateLinkOnce( std::list< Declaration *> & translationUnit );
void translateLinkOnce( ast::TranslationUnit & translationUnit );
/* Convert the cfa_linkonce attribute on top level declaration into
 * a special section declaration (.gnu.linkonce) so that it may be defined
 * multiple times (same name and same type, must have the same value).
 */

}
