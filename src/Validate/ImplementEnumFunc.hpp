#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Auto-Generation of enumeration functions.
/// Happens before fixReturnStatements because it inserts "unfixed" returns.
void implementEnumFunc( ast::TranslationUnit & translationUnit );

}
