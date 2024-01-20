#pragma once

namespace ast {
    class TranslationUnit;
}

namespace Validate {

void replacePseudoFunc( ast::TranslationUnit & translationUnit );
}