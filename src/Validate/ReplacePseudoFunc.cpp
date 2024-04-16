#include "ReplacePseudoFunc.hpp"

#include <set>

#include "AST/Decl.hpp"
#include "AST/Inspect.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Stmt.hpp"
#include "Common/utility.h"
#include "ResolvExpr/CandidateFinder.hpp"
#include "ResolvExpr/Resolver.h"
#include "SymTab/Mangler.h"

namespace Validate {

namespace {

ast::ptr<ast::Expr> reduceCastExpr(ast::ptr<ast::Expr> expr) {
    if (auto castExpr = expr.as<ast::CastExpr>()) {
        return reduceCastExpr(castExpr->arg);
    }
    return expr;
}

struct ReplaceSuccAndPred final : public ast::WithSymbolTable,
                                  public ast::WithConstTranslationUnit {
    const ast::Expr* postvisit(const ast::ApplicationExpr* expr) {
        auto fname = ast::getFunctionName(expr);
        if (fname == "succ" || fname == "pred") {
            const CodeLocation& location = expr->location;
            if (expr->args.size() != 1) return expr;

            auto param = expr->args.front();
            if (auto argAsVar = reduceCastExpr(param).as<ast::VariableExpr>()) {
                if (auto argAsDecl = argAsVar->var.as<ast::ObjectDecl>()) {
                    if (auto enumInst =
                            argAsDecl->type.as<ast::EnumInstType>()) {
                        auto castTo = new ast::EnumAttrType(
                            enumInst, ast::EnumAttribute::Posn);
                        auto castExpr =
                            new ast::CastExpr(param->location, param, castTo);

                        auto untyped = new ast::UntypedExpr(
                            expr->location,
                            new ast::NameExpr(location, fname == "succ"
                                                            ? "_successor_"
                                                            : "_predessor_"),
                            {castExpr});
                        ResolvExpr::ResolveContext context{symtab,
                                                           transUnit().global};
                        auto typedResult =
                            ResolvExpr::findVoidExpression(untyped, context);
                        ast::ptr<ast::ApplicationExpr> ret =
                            typedResult.strict_as<ast::ApplicationExpr>();
                        return ast::deepCopy(ret);
                    } else if (argAsDecl->type.as<ast::EnumAttrType>()) {
                        std::cerr << "PseudoFunc: succ/pred should not be applied on EnumAttrType directly" << std::endl;
                    }
                }
            }
        }
        return expr;
    }
};

}  // namespace

void replacePseudoFunc(ast::TranslationUnit& translationUnit) {
    ast::Pass<ReplaceSuccAndPred>::run(translationUnit);
}
}  // namespace Validate