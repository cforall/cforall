#include "ReplacePseudoFunc.hpp"

#include <set>

#include "AST/Decl.hpp"
#include "AST/Inspect.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "Common/utility.h"
#include "ResolvExpr/Resolver.h"
#include "SymTab/Mangler.h"  // for Mangler
namespace Validate {

namespace {

std::set<std::string> queryLabels;
std::set<std::string> queryValues;

struct FindGenEnumArray final : public ast::WithShortCircuiting {
    void previsit(const ast::ApplicationExpr* enumDecl);
};

void FindGenEnumArray::previsit(const ast::ApplicationExpr* expr) {
    auto fname = ast::getFunctionName(expr);
    if (fname == "labelE" || fname == "valueE") {
        if (expr->args.size() != 1) {
            SemanticError(expr, "Position Expression only take one parameter");
        }
        const ast::VariableExpr* arg =
            expr->args.front().as<const ast::VariableExpr>();
        if (!arg) {
            SemanticError(expr, "Unimplement Pseudo Function Cases");
        }
        const ast::ObjectDecl* argAsVar = arg->var.as<const ast::ObjectDecl>();
        const std::string referredName = argAsVar->name;
        const ast::EnumInstType* argType =
            argAsVar->type.as<const ast::EnumInstType>();
        if (!argType) {
            SemanticError(
                argAsVar,
                "Position can only be used on an enumeration instance");
        }
        ast::ptr<ast::EnumDecl> base = argType->base;
        assert(base);
        if (fname == "labelE") queryLabels.insert(base->name);
        if (fname == "valueE") queryValues.insert(base->name);
    }
}

struct PseudoFuncGenerateRoutine final : public ast::WithDeclsToAdd<>,
                                         public ast::WithSymbolTable,
                                         public ast::WithShortCircuiting {
    void previsit(const ast::EnumDecl* enumDecl);
};

void PseudoFuncGenerateRoutine::previsit(const ast::EnumDecl* enumDecl) {
    visit_children = false;
    const CodeLocation& location = enumDecl->location;
    if (enumDecl->members.size() == 0 || !enumDecl->base) return;

    std::vector<ast::ptr<ast::Init>> inits;
    std::vector<ast::ptr<ast::Init>> labels;
    for (const ast::Decl* mem : enumDecl->members) {
        auto memAsObjectDecl = dynamic_cast<const ast::ObjectDecl*>(mem);
        inits.emplace_back(memAsObjectDecl->init);
        labels.emplace_back(new ast::SingleInit(
            location, ast::ConstantExpr::from_string(location, mem->name)));
    }
    if (queryValues.count(enumDecl->name)) {
        auto init = new ast::ListInit(location, std::move(inits));
        auto values = new ast::ObjectDecl(
            location, "values_" + enumDecl->name,
            new ast::ArrayType(
                enumDecl->base,
                ast::ConstantExpr::from_int(location, enumDecl->members.size()),
                ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim),
            init, ast::Storage::Static, ast::Linkage::AutoGen);
        symtab.addId(values);
        values->mangleName = Mangle::mangle(values);
        declsToAddAfter.push_back(values);
    }
    if (queryLabels.count(enumDecl->name)) {
        auto label_strings = new ast::ListInit(location, std::move(labels));
        auto label_arr = new ast::ObjectDecl(
            location, "labels_" + enumDecl->name,
            new ast::ArrayType(
                new ast::PointerType(new ast::BasicType{ast::BasicType::Char}),
                ast::ConstantExpr::from_int(location, enumDecl->members.size()),
                ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim),
            label_strings, ast::Storage::Static, ast::Linkage::AutoGen);
        symtab.addId(label_arr);
        label_arr->mangleName = Mangle::mangle(label_arr);
        declsToAddAfter.push_back(label_arr);
    }
}

struct ReplacePseudoFuncCore : public ast::WithShortCircuiting,
                               public ast::WithSymbolTable,
                               public ast::WithConstTranslationUnit {
    ast::Expr const* postvisit(ast::ApplicationExpr const* decl);
};

ast::Expr const* ReplacePseudoFuncCore::postvisit(
    ast::ApplicationExpr const* expr) {
    auto fname = ast::getFunctionName(expr);
    auto location = expr->location;
    if (fname == "posE" || fname == "valueE" || fname == "labelE") {
        if (expr->args.size() != 1) {
            SemanticError(expr,
                          "Pseudo Enum Expression only take one parameter");
        }
        ast::ptr<ast::VariableExpr> arg =
            expr->args.front().as<const ast::VariableExpr>();
        if (!arg) {
            SemanticError(expr, "Unimplement Pseudo Function Cases");
        }
        const ast::ObjectDecl* argAsVar = arg->var.as<const ast::ObjectDecl>();
        const std::string referredName = argAsVar->name;
        const ast::EnumInstType* argType =
            argAsVar->type.as<const ast::EnumInstType>();
        if (!argType) {
            SemanticError(argAsVar,
                          "Pseudo Enum Expression can only be used on an "
                          "enumeration instance");
        }
        const ast::EnumDecl* base = argType->base;
        for (size_t i = 0; i < base->members.size(); i++) {
            if (base->members[i]->name == referredName) {
                if (fname == "posE")
                    return ast::ConstantExpr::from_int(expr->location, i);
                else if (fname == "labelE")
                    return ast::ConstantExpr::from_string(expr->location,
                                                          referredName);
                else
                    return new ast::TypeExpr(expr->location, argType);
            }
        }

        ResolvExpr::ResolveContext context{symtab, transUnit().global};

        if (fname == "labelE") {
            ast::Expr* toResolve =
                new ast::NameExpr(expr->location, "labels_" + base->name);
            auto result = ResolvExpr::findVoidExpression(toResolve, context);
            if (result.get()) {
                auto arrAsVar = result.strict_as<ast::VariableExpr>();
                auto untyped = new ast::UntypedExpr(
                    location, new ast::NameExpr(location, "?[?]"),
                    {new ast::VariableExpr(*arrAsVar),
                     ast::ConstantExpr::from_int(
                         location,
                         0)});  /// TODO: dummy value.
                                /// To make it works need to change the unifier

                auto typedResult =
                    ResolvExpr::findVoidExpression(untyped, context);
                if (result.get()) {
                    ast::ptr<ast::ApplicationExpr> ret =
                        typedResult.strict_as<ast::ApplicationExpr>();
                    return new ast::ApplicationExpr(*ret);
                }
            }
        }
        
        if (fname == "valueE") {
            ast::Expr* toResolve =
                new ast::NameExpr(expr->location, "values_" + base->name);
            auto result = ResolvExpr::findVoidExpression(toResolve, context);
            if (result.get()) {
                auto arrAsVar = result.strict_as<ast::VariableExpr>();
                auto untyped = new ast::UntypedExpr(
                    location, new ast::NameExpr(location, "?[?]"),
                    {new ast::VariableExpr(*arrAsVar),
                     ast::ConstantExpr::from_int(
                         location,
                         0)});  /// TODO: dummy value.
                                /// To make it works need to change the unifier

                auto typedResult =
                    ResolvExpr::findVoidExpression(untyped, context);
                if (result.get()) {
                    ast::ptr<ast::ApplicationExpr> ret =
                        typedResult.strict_as<ast::ApplicationExpr>();
                    return new ast::ApplicationExpr(*ret);
                }
            }
        }
    }
    return expr;
}

}  // namespace

void replacePseudoFunc(ast::TranslationUnit& translationUnit) {
    ast::Pass<FindGenEnumArray>::run(translationUnit);
    ast::Pass<PseudoFuncGenerateRoutine>::run(translationUnit);
    ast::Pass<ReplacePseudoFuncCore>::run(translationUnit);
}
}  // namespace Validate