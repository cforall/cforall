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

std::set<std::string> queryLabels;
std::set<std::string> queryValues;

struct ReplaceEnumInstWithPos final : public ast::WithShortCircuiting {
    const ast::ObjectDecl* postvisit(const ast::ObjectDecl* decl) {
        auto enumInst = decl->type.strict_as<ast::EnumInstType>();
        auto enumPos = new ast::EnumPosType(enumInst);
        auto ret = ast::mutate_field(decl, &ast::ObjectDecl::type, enumPos);
        ret = ast::mutate_field(ret, &ast::ObjectDecl::mangleName,
                                Mangle::mangle(ret));
        return ret;
    }
};

const inline std::string getValueArrayName(std::string enumName) {
    return "values_" + enumName;
}

// struct AutoInit {
//     ast::EnumDecl const* postvisit( const ast::EnumDecl* expr );
// };

struct WrapEnumValueExpr final : public ast::WithShortCircuiting,
                                 public ast::WithSymbolTable,
                                 public ast::WithConstTranslationUnit {
    void previsit(const ast::DeclStmt* expr);
    void previsit(const ast::ApplicationExpr* expr);
    void previsit(const ast::CastExpr* expr);
    void previsit(const ast::VariableExpr*) { visit_children = false; }

    ast::Expr const* postvisit(const ast::VariableExpr* expr);
};

struct FindGenEnumArray final : public ast::WithShortCircuiting {
    void previsit(const ast::ApplicationExpr* enumDecl);
};

struct PseudoFuncGenerateRoutine final : public ast::WithDeclsToAdd<>,
                                         public ast::WithSymbolTable,
                                         public ast::WithShortCircuiting,
                                         public ast::WithConstTranslationUnit {
    void previsit(const ast::EnumDecl* enumDecl);
};

struct ReplacePseudoFuncCore : public ast::WithShortCircuiting,
                               public ast::WithSymbolTable,
                               public ast::WithConstTranslationUnit {
    ast::Expr const* postvisit(ast::ApplicationExpr const* decl);
};

// ast::EnumDecl const * AutoInit::postvisit( const ast::EnumDecl * expr ) {
//     for ( size_t i = 0; i < expr->members.size(); i++ ) {
//         auto mem = expr->members[i].as<ast::ObjectDecl>();
//         assert( mem );
//         if ( mem->init )
//     }
//     return expr;
// }

void WrapEnumValueExpr::previsit(const ast::ApplicationExpr* expr) {
    auto varExpr = expr->func.as<ast::VariableExpr>();
    auto fname = ast::getFunctionName(expr);
    if (!varExpr || varExpr->var->linkage == ast::Linkage::Intrinsic) {
        if (fname == "?{}" || fname == "?=?") visit_children = false;
    }

    if (fname == "labelE" || fname == "valueE" || fname == "posE" ||
        fname == "pred" || fname == "succ") {
        visit_children = false;
    }
}

void WrapEnumValueExpr::previsit(const ast::DeclStmt*) {
    visit_children = false;
}

void WrapEnumValueExpr::previsit(const ast::CastExpr* expr) {
    if (expr->result && expr->result.as<ast::ReferenceType>()) {
        visit_children = false;
    }
}

ast::Expr const* WrapEnumValueExpr::postvisit(const ast::VariableExpr* expr) {
    if (!expr->result) {
        return expr;
    }
    if (auto enumInst = expr->result.as<ast::EnumInstType>()) {
        if (enumInst->base && enumInst->base->base) {
            auto untyped = new ast::UntypedExpr(
                expr->location, new ast::NameExpr(expr->location, "valueE"),
                {std::move(expr)});
            ResolvExpr::ResolveContext context{symtab, transUnit().global};
            auto result = ResolvExpr::findVoidExpression(untyped, context);
            ast::ptr<ast::ApplicationExpr> ret =
                result.strict_as<ast::ApplicationExpr>();
            return ast::deepCopy(ret);
        }
    }
    return expr;
}

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

const ast::Init* getAutoInit(const CodeLocation& location,
                             const ast::Type* type,
                             ResolvExpr::ResolveContext context,
                             const ast::Init* prev) {
    if (auto prevInit = dynamic_cast<const ast::SingleInit*>(prev)) {
        auto prevInitExpr = prevInit->value;
        if (auto constInit = prevInitExpr.as<ast::ConstantExpr>()) {
            // Assume no string literal for now
            return new ast::SingleInit(
                location, ast::ConstantExpr::from_int(
                              location, constInit->intValue() + 1));
        } else {
            auto untypedThisInit = new ast::UntypedExpr(
                location, new ast::NameExpr(location, "?++"), {prevInitExpr});
            auto typedInit = ResolvExpr::findSingleExpression(untypedThisInit,
                                                              type, context);
            return new ast::SingleInit(location, typedInit);
        }
    }
    SemanticError(prev, "Auto Init a List is not implemented");
    return prev;
}

void PseudoFuncGenerateRoutine::previsit(const ast::EnumDecl* enumDecl) {
    visit_children = false;
    const CodeLocation& location = enumDecl->location;
    if (enumDecl->members.size() == 0 || !enumDecl->base) return;

    std::vector<ast::ptr<ast::Init>> inits;
    std::vector<ast::ptr<ast::Init>> labels;
    auto type = enumDecl->base;

    for (size_t i = 0; i < enumDecl->members.size(); i++) {
        ast::ptr<ast::Decl> mem = enumDecl->members.at(i);
        auto memAsObjectDecl = mem.as<ast::ObjectDecl>();
        assert(memAsObjectDecl);
        if (memAsObjectDecl->init) {
            inits.emplace_back(memAsObjectDecl->init);
        } else {
            const CodeLocation& location = mem->location;
            if (i == 0) {
                inits.emplace_back(new ast::SingleInit(
                    location, ast::ConstantExpr::from_int(mem->location, 0)));
            } else {
                inits.emplace_back(getAutoInit(
                    location, enumDecl->base,
                    ResolvExpr::ResolveContext{symtab, transUnit().global},
                    inits.at(i - 1).as<ast::SingleInit>()));
            }
        }
        labels.emplace_back(new ast::SingleInit(
            location, ast::ConstantExpr::from_string(location, mem->name)));
    }
    if (queryValues.count(enumDecl->name)) {
        auto init = new ast::ListInit(location, std::move(inits));
        const ast::ArrayType* arrT = new ast::ArrayType(
            enumDecl->base,
            ast::ConstantExpr::from_int(location, enumDecl->members.size()),
            ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim);
        ast::ObjectDecl* values = new ast::ObjectDecl(
            location, "values_" + enumDecl->name, arrT, init,
            ast::Storage::Static, ast::Linkage::AutoGen);
        symtab.addId(values);
        values->mangleName = Mangle::mangle(values);
        declsToAddAfter.push_back(values);
    }
    if (queryLabels.count(enumDecl->name)) {
        auto label_strings = new ast::ListInit(location, std::move(labels));
        auto labels = new ast::ObjectDecl(
            location, "labels_" + enumDecl->name,
            new ast::ArrayType(
                new ast::PointerType(new ast::BasicType{ast::BasicType::Char}),
                ast::ConstantExpr::from_int(location, enumDecl->members.size()),
                ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim),
            label_strings, ast::Storage::Static, ast::Linkage::AutoGen);
        symtab.addId(labels);
        labels->mangleName = Mangle::mangle(labels);
        declsToAddAfter.push_back(labels);
    }
}

ast::ApplicationExpr const* resolveAttributeFunctions(
    const CodeLocation location, ResolvExpr::ResolveContext context,
    const ast::VariableExpr* arg, const ast::EnumDecl* base,
    const std::string& name) {
    ast::Expr* toResolve = new ast::NameExpr(location, name + base->name);
    // Find the request arrary
    auto arr = ResolvExpr::findVoidExpression(toResolve, context);
    assert(arr.get());
    auto arrAsVar = arr.strict_as<ast::VariableExpr>();
    // change EnumInstType to EnumPosType to avoid recursive resolution
    auto argAsDecl = arg->var.as<ast::ObjectDecl>();
    if (argAsDecl->type.as<ast::EnumInstType>()) {
        ast::Pass<ReplaceEnumInstWithPos> replacer;
        auto rep = argAsDecl->accept(replacer);
        auto mutatedArg = ast::mutate_field(arg, &ast::VariableExpr::var, rep);
        mutatedArg = ast::mutate_field(mutatedArg, &ast::VariableExpr::result,
                                       mutatedArg->var->get_type());
        auto untyped =
            new ast::UntypedExpr(location, new ast::NameExpr(location, "?[?]"),
                                 {std::move(arrAsVar), mutatedArg});
        auto typedResult = ResolvExpr::findVoidExpression(untyped, context);
        ast::ptr<ast::ApplicationExpr> ret =
            typedResult.strict_as<ast::ApplicationExpr>();
        return ast::deepCopy(ret);
    } else {
        auto untyped =
            new ast::UntypedExpr(location, new ast::NameExpr(location, "?[?]"),
                                 {std::move(arrAsVar), arg});
        auto typedResult = ResolvExpr::findVoidExpression(untyped, context);

        ast::ptr<ast::ApplicationExpr> ret =
            typedResult.strict_as<ast::ApplicationExpr>();
        return ast::deepCopy(ret);
    }
}

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

        if (const ast::EnumInstType* argTypeAsEnumInst =
                argAsVar->type.as<const ast::EnumInstType>()) {
            const ast::EnumDecl* base = argTypeAsEnumInst->base;
            ResolvExpr::ResolveContext context{symtab, transUnit().global};
            // If resolvable as constant
            for (size_t i = 0; i < base->members.size(); i++) {
                if (base->members[i]->name == referredName) {
                    if (fname == "posE")
                        return ast::ConstantExpr::from_int(expr->location, i);
                    else if (fname == "labelE")
                        return ast::ConstantExpr::from_string(expr->location,
                                                              referredName);
                    else {
                        return resolveAttributeFunctions(
                            location, context, arg.get(), base, "values_");
                    }
                }
            }

            if (fname == "labelE") {
                if (auto labelExpr = resolveAttributeFunctions(
                        location, context, arg.get(), base, "labels_")) {
                    return labelExpr;
                }
            } else if (fname == "valueE") {
                if (auto valueExpr = resolveAttributeFunctions(
                        location, context, arg.get(), base, "values_")) {
                    return valueExpr;
                }
            } else {  // it is position; replace itself
                return std::move(arg.get());
            }
        } else if (const ast::EnumPosType* argTypeAsPos =
                       argAsVar->type.as<const ast::EnumPosType>()) {
            const ast::EnumDecl* base = argTypeAsPos->instance->base;
            ResolvExpr::ResolveContext context{symtab, transUnit().global};
            if (fname == "labelE") {
                if (auto labelExpr = resolveAttributeFunctions(
                        location, context, arg.get(), base, "labels_")) {
                    return labelExpr;
                }
            } else if (fname == "valueE") {
                if (auto valueExpr = resolveAttributeFunctions(
                        location, context, arg.get(), base, "values_")) {
                    return valueExpr;
                }
            } else {  // it is position; replace itself
                return std::move(arg.get());
            }
        } else {
            SemanticError(argAsVar,
                          "Pseudo Enum Expression can only be used on an "
                          "enumeration instance");
        }
    }
    return expr;
}

ast::ptr<ast::Expr> reduceCastExpr(ast::ptr<ast::Expr> expr) {
    if (auto castExpr = expr.as<ast::CastExpr>()) {
        return reduceCastExpr(castExpr->arg);
    }
    return expr;
}

struct ReplaceEnumInst final {
    const ast::Expr* postvisit(const ast::ApplicationExpr* expr) {
        auto fname = ast::getFunctionName(expr);
        if (fname == "?[?]") {
            if (expr->args.size() != 2) return expr;

            auto arg1AsVar =
                reduceCastExpr(expr->args.front()).as<ast::VariableExpr>();
            auto arg2AsVar =
                reduceCastExpr(expr->args.back()).as<ast::VariableExpr>();

            if (!arg1AsVar || !arg2AsVar) return expr;

            auto arg1AsDecl = arg1AsVar->var.as<ast::ObjectDecl>();
            auto arg2AsDecl = arg2AsVar->var.as<ast::ObjectDecl>();

            if (!arg1AsDecl || !arg2AsDecl) return expr;
            auto arrInst = arg1AsDecl->type.as<ast::ArrayType>();
            auto pointerInst = arg1AsDecl->type.as<ast::PointerType>();
            if (!arrInst && !pointerInst) {
                return expr;
            }
            auto enumInst = arg2AsDecl->type.as<ast::EnumInstType>();
            if (!enumInst) return expr;

            const std::string arrName = arg1AsDecl->name;
            if (arrName != getValueArrayName(enumInst->base->name)) return expr;
            ast::Pass<ReplaceEnumInstWithPos> replacer;
            auto rep = arg2AsDecl->accept(replacer);
            if (!rep) return expr;
            auto mutObj =
                ast::mutate_field(arg2AsVar, &ast::VariableExpr::var, rep);
            mutObj = ast::mutate_field(mutObj, &ast::VariableExpr::result,
                                       mutObj->var->get_type());
            auto mut = ast::mutate_field_index(
                expr, &ast::ApplicationExpr::args, 1, mutObj);
            return mut;
        }
        // else if (fname == "succ" || fname == "pred") {
        //     if (expr->args.size() != 1) return expr;
        //     auto argExpr = expr->args.front();
        //     auto argAsVar = reduceCastExpr(argExpr).as<ast::VariableExpr>();

        //     if (auto argAsDecl = argAsVar->var.as<ast::ObjectDecl>()) {
        //         if (auto enumInst = argAsDecl->type.as<ast::EnumInstType>())
        //         {
        //             auto enumPos = new ast::EnumPosType(enumInst);
        //             auto castExpr =
        //                 new ast::CastExpr(argExpr->location, argExpr,
        //                 enumPos);
        //             auto mut = ast::mutate_field_index(
        //                 expr, &ast::ApplicationExpr::args, 0, castExpr);
        //             return mut;
        //         } else if (auto enumPos =
        //                        argAsDecl->type.as<ast::EnumPosType>()) {
        //             //     std::cout << "pos" << std::endl;
        //             return expr;
        //         }
        //     }
        // }
        return expr;
    }
};

struct ReplaceSuccAndPred final : public ast::WithSymbolTable,
                                  public ast::WithConstTranslationUnit {
    const ast::Expr* postvisit(const ast::ApplicationExpr* expr) {
        auto fname = ast::getFunctionName(expr);
        if (fname == "succ" || fname == "pred") {
            const CodeLocation& location = expr->location;
            if (expr->args.size() != 1) return expr;

            // if (auto argAsVar = reduceCastExpr(expr->args.front())
            //                         .as<ast::VariableExpr>()) {
            //     if (auto argAsDecl = argAsVar->var.as<ast::ObjectDecl>()) {
            //         auto enumPos = argAsDecl->type.as<ast::EnumPosType>();
            //         if (!enumPos) return expr;
            //         // ast::Pass<ReplaceEnumInstWithPos> replacer;
            //         // auto posObj = argAsDecl->accept(replacer);
            //         // if (!posObj) return expr;

            //         // auto newParam = new ast::VariableExpr( location,
            //         posObj
            //         // );

            //         auto untyped = new ast::UntypedExpr(
            //             location,
            //             new ast::NameExpr(location, fname == "succ"
            //                                             ? "_successor_"
            //                                             : "_predessor_"),
            //             {argAsVar});

            //         ResolvExpr::ResolveContext context{symtab,
            //                                            transUnit().global};

            //         auto typedResult =
            //             ResolvExpr::findVoidExpression(untyped, context);

            //         ast::ptr<ast::ApplicationExpr> ret =
            //             typedResult.strict_as<ast::ApplicationExpr>();

            //         return ast::deepCopy(ret);
            //     }
            // }
            auto param = expr->args.front();
            if (auto argAsVar = reduceCastExpr(param).as<ast::VariableExpr>()) {
                if (auto argAsDecl = argAsVar->var.as<ast::ObjectDecl>()) {
                    if (auto enumInst =
                            argAsDecl->type.as<ast::EnumInstType>()) {
                        auto castTo = new ast::EnumPosType(enumInst);
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
                    } else if (auto posType =
                                   argAsDecl->type.as<ast::EnumPosType>()) {
                        // Very nasty fix. Must be revisit
                        if (auto paramAsVar = param.as<ast::VariableExpr>()) {
                            if (paramAsVar->result.as<ast::EnumInstType>()) {
                                auto paramToUse = ast::mutate_field(
                                    paramAsVar, &ast::VariableExpr::result,
                                    posType);
                                auto untyped = new ast::UntypedExpr(
                                    expr->location,
                                    new ast::NameExpr(location,
                                                      fname == "succ"
                                                          ? "_successor_"
                                                          : "_predessor_"),
                                    {paramToUse});
                                ResolvExpr::ResolveContext context{
                                    symtab, transUnit().global};
                                auto typedResult =
                                    ResolvExpr::findVoidExpression(untyped,
                                                                   context);
                                ast::ptr<ast::ApplicationExpr> ret =
                                    typedResult
                                        .strict_as<ast::ApplicationExpr>();
                                return ast::deepCopy(ret);
                            }
                        }
                        auto untyped = new ast::UntypedExpr(
                            expr->location,
                            new ast::NameExpr(location, fname == "succ"
                                                            ? "_successor_"
                                                            : "_predessor_"),
                            {param});
                        ResolvExpr::ResolveContext context{symtab,
                                                           transUnit().global};
                        auto typedResult =
                            ResolvExpr::findVoidExpression(untyped, context);
                        ast::ptr<ast::ApplicationExpr> ret =
                            typedResult.strict_as<ast::ApplicationExpr>();
                        return ast::deepCopy(ret);
                    }
                }
            }
        }
        return expr;
    }
};

}  // namespace

void replacePseudoFunc(ast::TranslationUnit& translationUnit) {
    ast::Pass<WrapEnumValueExpr>::run(translationUnit);
    ast::Pass<FindGenEnumArray>::run(translationUnit);

    ast::Pass<PseudoFuncGenerateRoutine>::run(translationUnit);
    ast::Pass<ReplacePseudoFuncCore>::run(translationUnit);
    ast::Pass<ReplaceEnumInst>::run(translationUnit);

    ast::Pass<ReplaceSuccAndPred>::run(translationUnit);
}
}  // namespace Validate