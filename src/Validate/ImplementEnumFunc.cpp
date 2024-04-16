#include "AST/Create.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "CodeGen/OperatorTable.h"  // for isCtorDtor, isCtorDtorAssign
#include "InitTweak/InitTweak.h"    // for isAssignment, isCopyConstructor
namespace Validate {

namespace {
class EnumAttrFuncGenerator {
    const ast::EnumDecl* decl;
    const ast::EnumInstType* instType;
    // const ast::EnumAttrType* attrType;
    unsigned int functionNesting;
    ast::Linkage::Spec proto_linkage;

   public:
    std::list<ast::ptr<ast::Decl>> forwards;
    std::list<ast::ptr<ast::Decl>> definitions;

    void generateAndAppendFunctions(std::list<ast::ptr<ast::Decl>>&);

    EnumAttrFuncGenerator(const ast::EnumDecl* decl,
                          const ast::EnumInstType* instType,
                          // const ast::EnumAttrType* enumAttrType,
                          unsigned int functionNesting)
        : decl(decl),
          instType{instType},
          // attrType{enumAttrType},
          functionNesting{functionNesting},
          proto_linkage{ast::Linkage::Cforall} {}

    void genAttrFunctions();
    void genSuccPredPosn();
    void genSuccPredDecl();

    void appendReturnThis(ast::FunctionDecl* decl) {
        assert(1 <= decl->params.size());
        assert(1 == decl->returns.size());
        assert(decl->stmts);

        const CodeLocation& location = (decl->stmts->kids.empty())
                                           ? decl->stmts->location
                                           : decl->stmts->kids.back()->location;
        const ast::DeclWithType* thisParam = decl->params.front();
        decl->stmts.get_and_mutate()->push_back(new ast::ReturnStmt(
            location, new ast::VariableExpr(location, thisParam)));
    }
    void genAttrStandardFuncs() {
        ast::FunctionDecl* (EnumAttrFuncGenerator::*standardProtos[4])()
            const = {&EnumAttrFuncGenerator::genCtorProto,
                     &EnumAttrFuncGenerator::genCopyProto,
                     &EnumAttrFuncGenerator::genDtorProto,
                     &EnumAttrFuncGenerator::genAssignProto};
        for (auto& generator : standardProtos) {
            ast::FunctionDecl* decl = (this->*generator)();
            produceForwardDecl(decl);
            genFuncBody(decl);
            if (CodeGen::isAssignment(decl->name)) {
                appendReturnThis(decl);
            }
            produceDecl(decl);
        }
    }

   private:
    const CodeLocation& getLocation() const { return decl->location; }

    ast::FunctionDecl* genProto(
        std::string&& name, std::vector<ast::ptr<ast::DeclWithType>>&& params,
        std::vector<ast::ptr<ast::DeclWithType>>&& returns) const;

    void produceDecl(const ast::FunctionDecl* decl);
    void produceForwardDecl(const ast::FunctionDecl* decl);

    const ast::Decl* getDecl() const { return decl; }

    ast::FunctionDecl* genPosnProto() const;
    ast::FunctionDecl* genLabelProto() const;
    ast::FunctionDecl* genValueProto() const;
    ast::FunctionDecl* genSuccProto() const;
    ast::FunctionDecl* genPredProto() const;

    ast::FunctionDecl* genSuccPosProto() const;
    ast::FunctionDecl* genPredPosProto() const;

    // ---------------------------------------------------
    // ast::FunctionDecl* genAttrCtorProto() const;
    /// Changes the node inside a pointer so that it has the unused attribute.
    void addUnusedAttribute(ast::ptr<ast::DeclWithType>& declPtr) {
        ast::DeclWithType* decl = declPtr.get_and_mutate();
        decl->attributes.push_back(new ast::Attribute("unused"));
    }

    ast::ObjectDecl* dstParam() const {
        return new ast::ObjectDecl(getLocation(), "_dst",
                                   new ast::ReferenceType(new ast::EnumAttrType(
                                       ast::deepCopy(instType))));
    }

    ast::ObjectDecl* srcParam() const {
        return new ast::ObjectDecl(
            getLocation(), "_src",
            new ast::EnumAttrType(ast::deepCopy(instType)));
    }

    /// E = EnumAttrType<T>`
    /// `void ?{}(E & _dst)`.
    ast::FunctionDecl* genCtorProto() const {
        return genProto("?{}", {dstParam()}, {});
    }

    /// void ?{}(E & _dst, E _src)`.
    ast::FunctionDecl* genCopyProto() const {
        return genProto("?{}", {dstParam(), srcParam()}, {});
    }

    ///`void ^?{}(E & _dst)`.
    ast::FunctionDecl* genDtorProto() const {
        // The destructor must be mutex on a concurrent type.
        return genProto("^?{}", {dstParam()}, {});
    }

    /// `E ?{}(E & _dst, E _src)`.
    ast::FunctionDecl* genAssignProto() const {
        // Only the name is different, so just reuse the generation function.
        auto retval = srcParam();
        retval->name = "_ret";
        return genProto("?=?", {dstParam(), srcParam()}, {retval});
    }

    void genFuncBody(ast::FunctionDecl* func) {
        const CodeLocation& location = func->location;
        auto& params = func->params;
        if (InitTweak::isCopyConstructor(func) ||
            InitTweak::isAssignment(func)) {
            assert(2 == params.size());
            auto dstParam = params.front().strict_as<ast::ObjectDecl>();
            auto srcParam = params.back().strict_as<ast::ObjectDecl>();
            func->stmts = genCopyBody(location, dstParam, srcParam);
        } else {
            assert(1 == params.size());
            // Default constructor and destructor is empty.
            func->stmts = new ast::CompoundStmt(location);
            // Add unused attribute to parameter to silence warnings.
            addUnusedAttribute(params.front());

            // Just an extra step to make the forward and declaration match.
            if (forwards.empty()) return;
            ast::FunctionDecl* fwd = strict_dynamic_cast<ast::FunctionDecl*>(
                forwards.back().get_and_mutate());
            addUnusedAttribute(fwd->params.front());
        }
    }

    const ast::CompoundStmt* genCopyBody(const CodeLocation& location,
                                         const ast::ObjectDecl* dstParam,
                                         const ast::ObjectDecl* srcParam) {
        // const CodeLocation& location = func->location;
        // auto& params = func->params;
        // assert(2 == params.size());
        // auto dstParam = params.front().strict_as<ast::ObjectDecl>();
        // auto srcParam = params.back().strict_as<ast::ObjectDecl>();
        return new ast::CompoundStmt(
            location,
            {new ast::ExprStmt(
                location,
                new ast::UntypedExpr(
                    location, new ast::NameExpr(location, "__builtin_memcpy"),
                    {
                        new ast::AddressExpr(location, new ast::VariableExpr(
                                                           location, dstParam)),
                        new ast::AddressExpr(location, new ast::VariableExpr(
                                                           location, srcParam)),
                        new ast::SizeofExpr(location, srcParam->type),
                    }))});
    }

    void genDtorBody(ast::FunctionDecl* func) {
        const CodeLocation& location = func->location;
        auto& params = func->params;
        assert(1 == params.size());
        func->stmts = new ast::CompoundStmt(location);
        addUnusedAttribute(params.front());

        // Just an extra step to make the forward and declaration match.
        if (forwards.empty()) return;
        ast::FunctionDecl* fwd = strict_dynamic_cast<ast::FunctionDecl*>(
            forwards.back().get_and_mutate());
        addUnusedAttribute(fwd->params.front());
    }

    // ast::FunctionDecl*
    // ----------------------------------------------------

    ast::FunctionDecl* genSuccPredFunc(bool succ);

    const ast::Init* getAutoInit(const ast::Init* prev) const;

    std::vector<ast::ptr<ast::Init>> genLabelInit() const;

    std::vector<ast::ptr<ast::Init>> genValueInit() const;
    ast::ObjectDecl* genAttrArrayProto(
        const ast::EnumAttribute attr, const CodeLocation& location,
        std::vector<ast::ptr<ast::Init>>& inits) const;
    void genValueOrLabelBody(ast::FunctionDecl* func,
                             ast::ObjectDecl* arrDecl) const;
    void genPosnBody(ast::FunctionDecl* func) const;
    void genAttributesDecls(const ast::EnumAttribute attr);
};

std::vector<ast::ptr<ast::Init>> EnumAttrFuncGenerator::genLabelInit() const {
    std::vector<ast::ptr<ast::Init>> inits;
    for (size_t i = 0; i < decl->members.size(); i++) {
        ast::ptr<ast::Decl> mem = decl->members.at(i);
        auto memAsObjectDecl = mem.as<ast::ObjectDecl>();
        assert(memAsObjectDecl);
        inits.emplace_back(new ast::SingleInit(
            mem->location,
            ast::ConstantExpr::from_string(mem->location, mem->name)));
    }
    return inits;
}

std::vector<ast::ptr<ast::Init>> EnumAttrFuncGenerator::genValueInit() const {
    std::vector<ast::ptr<ast::Init>> inits;
    for (size_t i = 0; i < decl->members.size(); i++) {
        ast::ptr<ast::Decl> mem = decl->members.at(i);
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
                inits.emplace_back(getAutoInit(inits.at(i - 1)));
            }
        }
    }
    return inits;
}
const ast::Init* EnumAttrFuncGenerator::getAutoInit(
    const ast::Init* prev) const {
    if (prev == nullptr) {
        return new ast::SingleInit(
            getLocation(), ast::ConstantExpr::from_int(getLocation(), 0));
    }
    auto prevInit = dynamic_cast<const ast::SingleInit*>(prev);
    assert(prevInit);
    auto prevInitExpr = prevInit->value;
    if (auto constInit = prevInitExpr.as<ast::ConstantExpr>()) {
        // Assume no string literal for now
        return new ast::SingleInit(
            getLocation(), ast::ConstantExpr::from_int(
                               getLocation(), constInit->intValue() + 1));
    } else {
        auto untypedThisInit = new ast::UntypedExpr(
            getLocation(), new ast::NameExpr(getLocation(), "?++"),
            {prevInitExpr});
        return new ast::SingleInit(getLocation(), untypedThisInit);
    }
}

ast::FunctionDecl* EnumAttrFuncGenerator::genProto(
    std::string&& name, std::vector<ast::ptr<ast::DeclWithType>>&& params,
    std::vector<ast::ptr<ast::DeclWithType>>&& returns) const {
    ast::FunctionDecl* decl = new ast::FunctionDecl(
        // Auto-generated routines use the type declaration's location.
        getLocation(), std::move(name), {}, {}, std::move(params),
        std::move(returns),
        // Only a prototype, no body.
        nullptr,
        // Use static storage if we are at the top level.
        (0 < functionNesting) ? ast::Storage::Classes() : ast::Storage::Static,
        proto_linkage, std::vector<ast::ptr<ast::Attribute>>(),
        // Auto-generated routines are inline to avoid conflicts.
        ast::Function::Specs(ast::Function::Inline));
    decl->fixUniqueId();
    return decl;
}

void EnumAttrFuncGenerator::produceDecl(const ast::FunctionDecl* decl) {
    assert(nullptr != decl->stmts);

    definitions.push_back(decl);
}

void EnumAttrFuncGenerator::produceForwardDecl(const ast::FunctionDecl* decl) {
    if (0 != functionNesting) return;
    ast::FunctionDecl* fwd =
        (decl->stmts) ? ast::asForward(decl) : ast::deepCopy(decl);
    fwd->fixUniqueId();
    forwards.push_back(fwd);
}

ast::FunctionDecl* EnumAttrFuncGenerator::genPosnProto() const {
    return genProto(
        "posE",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret",
                             new ast::EnumAttrType(new ast::EnumInstType(decl),
                                                   ast::EnumAttribute::Posn))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genLabelProto() const {
    return genProto(
        "labelE",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(
            getLocation(), "_ret",
            new ast::PointerType(new ast::BasicType{ast::BasicType::Char}))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genValueProto() const {
    return genProto(
        "valueE",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret",
                             ast::deepCopy(decl->base))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genSuccProto() const {
    return genProto(
        "succ",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret",
                             new ast::EnumInstType(decl))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genPredProto() const {
    return genProto(
        "pred",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret",
                             new ast::EnumInstType(decl))});
}

inline ast::EnumAttrType * getPosnType( const ast::EnumDecl * decl ) {
    return new ast::EnumAttrType(new ast::EnumInstType(decl), ast::EnumAttribute::Posn);
}

ast::FunctionDecl* EnumAttrFuncGenerator::genSuccPosProto() const {
    return genProto(
        "_successor_",
        {new ast::ObjectDecl(getLocation(), "_i", getPosnType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret", getPosnType(decl))}
    );
}

ast::FunctionDecl* EnumAttrFuncGenerator::genPredPosProto() const {
    return genProto(
        "_predessor_",
        {new ast::ObjectDecl(getLocation(), "_i", getPosnType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret", getPosnType(decl))}
    );
}

ast::ObjectDecl* EnumAttrFuncGenerator::genAttrArrayProto(
    const ast::EnumAttribute attr, const CodeLocation& location,
    std::vector<ast::ptr<ast::Init>>& inits) const {
    ast::ArrayType* arrT = new ast::ArrayType(
        attr == ast::EnumAttribute::Value
            ? decl->base
            : new ast::PointerType(new ast::BasicType{ast::BasicType::Char}),
        ast::ConstantExpr::from_int(decl->location, decl->members.size()),
        ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim);

    ast::ObjectDecl* objDecl =
        new ast::ObjectDecl(decl->location, decl->getUnmangeldArrayName(attr),
                            arrT, new ast::ListInit(location, std::move(inits)),
                            ast::Storage::Static, ast::Linkage::AutoGen);

    return objDecl;
}

void EnumAttrFuncGenerator::genValueOrLabelBody(
    ast::FunctionDecl* func, ast::ObjectDecl* arrDecl) const {
    ast::UntypedExpr* untyped = ast::UntypedExpr::createCall(
        func->location, "?[?]",
        {new ast::NameExpr(func->location, arrDecl->name),
         new ast::CastExpr(
             func->location,
             new ast::VariableExpr(func->location, func->params.front()),
             new ast::EnumAttrType(new ast::EnumInstType(decl),
                                   ast::EnumAttribute::Posn))});
    func->stmts = new ast::CompoundStmt(
        func->location, {new ast::ReturnStmt(func->location, untyped)});
}

void EnumAttrFuncGenerator::genPosnBody(ast::FunctionDecl* func) const {
    auto castExpr = new ast::CastExpr(
        func->location,
        new ast::VariableExpr(func->location, func->params.front()),
        new ast::EnumAttrType(new ast::EnumInstType(decl),
                              ast::EnumAttribute::Posn));
    func->stmts = new ast::CompoundStmt(
        func->location, {new ast::ReturnStmt(func->location, castExpr)});
}

void EnumAttrFuncGenerator::genAttributesDecls(const ast::EnumAttribute attr) {
    if (attr == ast::EnumAttribute::Value ||
        attr == ast::EnumAttribute::Label) {
        std::vector<ast::ptr<ast::Init>> inits =
            attr == ast::EnumAttribute::Value ? genValueInit() : genLabelInit();
        ast::ObjectDecl* arrayProto =
            genAttrArrayProto(attr, getLocation(), inits);
        forwards.push_back(arrayProto);

        ast::FunctionDecl* funcProto = attr == ast::EnumAttribute::Value
                                           ? genValueProto()
                                           : genLabelProto();
        produceForwardDecl(funcProto);
        genValueOrLabelBody(funcProto, arrayProto);
        produceDecl(funcProto);
    } else {
        ast::FunctionDecl* funcProto = genPosnProto();
        produceForwardDecl(funcProto);
        genPosnBody(funcProto);
        produceDecl(funcProto);
    }
}

ast::FunctionDecl* EnumAttrFuncGenerator::genSuccPredFunc(bool succ) {
    ast::FunctionDecl* funcDecl = succ ? genSuccPosProto() : genPredPosProto();
    produceForwardDecl(funcDecl);

    const CodeLocation& location = getLocation();

    auto& params = funcDecl->params;
    assert(params.size() == 1);
    auto param = params.front().strict_as<ast::ObjectDecl>();


    auto rets = funcDecl->returns;
    assert(params.size() == 1);
    auto ret = rets.front().strict_as<ast::ObjectDecl>();
    auto retType = ret->type.strict_as<ast::EnumAttrType>();

    auto addOneExpr = ast::UntypedExpr::createCall( location,
        "?+?",
        {new ast::VariableExpr(location, param),
        ast::ConstantExpr::from_int(location, 1)}
    );

    funcDecl->stmts = new ast::CompoundStmt(
        location, {
            new ast::ReturnStmt(
                location, 
                new ast::CastExpr(location, addOneExpr, retType) 
            )
        }
    );

    return funcDecl;
}

void EnumAttrFuncGenerator::genAttrFunctions() {
    if (decl->base) {
        genAttributesDecls(ast::EnumAttribute::Value);
        genAttributesDecls(ast::EnumAttribute::Label);
        genAttributesDecls(ast::EnumAttribute::Posn);
    }
}

void EnumAttrFuncGenerator::genSuccPredDecl() {
    if (decl->base) {
        auto succProto = genSuccProto();
        auto predProto = genPredProto();

        produceForwardDecl(succProto);
        produceForwardDecl(predProto);
    }
}

void EnumAttrFuncGenerator::genSuccPredPosn() {
    if (decl->base) {
        ast::FunctionDecl* succ = genSuccPredFunc(true);
        ast::FunctionDecl* pred = genSuccPredFunc(false);

        produceDecl(succ);
        produceDecl(pred);
    }
}

void EnumAttrFuncGenerator::generateAndAppendFunctions(
    std::list<ast::ptr<ast::Decl>>& decls) {
    // Generate the functions (they go into forwards and definitions).
    genAttrStandardFuncs();
    genAttrFunctions();
    genSuccPredDecl();
    genSuccPredPosn(); // Posn
    // Now export the lists contents.
    decls.splice(decls.end(), forwards);
    decls.splice(decls.end(), definitions);
}

// ---------------------------------------------------------

struct ImplementEnumFunc final : public ast::WithDeclsToAdd<>,
                                 public ast::WithShortCircuiting {
    void previsit(const ast::EnumDecl* enumDecl);
    void previsit(const ast::FunctionDecl* functionDecl);
    void postvisit(const ast::FunctionDecl* functionDecl);

   private:
    // Current level of nested functions.
    unsigned int functionNesting = 0;
};

void ImplementEnumFunc::previsit(const ast::EnumDecl* enumDecl) {
    if (!enumDecl->body) return;
    if (!enumDecl->base) return;

    ast::EnumInstType enumInst(enumDecl->name);
    enumInst.base = enumDecl;

    EnumAttrFuncGenerator gen(enumDecl, &enumInst, functionNesting);
    gen.generateAndAppendFunctions(declsToAddAfter);
}

void ImplementEnumFunc::previsit(const ast::FunctionDecl*) {
    functionNesting += 1;
}

void ImplementEnumFunc::postvisit(const ast::FunctionDecl*) {
    functionNesting -= 1;
}

}  // namespace

void implementEnumFunc(ast::TranslationUnit& translationUnit) {
    ast::Pass<ImplementEnumFunc>::run(translationUnit);
}
}  // namespace Validate