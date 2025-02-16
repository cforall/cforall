#include "AST/Create.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "CodeGen/OperatorTable.hpp"  // for isCtorDtor, isCtorDtorAssign
#include "InitTweak/InitTweak.hpp"    // for isAssignment, isCopyConstructor
namespace Validate {

namespace {
class EnumAttrFuncGenerator {
	const ast::EnumDecl* decl;
	unsigned int functionNesting;
	const ast::StructDecl* quasi_void_decl;
	ast::Linkage::Spec proto_linkage;

public:
	std::list<ast::ptr<ast::Decl>> forwards;
	std::list<ast::ptr<ast::Decl>> definitions;

	void generateAndAppendFunctions(std::list<ast::ptr<ast::Decl>>&);

	EnumAttrFuncGenerator(
			const ast::EnumDecl* decl,
			const ast::EnumInstType*,
			unsigned int functionNesting )
		: decl(decl),
		  functionNesting{functionNesting},
		  proto_linkage{ast::Linkage::Cforall} {}

private:
	const CodeLocation& getLocation() const { return decl->location; }

	ast::FunctionDecl* genProto(
		std::string&& name, std::vector<ast::ptr<ast::DeclWithType>>&& params,
		std::vector<ast::ptr<ast::DeclWithType>>&& returns) const;

	void produceDecl(const ast::FunctionDecl* decl);
	void produceForwardDecl(const ast::FunctionDecl* decl);

	const ast::Decl* getDecl() const { return decl; }

	// Implement Bounded trait
	void genBoundedFunctions();
	ast::FunctionDecl* genBoundedProto(const char *) const;
	void genBoundedBody(ast::FunctionDecl* func) const;

	// Implement Serial trait
	void genSerialTraitFuncs();
	ast::FunctionDecl* genFromIntProto() const;
	ast::FunctionDecl* genFromInstanceProto() const;
	ast::FunctionDecl* genInstToInstFuncProto(const char* func) const;
	void genFromIntBody(ast::FunctionDecl *) const;
	void genFromInstanceBody(ast::FunctionDecl *) const;
	void genSuccPredBody(ast::FunctionDecl *, const char *) const;

	void genTypeNameFunc();

	// Implement TypedEnum trait
	void genTypedEnumFuncs();
	void genTypedEnumFunction(const ast::EnumAttribute attr);
	ast::FunctionDecl* genPosnProto() const;
	ast::FunctionDecl* genLabelProto() const;
	ast::FunctionDecl* genValueProto() const;
	ast::FunctionDecl* genTypeNameProto() const;

	void genValueOrLabelBody(
		ast::FunctionDecl* func, ast::ObjectDecl* arrDecl) const;
	void genPosnBody(ast::FunctionDecl* func) const;
	void genQuasiValueBody(ast::FunctionDecl* func) const;
	void genTypeNameBody(ast::FunctionDecl* func) const;

	// ----------------------------------------------------

	const ast::Init* getAutoInit(const ast::Init* prev) const;

	std::vector<ast::ptr<ast::Init>> genLabelInit() const;

	std::vector<ast::ptr<ast::Init>> genValueInit() const;
	ast::ObjectDecl* genAttrArrayProto(
		const CodeLocation& location, const std::string& prefix,
		const ast::Type * type, std::vector<ast::ptr<ast::Init>>& inits ) const;
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
		if (auto& init = memAsObjectDecl->init) {
			if ( auto singleInit = init.as<ast::SingleInit>() ) {
				if ( auto nameExpr = singleInit->value.as<ast::NameExpr>() ) {
					auto name = nameExpr->name;
					if (auto it = std::find_if(decl->members.begin(), decl->members.end(),
						[name](ast::ptr<ast::Decl> mem_decl) {
							return (mem_decl->name == name);
						}); it != std::end(decl->members)
					) {
						auto index = std::distance( decl->members.begin(), it );
						auto targetInit = inits.at(index).strict_as<ast::SingleInit>();
						auto targetExpr = targetInit->value;
						inits.push_back( new ast::SingleInit( targetExpr->location, targetExpr ) );
						continue;
					}
				}
			}
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
			getLocation(), new ast::NameExpr(getLocation(), "?+?"),
			{	prevInitExpr,
				new ast::ConstantExpr( getLocation(), new ast::OneType, "1", 1) });
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
        "posn",
        {new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
        {new ast::ObjectDecl(getLocation(), "_ret",
            new ast::BasicType(ast::BasicKind::SignedInt))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genLabelProto() const {
	return genProto(
		"label",
		{new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
		{new ast::ObjectDecl(
			getLocation(), "_ret",
			new ast::PointerType(
				new ast::BasicType(ast::BasicKind::Char, ast::CV::Const)))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genValueProto() const {
	assert( decl->is_typed_enum() );
	return genProto(
		"value",
		{new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
		{new ast::ObjectDecl(getLocation(), "_ret", decl->base)});
}

// ast::FunctionDecl* EnumAttrFuncGenerator::genQuasiValueProto() const {
// 	return genProto(
// 		"value",
// 		{new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
// 		{new ast::ObjectDecl(getLocation(), "_ret",
// 		                		new ast::StructInstType(quasi_void_decl))});
// }

ast::FunctionDecl* EnumAttrFuncGenerator::genFromIntProto() const {
	return genProto(
		"fromInt_unsafe",
		{new ast::ObjectDecl(getLocation(), "_i", new ast::BasicType(ast::BasicKind::SignedInt))},
		{new ast::ObjectDecl(getLocation(), "_ret", new ast::EnumInstType(decl))}
	);
}

ast::FunctionDecl* EnumAttrFuncGenerator::genFromInstanceProto() const {
	return genProto(
		"fromInstance",
		{new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
		{new ast::ObjectDecl(getLocation(), "_ret", new ast::BasicType(ast::BasicKind::SignedInt))}
	);
}

ast::FunctionDecl* EnumAttrFuncGenerator::genTypeNameProto() const {
	return genProto(
		"type_name",
		{new ast::ObjectDecl(getLocation(), "", new ast::EnumInstType(decl))},
		{new ast::ObjectDecl(
			getLocation(), "_ret",
			new ast::PointerType(
				new ast::BasicType(ast::BasicKind::Char, ast::CV::Const)))});
}

void EnumAttrFuncGenerator::genFromIntBody(ast::FunctionDecl* func) const {
	auto params = func->params;
	assert( params.size() == 1 );
	auto param = params.front();
	auto castExpr = new ast::CastExpr(
		func->location,
		new ast::VariableExpr(func->location, param),
		new ast::EnumInstType(decl),
		ast::GeneratedFlag::ExplicitCast
	);
	func->stmts = new ast::CompoundStmt(
		func->location, {new ast::ReturnStmt(func->location, castExpr)}
	);
}

void EnumAttrFuncGenerator::genFromInstanceBody(ast::FunctionDecl* func) const {
	auto params = func->params;
	assert( params.size() == 1 );
	auto param = params.front();
	ast::UntypedExpr* untyped = ast::UntypedExpr::createCall(
		func->location, "posn", { new ast::VariableExpr(func->location, param) });
	func->stmts = new ast::CompoundStmt(
		func->location, {new ast::ReturnStmt(func->location, untyped)}
	);
}

void EnumAttrFuncGenerator::genSuccPredBody(ast::FunctionDecl * func, const char* opt) const {
	auto params = func->params;
	assert( params.size() == 1 );
	auto param = params.front();
	auto enumToInt = new ast::CastExpr(
		func->location,
		new ast::VariableExpr(func->location, param),
		new ast::BasicType(ast::BasicKind::SignedInt),
		ast::GeneratedFlag::ExplicitCast
	);
	ast::UntypedExpr* addOneExpr = ast::UntypedExpr::createCall( func->location,
		opt,
		{enumToInt,
		ast::ConstantExpr::from_int(func->location, 1)}
	);
	auto intToEnum = new ast::CastExpr(
		func->location,
		addOneExpr,
		new ast::EnumInstType( decl ),
		ast::GeneratedFlag::ExplicitCast
	);
	func->stmts = new ast::CompoundStmt(
		func->location, {
			new ast::ReturnStmt(
				func->location,
				intToEnum
			)
		}
	);
}

void EnumAttrFuncGenerator::genSerialTraitFuncs() {
	ast::FunctionDecl * protos[4] = {
		genFromIntProto(),
		genFromInstanceProto(),
		genInstToInstFuncProto("succ_unsafe"),
		genInstToInstFuncProto("pred_unsafe")
	};
	for (auto& proto: protos) produceForwardDecl(proto);
	genFromIntBody(protos[0]);
	genFromInstanceBody(protos[1]);
	genSuccPredBody(protos[2], "?+?");
	genSuccPredBody(protos[3], "?-?");
	for (auto& proto: protos) produceDecl(proto);
}

ast::FunctionDecl* EnumAttrFuncGenerator::genInstToInstFuncProto(const char * func) const {
	return genProto(
		func,
		{new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))},
		{new ast::ObjectDecl(getLocation(), "_ret",
		                     new ast::EnumInstType(decl))});
}

ast::FunctionDecl* EnumAttrFuncGenerator::genBoundedProto(const char * func) const {
    return genProto(func, {}, {
        new ast::ObjectDecl(getLocation(), "_i", new ast::EnumInstType(decl))
    });
}

void EnumAttrFuncGenerator::genBoundedBody(ast::FunctionDecl* func) const {
	const CodeLocation & loc = func->location;
	auto mem = func->name=="lowerBound"?  decl->members.front() : decl->members.back();
	// auto expr = new ast::NameExpr( loc, mem->name );
	auto expr = new ast::QualifiedNameExpr( loc, decl->name, mem->name );
	func->stmts = new ast::CompoundStmt( loc, {new ast::ReturnStmt(loc, expr)});
}

void EnumAttrFuncGenerator::genBoundedFunctions() {
	ast::FunctionDecl * boundedProtos[2] = {genBoundedProto("upperBound"), genBoundedProto("lowerBound")};
	for (auto & protos: boundedProtos) {
		produceForwardDecl(protos);
		genBoundedBody(protos);
		produceDecl(protos);
	}
}

ast::ObjectDecl* EnumAttrFuncGenerator::genAttrArrayProto(
		const CodeLocation& location, const std::string& prefix,
		const ast::Type * type, std::vector<ast::ptr<ast::Init>>& inits ) const {
	ast::ArrayType* arrT = new ast::ArrayType(
		type,
		ast::ConstantExpr::from_int(decl->location, decl->members.size()),
		ast::LengthFlag::FixedLen, ast::DimensionFlag::DynamicDim);

	ast::ObjectDecl* objDecl =
		new ast::ObjectDecl(
			decl->location, prefix + decl->name,
			arrT, new ast::ListInit( location, std::move( inits ) ),
			ast::Storage::Static, ast::Linkage::AutoGen );

	return objDecl;
}

void EnumAttrFuncGenerator::genValueOrLabelBody(
	ast::FunctionDecl* func, ast::ObjectDecl* arrDecl) const {
	ast::UntypedExpr* untyped = ast::UntypedExpr::createCall(
		func->location, "?[?]",
		{new ast::NameExpr(func->location, arrDecl->name),
		new ast::CastExpr(
			func->location,
			new ast::VariableExpr( func->location, func->params.front() ),
			new ast::BasicType( ast::BasicKind::SignedInt ),
			ast::GeneratedFlag::ExplicitCast
		)});
	func->stmts = new ast::CompoundStmt(
		func->location, {new ast::ReturnStmt(func->location, untyped)});
}

// void EnumAttrFuncGenerator::genQuasiValueBody(ast::FunctionDecl* func) const {
// 	auto location = func->location;
// 	const ast::ObjectDecl * objDecl = new ast::ObjectDecl(
// 		location, "_out", new ast::StructInstType( quasi_void_decl ));
// 	const ast::DeclStmt * declStmt = new ast::DeclStmt(location, objDecl);
// 	const ast::VariableExpr * varExpr = new ast::VariableExpr(location, objDecl);
// 	const ast::ReturnStmt * retStmt = new ast::ReturnStmt(location, varExpr);

// 	func->stmts = new ast::CompoundStmt(
// 		location, {declStmt, retStmt}
// 	);
// }

void EnumAttrFuncGenerator::genPosnBody(ast::FunctionDecl* func) const {
	auto castExpr = new ast::CastExpr(
		func->location,
		new ast::VariableExpr(func->location, func->params.front()),
		new ast::BasicType( ast::BasicKind::SignedInt ),
			ast::GeneratedFlag::ExplicitCast);
	func->stmts = new ast::CompoundStmt(
		func->location, {new ast::ReturnStmt(func->location, castExpr)});
}

void EnumAttrFuncGenerator::genTypeNameBody(ast::FunctionDecl* func) const {
	const ast::Expr * type_name = ast::ConstantExpr::from_string(func->location, decl->name);
	func->stmts = new ast::CompoundStmt(
		func->location, {new ast::ReturnStmt(func->location, type_name)}
	);
}

void EnumAttrFuncGenerator::genTypedEnumFunction(const ast::EnumAttribute attr) {
	if (attr == ast::EnumAttribute::Value) {
		if ( !decl->is_typed_enum() ) return;
		std::vector<ast::ptr<ast::Init>> inits = genValueInit();
		ast::ObjectDecl* arrayProto =
			genAttrArrayProto( getLocation(), "values_", decl->base, inits );
		forwards.push_back(arrayProto);

		ast::FunctionDecl* funcProto = genValueProto();
		produceForwardDecl(funcProto);
		genValueOrLabelBody(funcProto, arrayProto);
		produceDecl(funcProto);
	} else if (attr == ast::EnumAttribute::Label) {
		std::vector<ast::ptr<ast::Init>> inits = genLabelInit();
		const ast::Type * type = new ast::PointerType(
			new ast::BasicType( ast::BasicKind::Char, ast::CV::Const ) );
		ast::ObjectDecl* arrayProto =
			genAttrArrayProto( getLocation(), "labels_", type, inits );
		forwards.push_back(arrayProto);
		ast::FunctionDecl* funcProto = genLabelProto();
		produceForwardDecl(funcProto);
		genValueOrLabelBody(funcProto, arrayProto);
		produceDecl(funcProto);
	} else {
		assert( attr == ast::EnumAttribute::Posn );
		ast::FunctionDecl* funcProto = genPosnProto();
		produceForwardDecl(funcProto);
		genPosnBody(funcProto);
		produceDecl(funcProto);
	}
}

void EnumAttrFuncGenerator::genTypedEnumFuncs() {
	genTypedEnumFunction(ast::EnumAttribute::Value);
	genTypedEnumFunction(ast::EnumAttribute::Label);
	genTypedEnumFunction(ast::EnumAttribute::Posn);
}

void EnumAttrFuncGenerator::genTypeNameFunc() {
	ast::FunctionDecl* funcProto = genTypeNameProto();
	produceForwardDecl(funcProto);
	genTypeNameBody(funcProto);
	produceDecl(funcProto);
}

void EnumAttrFuncGenerator::generateAndAppendFunctions(
	std::list<ast::ptr<ast::Decl>>& decls) {
	// Generate the functions (they go into forwards and definitions).
	genTypeNameFunc();
	genTypedEnumFuncs();
	genSerialTraitFuncs();
	genBoundedFunctions();
	// Now export the lists contents.
	decls.splice(decls.end(), forwards);
	decls.splice(decls.end(), definitions);
}

// ---------------------------------------------------------

struct ImplementEnumFunc final :
		public ast::WithDeclsToAdd, public ast::WithShortCircuiting {
	void previsit(const ast::EnumDecl* enumDecl);
	void previsit(const ast::FunctionDecl* functionDecl);
	void postvisit(const ast::FunctionDecl* functionDecl);

private:
	// Current level of nested functions.
	unsigned int functionNesting = 0;
};

void ImplementEnumFunc::previsit(const ast::EnumDecl* enumDecl) {
	if ( !enumDecl->body || enumDecl->is_c_enum() ) return;
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

} // namespace

void implementEnumFunc(ast::TranslationUnit& translationUnit) {
	ast::Pass<ImplementEnumFunc>::run(translationUnit);
}

} // namespace Validate
