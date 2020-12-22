#include "sema/SemanticAnalyzer.hpp"

#include <algorithm>
#include <cassert>
#include <cstdlib>

#include "visitor/AstNodeInclude.hpp"

void SemanticAnalyzer::visit(ProgramNode &p_program) {
    startScope();
    insert({p_program.getNameCString(),
            PK_PROGRAM,
            currentLevel,
            p_program.getType(),
            "",
            p_program.getLocation(),
            true});
    currentScopeKind = PK_ERR;
    beginProcedure(p_program.getType());
    p_program.visitChildNodes(*this);
    endProcedure();
    endScope();
    
    /*assert(expTypeStack.empty());
    assert(reserved.empty());
    assert(tableRowStack.empty());
    assert(effectiveTableStack.empty());
    assert(tableStack.empty());
    assert(procTypeStack.empty());
    assert(currentLevel == -1);
    assert(!levelPreserved);*/
}

void SemanticAnalyzer::visit(DeclNode &p_decl) {
    p_decl.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(VariableNode &p_variable) {
    // If is array then length must be positive
    bool ok = 1;
    for (int len : p_variable.getType().getDimensions()) {
        if (len <= 0) {
            std::string msg = "'";
            msg += p_variable.getNameCString();
            msg += "' declared as an array with an index that is not greater than 0";
            result.push_back({p_variable.getLocation(),
                              msg});
            ok = 0;
            break;
        }
    }

    p_symbol_kind kind = PK_VARIABLE;
    if (currentScopeKind == PK_FUNTION)
        kind = PK_PARAMETER;
    else if (currentScopeKind == PK_FUNTION)
        kind = PK_LOOP_VAR;

    if (p_variable.isConstant()) {
        insert({p_variable.getNameCString(),
                PK_CONSTANT,
                currentLevel,
                p_variable.getType(),
                p_variable.getConstantValue(),
                p_variable.getLocation(),
                ok});
    } else {
        insert({p_variable.getNameCString(),
                kind,
                currentLevel,
                p_variable.getType(),
                "",
                p_variable.getLocation(),
                ok});
    }
}

void SemanticAnalyzer::visit(ConstantValueNode &p_constant_value) {
    pushExpType({p_constant_value.getLocation(), *p_constant_value.getTypePtr(), PE_CONST});
}

void SemanticAnalyzer::visit(FunctionNode &p_function) {
    insert({p_function.getNameCString(),
            PK_FUNTION,
            currentLevel,
            p_function.getType(),
            p_function.getPrototype(),
            p_function.getLocation(),
            true});

    startScope();
    p_symbol_kind prevScopeKind = currentScopeKind;
    currentScopeKind = PK_FUNTION;
    preserveLevel();

    beginProcedure(p_function.getType());
    p_function.visitChildNodes(*this);
    endProcedure();

    currentScopeKind = prevScopeKind;
    endScope();
}

void SemanticAnalyzer::visit(CompoundStatementNode &p_compound_statement) {
    p_symbol_kind tmp = currentScopeKind;
    currentScopeKind = PK_ERR;
    startScope();
    int p = expTypeStack.size();
    p_compound_statement.visitChildNodes(*this);
    while (expTypeStack.size() > p) expTypeStack.pop_back();
    endScope();
    currentScopeKind = tmp;
}

void SemanticAnalyzer::visit(PrintNode &p_print) {
    p_print.visitChildNodes(*this);

    // Checks if type is scalar
    ExpressionTypeInfo etype = popExpType();
    if (etype.type.getPrimitiveType() == Prim::kUnknown) {
        return;
    }
    if (etype.type.getDimensions().size() > 0 || etype.type.getPrimitiveType() == Prim::kVoidType) {
        std::string errMsg = "expression of print statement must be scalar type";
        result.push_back({etype.location, errMsg});
    }
}

void SemanticAnalyzer::visit(BinaryOperatorNode &p_bin_op) {
    p_bin_op.visitChildNodes(*this);

    PType retype = popExpType().type;
    PType letype = popExpType().type;

    auto gg = [&]() {
        std::string errMsg = "invalid operands to binary operator '";
        errMsg += p_bin_op.getOpCString();
        errMsg += "' ('";
        errMsg += letype.getPTypeCString();
        errMsg += "' and '";
        errMsg += retype.getPTypeCString();
        errMsg += "')";
        result.push_back({p_bin_op.getLocation(),
                          errMsg});
        pushExpType({p_bin_op.getLocation(), Prim::kUnknown, PE_EXPR});
    };

    if (letype.getDimensions().size() > 0 || retype.getDimensions().size() > 0) {
        gg();
        return;
    }

    const Prim &rtype = retype.getPrimitiveType();
    const Prim &ltype = letype.getPrimitiveType();
    // skip already described error
    if (ltype == Prim::kUnknown || rtype == Prim::kUnknown) {
        pushExpType({p_bin_op.getLocation(), Prim::kUnknown, PE_EXPR});
        return;
    }

    switch (p_bin_op.getOperator()) {
        case Operator::kMultiplyOp:
        case Operator::kDivideOp:
        case Operator::kMinusOp:
            if (ltype == Prim::kRealType && rtype == Prim::kRealType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType, PE_EXPR});
            } else {
                gg();
            }
            break;
        case Operator::kGreaterOp:
        case Operator::kGreaterOrEqualOp:
        case Operator::kLessOp:
        case Operator::kLessOrEqualOp:
        case Operator::kEqualOp:
        case Operator::kNotEqualOp:
            if (ltype == Prim::kRealType && rtype == Prim::kRealType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else {
                gg();
            }
            break;
        case Operator::kPlusOp:
            if (ltype == Prim::kRealType && rtype == Prim::kRealType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType, PE_EXPR});
            } else if (ltype == Prim::kStringType && rtype == Prim::kStringType) {
                pushExpType({p_bin_op.getLocation(), Prim::kStringType, PE_EXPR});
            } else {
                gg();
            }
            break;
        case Operator::kModOp:
            if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType, PE_EXPR});
            } else {
                gg();
            }
            break;
        case Operator::kAndOp:
        case Operator::kOrOp:
        case Operator::kNotOp:
            if (ltype == Prim::kBoolType && rtype == Prim::kBoolType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else {
                gg();
            }
            break;
        default:
            gg();
            break;
    }
}

void SemanticAnalyzer::visit(UnaryOperatorNode &p_un_op) {
    p_un_op.visitChildNodes(*this);

    PType ptype = popExpType().type;
    auto gg = [&]() {
        std::string errMsg = "invalid operand to unary operator '";
        errMsg += p_un_op.getOpCString();
        errMsg += "' ('";
        errMsg += ptype.getPTypeCString();
        errMsg += "')";
        result.push_back({p_un_op.getLocation(),
                          errMsg});
        pushExpType({p_un_op.getLocation(), Prim::kUnknown, PE_EXPR});
    };

    if (ptype.getDimensions().size() > 0 || ptype.getDimensions().size() > 0) {
        gg();
        return;
    }

    const Prim &stype = ptype.getPrimitiveType();
    // skip already described error
    if (stype == Prim::kUnknown) {
        pushExpType({p_un_op.getLocation(), Prim::kUnknown, PE_EXPR});
        return;
    }

    switch (p_un_op.getOperator()) {
        case Operator::kNegOp:
            if (stype == Prim::kIntegerType) {
                pushExpType({p_un_op.getLocation(), Prim::kIntegerType, PE_EXPR});
            } else if (stype == Prim::kRealType) {
                pushExpType({p_un_op.getLocation(), Prim::kRealType, PE_EXPR});
            } else {
                gg();
            }
            break;
        case Operator::kNotOp:
            if (stype == Prim::kBoolType) {
                pushExpType({p_un_op.getLocation(), Prim::kBoolType, PE_EXPR});
            } else {
                gg();
            }
            break;
        default:
            gg();
            break;
    }
}

void SemanticAnalyzer::visit(FunctionInvocationNode &p_func_invocation) {
    // Checks if such function reference exists
    if (!refer(p_func_invocation.getNameCString())) {
        std::string msg = "use of undeclared symbol '";
        msg += p_func_invocation.getNameCString();
        msg += "'";
        result.push_back({p_func_invocation.getLocation(),
                          msg});
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown, PE_FUNC});
        return;
    }

    // Checks if such function is function
    const SymbolTableRow &row = reference(p_func_invocation.getNameCString());
    if (row.getKind() != PK_FUNTION) {
        std::string msg = "call of non-function symbol '";
        msg += p_func_invocation.getNameCString();
        msg += "'";
        result.push_back({p_func_invocation.getLocation(),
                          msg});
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown, PE_FUNC});
        return;
    }

    // Checks if argument count matches
    int alen = p_func_invocation.getArguments().size();
    const std::vector<PType> &paramTypes = row.getAttribute().getTypes();
    if (alen != paramTypes.size()) {
        std::string msg = "too few/much arguments provided for function '";
        msg += p_func_invocation.getNameCString();
        msg += "'";
        result.push_back({p_func_invocation.getLocation(),
                          msg});
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown, PE_FUNC});
        return;
    }

    p_func_invocation.visitChildNodes(*this);

    // Check if argument type matches
    std::vector<ExpressionTypeInfo> args;
    for (int i = 0; i < paramTypes.size(); i++) {
        args.push_back(popExpType());
    }
    reverse(args.begin(), args.end());

    bool ok = 1;
    for (int i = 0; i < paramTypes.size(); i++) {
        PType argType = args[i].type;
        PType paramType = paramTypes[i];
        if (argType.getPrimitiveType() == Prim::kUnknown) {
            // If this is solved error, omits it
            ok = 0;
        } else if (argType != paramType) {
            std::string msg = "incompatible type passing '";
            msg += argType.getPTypeCString();
            msg += "' to parameter of type '";
            msg += paramType.getPTypeCString();
            msg += "'";
            result.push_back({args[i].location,
                              msg});
            ok = 0;
        }
    }

    if (ok) {
        pushExpType({p_func_invocation.getLocation(), row.getType(), PE_FUNC});
    } else {
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown, PE_FUNC});
    }
}

void SemanticAnalyzer::visit(VariableReferenceNode &p_variable_ref) {
    // Checks if the variable is already defined
    if (!refer(p_variable_ref.getNameCString())) {
        std::string msg = "use of undeclared symbol '";
        msg += p_variable_ref.getNameCString();
        msg += "'";
        result.push_back({p_variable_ref.getLocation(),
                          msg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown, PE_VAR});
        return;
    }

    const SymbolTableRow &var = reference(p_variable_ref.getNameCString());

    // Checks if such reference is of type varirable
    if (var.getKind() == PK_FUNTION || var.getKind() == PK_PROGRAM || var.getKind() == PK_ERR) {
        std::string msg = "use of non-variable symbol '";
        msg += p_variable_ref.getNameCString();
        msg += "'";
        result.push_back({p_variable_ref.getLocation(), msg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown, PE_VAR});
        return;
    }

    // Checks if this varirable is declared correctly
    if (!var.isEffective()) {
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown, PE_VAR});
        return;
    }

    p_variable_ref.visitChildNodes(*this);
    bool ok = 1;

    // Checks if array index is integer
    int arrayLen = p_variable_ref.getArrayIndicies().size();
    for (int i = 0; i < arrayLen; i++) {
        ExpressionTypeInfo et = popExpType();
        if (et.type.getPrimitiveType() == Prim::kUnknown) {
            ok = 0;
        } else if (et.type.getDimensions().size() > 0 || et.type.getPrimitiveType() != Prim::kIntegerType) {
            std::string errMsg = "index of array reference must be an integer";
            result.push_back({et.location, errMsg});
            ok = 0;
        }
    }

    if (!ok) {
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown, PE_VAR});
        return;
    }

    // Checks over array subscript
    int al = reference(p_variable_ref.getNameCString()).getType().getDimensions().size();
    if (arrayLen > al) {
        std::string errMsg = "there is an over array subscript on '";
        errMsg += p_variable_ref.getNameCString();
        errMsg += "'";
        result.push_back({p_variable_ref.getLocation(), errMsg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown, PE_VAR});
        return;
    }

    const SymbolTableRow &row = reference(p_variable_ref.getNameCString());
    PType type = row.getType();
    const std::vector<uint64_t> &dim = type.getDimensions();
    const std::vector<uint64_t> newdim(dim.begin() + arrayLen, dim.end());
    PType newType(type.getPrimitiveType());
    newType.setDimensions(newdim);
    pushExpType({p_variable_ref.getLocation(), newType, PE_VAR});
}

void SemanticAnalyzer::visit(AssignmentNode &p_assignment) {
    p_assignment.visitChildNodes(*this);

    ExpressionTypeInfo rtype = popExpType();
    ExpressionTypeInfo ltype = popExpType();
    if (ltype.type == Prim::kUnknown) {
        return;
    }

    const VariableReferenceNode &lnode = p_assignment.getLeftNode();
    const ExpressionNode &rnode = p_assignment.getRightNode();

    // array assignment is not allowed
    if (ltype.type.getDimensions().size() > 0) {
        result.push_back({ltype.location,
                          "array assignment is not allowed"});
        return;
    }

    // cannot assign to variable '{symbol_name}' which is a constant
    const char *key = lnode.getNameCString();
    assert(refer(key));
    const SymbolTableRow &row = reference(key);
    if (row.getKind() == PK_CONSTANT) {
        std::string errMsg = "cannot assign to variable '";
        errMsg += key;
        errMsg += "' which is a constant";
        result.push_back({ltype.location, errMsg});
        return;
    }

    // The variable reference cannot be a reference to a loop variable when
    // the context is within a loop body.
    if (row.getKind() == PK_LOOP_VAR) {
        result.push_back({ltype.location,
                          "the value of loop variable cannot be modified inside the loop body"});
        return;
    }

    // The type of the result of the expression cannot be an array type
    if (rtype.type.getDimensions().size() > 0) {
        result.push_back({rtype.location, "array assignment is not allowed"});
        return;
    }

    // The type of the variable reference (lvalue) must be the same as the one
    // of the expression after appropriate type coercion.
    Prim lptype = ltype.type.getPrimitiveType();
    Prim rptype = rtype.type.getPrimitiveType();
    auto gg = [&]() {
        std::string errMsg = "assigning to '";
        errMsg += ltype.type.getPTypeCString();
        errMsg += "' from incompatible type '";
        errMsg += rtype.type.getPTypeCString();
        errMsg += "'";
        result.push_back({p_assignment.getLocation(),
                          errMsg});
    };
    if (ltype.type != rtype.type) {
        if (lptype == Prim::kRealType && rptype == Prim::kIntegerType) {
            // safe
        } else {
            gg();
            return;
        }
    }
}

void SemanticAnalyzer::visit(ReadNode &p_read) {
    p_read.visitChildNodes(*this);

    // Checks if type is scalar
    ExpressionTypeInfo etype = popExpType();
    if (etype.type.getPrimitiveType() == Prim::kUnknown) {
        return;
    }
    if (etype.type.getDimensions().size() > 0) {
        std::string errMsg = "variable reference of read statement must be scalar type";
        result.push_back({etype.location, errMsg});
    }

    const char *key = p_read.getVarRefNameCString();
    assert(refer(key));
    const SymbolTableRow &row = reference(key);
    if (row.getKind() == PK_CONSTANT || row.getKind() == PK_LOOP_VAR || row.getKind() == PK_ERR) {
        std::string errMsg = "variable reference of read statement cannot be a constant or loop variable";
        result.push_back({etype.location, errMsg});
    }
}

void SemanticAnalyzer::visit(IfNode &p_if) {
    p_if.visitChildNodes(*this);
    ExpressionTypeInfo et = popExpType();
    if (et.type.getPrimitiveType() == Prim::kUnknown) {
        return;
    }

    if (et.type.getDimensions().size() > 0 || et.type.getPrimitiveType() != Prim::kBoolType) {
        result.push_back({et.location,
                          "the expression of condition must be boolean type"});
        return;
    }
}

void SemanticAnalyzer::visit(WhileNode &p_while) {
    p_while.visitChildNodes(*this);

    ExpressionTypeInfo et = popExpType();
    if (et.type.getPrimitiveType() == Prim::kUnknown) {
        return;
    }

    if (et.type.getDimensions().size() > 0 || et.type.getPrimitiveType() != Prim::kBoolType) {
        result.push_back({et.location,
                          "the expression of condition must be boolean type"});
        return;
    }
}

void SemanticAnalyzer::visit(ForNode &p_for) {
    startScope();
    const VariableNode &varNode = p_for.getLoopVarNode();

    bool ok = 1;
    // The initial value of the loop variable and the constant value of the
    // condition must be in the incremental order
    int l = p_for.getLoopBeginBound();
    int r = p_for.getLoopEndBound();
    if (r <= l) {
        result.push_back({p_for.getLocation(),
                          "the lower bound and upper bound of iteration count must be in the incremental order"});
        ok = 0;
    }

    insert({varNode.getNameCString(),
            PK_LOOP_VAR,
            currentLevel,
            PType(PType::PrimitiveTypeEnum::kIntegerType),
            "",
            varNode.getLocation(),
            ok});
    reserve(varNode.getNameCString());

    p_for.visitBodyNode(*this);

    release(varNode.getNameCString());
    endScope();
}

void SemanticAnalyzer::visit(ReturnNode &p_return) {
    p_return.visitChildNodes(*this);

    ExpressionTypeInfo et = popExpType();
    if (et.type.getPrimitiveType() == Prim::kUnknown) {
        return;
    }

    PType proctype = currentProcedureType();
    if (et.type != proctype) {
        if (proctype.getPrimitiveType() == Prim::kVoidType) {
            result.push_back({p_return.getLocation(),
                              "program/procedure should not return a value"});
        } else {
            std::string errMsg = "return '";
            errMsg += et.type.getPTypeCString();
            errMsg += "' from a function with return type '";
            errMsg += proctype.getPTypeCString();
            errMsg += "'";
            result.push_back({et.location, errMsg});
        }
        return;
    }
}

void SemanticAnalyzer::startScope() {
    if (levelPreserved > 0) {
        levelPreserved--;
        tableStack.push_back(nullptr);
    } else {
        tableStack.push_back(new SymbolTable());
        effectiveTableStack.push_back(tableStack.back());
        currentLevel++;
    }
}

void SemanticAnalyzer::endScope() {
    assert(!tableStack.empty());
    SymbolTable *t = tableStack.back();
    if (t) {
        effectiveTableStack.pop_back();
        t->display(tableLog);
        delete t;
        currentLevel--;
    }
    tableStack.pop_back();
}

SymbolTable &SemanticAnalyzer::currentScopeTable() {
    return *effectiveTableStack.back();
}

const SymbolTable &SemanticAnalyzer::currentScopeTable() const {
    return *effectiveTableStack.back();
}

void SemanticAnalyzer::insert(const SymbolTableRow &row) {
    // heck key collision
    std::string key = row.getName();
    if (currentScopeTable().getKeySet().count(key) || reserved.count(key)) {
        std::string errMsg = "symbol '";
        errMsg += row.getName();
        errMsg += "' is redeclared";
        result.push_back({row.getLocation(),
                          errMsg});
        return;
    }

    currentScopeTable().getContent().push_back(row);
    currentScopeTable().getKeySet().insert(key);
}

void SemanticAnalyzer::stashRow() {
    assert(!currentScopeTable().getContent().empty());
    tableRowStack.push_back(&currentScopeTable().getContent().back());
}

void SemanticAnalyzer::popRow() {
    assert(!tableRowStack.empty());
    tableRowStack.pop_back();
}

SymbolTableRow &SemanticAnalyzer::topRow() {
    assert(!tableRowStack.empty());
    return *tableRowStack.back();
}

void SemanticAnalyzer::preserveLevel() {
    levelPreserved++;
}

void SemanticAnalyzer::reserve(const char *key) {
    reserved.insert(std::string(key));
}

void SemanticAnalyzer::release(const char *key) {
    reserved.erase(std::string(key));
}

bool SemanticAnalyzer::refer(const char *key) const {
    std::string skey = key;
    auto a = effectiveTableStack.rbegin();
    while (a != effectiveTableStack.rend()) {
        const std::vector<SymbolTableRow> &rows = (*a)->getContent();
        auto b = rows.rbegin();
        while (b != rows.rend()) {
            if (std::string(b->getName()) == skey) return true;
            b++;
        }
        a++;
    }
    return false;
}

void SemanticAnalyzer::pushExpType(const ExpressionTypeInfo &type) {
   // printf("PUSH (%d, %d)\n", type.location.line, type.location.col);
    expTypeStack.push_back(type);
}

ExpressionTypeInfo SemanticAnalyzer::popExpType() {
    assert(!expTypeStack.empty());
    ExpressionTypeInfo ret = expTypeStack.back();
   // printf("POP (%d, %d)\n", ret.location.line, ret.location.col);
    expTypeStack.pop_back();
    return ret;
}

const PType &SemanticAnalyzer::currentProcedureType() const {
    assert(!procTypeStack.empty());
    return procTypeStack.back();
}

void SemanticAnalyzer::beginProcedure(const PType &type) {
    procTypeStack.push_back(type);
}

void SemanticAnalyzer::endProcedure() {
    assert(!procTypeStack.empty());
    procTypeStack.pop_back();
}

void SemanticAnalyzer::displayTables() const {
    printf("%s", tableLog.c_str());
}

const SymbolTableRow &SemanticAnalyzer::reference(const char *key) const {
    std::string skey = key;
    auto a = effectiveTableStack.rbegin();
    while (a != effectiveTableStack.rend()) {
        const std::vector<SymbolTableRow> &rows = (*a)->getContent();
        auto b = rows.rbegin();
        while (b != rows.rend()) {
            if (std::string(b->getName()) == skey) return *b;
            b++;
        }
        a++;
    }
    // GG
    throw "";
}

const char *ktoa(p_symbol_kind kind) {
    switch (kind) {
        case PK_CONSTANT:
            return "constant";
            break;
        case PK_PROGRAM:
            return "program";
            break;
        case PK_PARAMETER:
            return "parameter";
            break;
        case PK_FUNTION:
            return "function";
            break;
        case PK_VARIABLE:
            return "variable";
            break;
        case PK_LOOP_VAR:
            return "loop_var";
            break;
    }
    // Never thrown
    throw "我的室友是肥宅";
}

void dumpDemarcation(std::string &buff, const char chr) {
    for (size_t i = 0; i < 110; ++i) {
        buff.push_back(chr);
    }
    buff.push_back('\n');
}

void SymbolTable::display(std::string &buff) const {
    char cbuff[65536];
    dumpDemarcation(buff, '=');
    sprintf(cbuff,
            "%-33s%-11s%-11s%-17s%-11s\n",
            "Name",
            "Kind",
            "Level",
            "Type",
            "Attribute");
    buff += cbuff;
    dumpDemarcation(buff, '-');

    for (const SymbolTableRow &row : rows) {
        sprintf(cbuff, "%-33s", row.getName());
        buff += cbuff;
        sprintf(cbuff, "%-11s", ktoa(row.getKind()));
        buff += cbuff;
        if (row.getLevel()) {
            sprintf(cbuff, "%d%-10s", row.getLevel(), "(local)");
        } else {
            sprintf(cbuff, "%d%-10s", 0, "(global)");
        }
        buff += cbuff;
        sprintf(cbuff, "%-17s", row.getType().getPTypeCString());
        buff += cbuff;
        sprintf(cbuff, "%-11s", row.getAttribute().getContentCString());
        buff += cbuff;
        buff.push_back('\n');
    }
    dumpDemarcation(buff, '-');
}

SymbolTableRow::SymbolTableRow(const char *_name, p_symbol_kind _kind,
                               int _level, const PType &_type,
                               const AttributeContent &_attr,
                               const Location &_location,
                               bool _eff)
    : name(_name), kind(_kind), level(_level), type(_type), attribute(_attr), location(_location), effective(_eff) {}

const char *SymbolTableRow::getName() const {
    return name.c_str();
}

const bool &SymbolTableRow::isEffective() const {
    return effective;
}

const int &SymbolTableRow::getLevel() const { return level; }

const PType &SymbolTableRow::getType() const { return type; }

const p_symbol_kind &SymbolTableRow::getKind() const { return kind; }

const AttributeContent &SymbolTableRow::getAttribute() const {
    return attribute;
}

const std::vector<SymbolTableRow> &SymbolTable::getContent() const {
    return rows;
}

std::vector<SymbolTableRow> &SymbolTable::getContent() { return rows; }

const std::unordered_set<std::string> &SymbolTable::getKeySet() const {
    return key_set;
}

std::unordered_set<std::string> &SymbolTable::getKeySet() {
    return key_set;
}

void SymbolTableRow::setKind(p_symbol_kind _kind) {
    kind = _kind;
}

void SymbolTableRow::setAttribute(const char *_attr) {
    attribute = AttributeContent(_attr);
}

const Location &SymbolTableRow::getLocation() const {
    return location;
}

AttributeContent::AttributeContent(const char *attr)
    : is_const(true), type_val({}), const_val(attr) {}

AttributeContent::AttributeContent(const std::vector<PType> &types)
    : is_const(false), type_val(types), const_val("") {}

bool AttributeContent::isConstant() const { return is_const; }

const std::vector<PType> &AttributeContent::getTypes() const { return type_val; }

const char *AttributeContent::getContentCString() const {
    if (is_const) {
        return const_val.c_str();
    }  //
    else {
        if (!is_type_val_ready) {
            for (const PType &t : type_val) {
                type_val_str += t.getPTypeCString();
                type_val_str += ", ";
            }
            if (type_val.size() > 0) {
                type_val_str.pop_back();
                type_val_str.pop_back();
            }
            is_type_val_ready = true;
        }

        return type_val_str.c_str();
    }
}
