#include "sema/SemanticAnalyzer.hpp"

#include <algorithm>
#include <cassert>

#include "visitor/AstNodeInclude.hpp"

void SemanticAnalyzer::visit(ProgramNode &p_program) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    // 1.2.
    startScope();
    insert({p_program.getNameCString(),
            PK_PROGRAM,
            currentLevel,
            p_program.getType(),
            "",
            p_program.getLocation()});

    // 3.
    currentScopeKind = PK_UNKNOWN;
    p_program.visitChildNodes(*this);

    // 4.
    // TODO

    // 5.
    endScope();
    // assert(expTypeStack.empty());
}

void SemanticAnalyzer::visit(DeclNode &p_decl) {
    p_decl.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(VariableNode &p_variable) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    // If is array then length must be positive
    for (int len : p_variable.getType().getDimensions()) {
        if (len <= 0) {
            std::string msg = "'";
            msg += p_variable.getNameCString();
            msg += "' declared as an array with an index that is not greater than 0";
            result.push_back({p_variable.getLocation(),
                              msg});
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
                p_variable.getLocation()});
    } else {
        insert({p_variable.getNameCString(),
                kind,
                currentLevel,
                p_variable.getType(),
                "",
                p_variable.getLocation()});
    }
}

void SemanticAnalyzer::visit(ConstantValueNode &p_constant_value) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    //const char *val = p_constant_value.getConstantValueCString();
    //topRow().setAttribute(val);
    //topRow().setKind(PK_CONSTANT);
    pushExpType({p_constant_value.getLocation(), *p_constant_value.getTypePtr()});
}

void SemanticAnalyzer::visit(FunctionNode &p_function) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */
    insert({p_function.getNameCString(),
            PK_FUNTION,
            currentLevel,
            p_function.getType(),
            p_function.getPrototype(),
            p_function.getLocation()});

    startScope();
    p_symbol_kind prevScopeKind = currentScopeKind;
    currentScopeKind = PK_FUNTION;
    preserveLevel();

    p_function.visitChildNodes(*this);

    currentScopeKind = prevScopeKind;
    endScope();
}

void SemanticAnalyzer::visit(CompoundStatementNode &p_compound_statement) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_symbol_kind tmp = currentScopeKind;
    currentScopeKind = PK_UNKNOWN;
    startScope();
    p_compound_statement.visitChildNodes(*this);
    endScope();
    currentScopeKind = tmp;
}

void SemanticAnalyzer::visit(PrintNode &p_print) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_print.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(BinaryOperatorNode &p_bin_op) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

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
        pushExpType({p_bin_op.getLocation(), Prim::kUnknown});
    };

    if (letype.getDimensions().size() > 0 || retype.getDimensions().size() > 0) {
        gg();
        return;
    }

    const Prim &rtype = retype.getPrimitiveType();
    const Prim &ltype = letype.getPrimitiveType();
    // skip already described error
    if (ltype == Prim::kUnknown || rtype == Prim::kUnknown) {
        return;
    }

    switch (p_bin_op.getOperator()) {
        case Operator::kMultiplyOp:
        case Operator::kDivideOp:
        case Operator::kMinusOp:
            if (ltype == Prim::kRealType && rtype == Prim::kRealType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType});
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
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType});
            } else {
                gg();
            }
            break;
        case Operator::kPlusOp:
            if (ltype == Prim::kRealType && rtype == Prim::kRealType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (ltype == Prim::kRealType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (rtype == Prim::kRealType && ltype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kRealType});
            } else if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType});
            } else if (ltype == Prim::kStringType && rtype == Prim::kStringType) {
                pushExpType({p_bin_op.getLocation(), Prim::kStringType});
            } else {
                gg();
            }
            break;
        case Operator::kModOp:
            if (ltype == Prim::kIntegerType && rtype == Prim::kIntegerType) {
                pushExpType({p_bin_op.getLocation(), Prim::kIntegerType});
            } else {
                gg();
            }
            break;
        case Operator::kAndOp:
        case Operator::kOrOp:
        case Operator::kNotOp:
            if (ltype == Prim::kBoolType && rtype == Prim::kBoolType) {
                pushExpType({p_bin_op.getLocation(), Prim::kBoolType});
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
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_un_op.visitChildNodes(*this);

    PType ptype = popExpType().type;
    auto gg = [&]() {
        std::string errMsg = "invalid operands to binary operator '";
        errMsg += p_un_op.getOpCString();
        errMsg += "' ('";
        errMsg += ptype.getPTypeCString();
        errMsg += "' and '";
        errMsg += ptype.getPTypeCString();
        errMsg += "')";
        result.push_back({p_un_op.getLocation(),
                          errMsg});
        pushExpType({p_un_op.getLocation(), Prim::kUnknown});
    };

    if (ptype.getDimensions().size() > 0 || ptype.getDimensions().size() > 0) {
        gg();
        return;
    }

    const Prim &stype = ptype.getPrimitiveType();
    // skip already described error
    if (stype == Prim::kUnknown) {
        return;
    }

    switch (p_un_op.getOperator()) {
        case Operator::kNegOp:
            if (stype == Prim::kIntegerType) {
                pushExpType({p_un_op.getLocation(), Prim::kIntegerType});
            } else if (stype == Prim::kRealType) {
                pushExpType({p_un_op.getLocation(), Prim::kRealType});
            } else {
                gg();
            }
            break;
        case Operator::kNotOp:
            if (stype == Prim::kBoolType) {
                pushExpType({p_un_op.getLocation(), Prim::kBoolType});
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
    // return;

    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    // Checks if such function reference exists
    if (!refer(p_func_invocation.getNameCString())) {
        std::string msg = "use of undeclared symbol '";
        msg += p_func_invocation.getNameCString();
        msg += "'";
        result.push_back({p_func_invocation.getLocation(),
                          msg});
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown});
        return;
    }

    // Checks if such function is function
    const SymbolTableRow &row = reference(p_func_invocation.getNameCString());
    if (row.getKind() != PK_FUNTION) {
        std::string msg = "use of non-function symbol '";
        msg += p_func_invocation.getNameCString();
        msg += "'";
        result.push_back({p_func_invocation.getLocation(),
                          msg});
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown});
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
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown});
        return;
    }

    p_func_invocation.visitChildNodes(*this);

    // Check if argument type matches
    std::vector<ExpressionType> args;
    for (int i = 0; i < paramTypes.size(); i++) {
        args.push_back(popExpType());
    }
    reverse(args.begin(), args.end());

    int ok = 1;
    for (int i = 0; i < paramTypes.size(); i++) {
        PType argType = args[i].type;
        PType paramType = paramTypes[i];
        if (argType != paramType) {
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
        pushExpType({p_func_invocation.getLocation(), row.getType()});
    }
    else {
        pushExpType({p_func_invocation.getLocation(), Prim::kUnknown});
    }
}

void SemanticAnalyzer::visit(VariableReferenceNode &p_variable_ref) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    // Checks if the variable is already defined
    if (!refer(p_variable_ref.getNameCString())) {
        std::string msg = "use of undeclared symbol '";
        msg += p_variable_ref.getNameCString();
        msg += "'";
        result.push_back({p_variable_ref.getLocation(),
                          msg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown});
        return;
    }

    // Checks if such reference is of type varirable
    const SymbolTableRow &var = reference(p_variable_ref.getNameCString());
    if (var.getKind() == PK_FUNTION || var.getKind() == PK_PROGRAM || var.getKind() == PK_UNKNOWN) {
        std::string msg = "use of non-variable symbol '";
        msg += p_variable_ref.getNameCString();
        msg += "'";
        result.push_back({p_variable_ref.getLocation(),
                          msg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown});
        return;
    }

    p_variable_ref.visitChildNodes(*this);

    // Checks if array index is integer
    int arrayLen = p_variable_ref.getArrayIndicies().size();
    for (int i = 0; i < arrayLen; i++) {
        ExpressionType et = popExpType();
        if (et.type.getDimensions().size() > 0 || et.type.getPrimitiveType() != Prim::kIntegerType) {
            std::string errMsg = "index of array reference must be an integer";
            result.push_back({et.location,
                              errMsg});
            pushExpType({p_variable_ref.getLocation(), Prim::kUnknown});
            return;
        }
    }

    // Checks over array subscript
    int al = reference(p_variable_ref.getNameCString()).getType().getDimensions().size();
    if (arrayLen > al) {
        std::string errMsg = "there is an over array subscript on '";
        errMsg += p_variable_ref.getNameCString();
        errMsg += "'";
        result.push_back({p_variable_ref.getLocation(),
                          errMsg});
        pushExpType({p_variable_ref.getLocation(), Prim::kUnknown});
        return;
    }

    const SymbolTableRow &row = reference(p_variable_ref.getNameCString());
    PType type = row.getType();
    const std::vector<uint64_t> &dim = type.getDimensions();
    const std::vector<uint64_t> newdim(dim.begin() + arrayLen, dim.end());
    PType newType(type.getPrimitiveType());
    newType.setDimensions(newdim);
    pushExpType({p_variable_ref.getLocation(), newType});
}

void SemanticAnalyzer::visit(AssignmentNode &p_assignment) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_assignment.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(ReadNode &p_read) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_read.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(IfNode &p_if) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_if.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(WhileNode &p_while) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_while.visitChildNodes(*this);
}

void SemanticAnalyzer::visit(ForNode &p_for) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    startScope();
    const VariableNode &varNode = p_for.getLoopVarNode();
    insert({varNode.getNameCString(),
            PK_LOOP_VAR,
            currentLevel,
            PType(PType::PrimitiveTypeEnum::kIntegerType),
            "",
            varNode.getLocation()});
    reserve(varNode.getNameCString());

    p_for.visitBodyNode(*this);

    release(varNode.getNameCString());
    endScope();
}

void SemanticAnalyzer::visit(ReturnNode &p_return) {
    /*
     * TODO:
     *
     * 1. Push a new symbol table if this node forms a scope.
     * 2. Insert the symbol into current symbol table if this node is related to
     *    declaration (ProgramNode, VariableNode, FunctionNode).
     * 3. Travere child nodes of this node.
     * 4. Perform semantic analyses of this node.
     * 5. Pop the symbol table pushed at the 1st step.
     */

    p_return.visitChildNodes(*this);
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
        t->print();
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

void SemanticAnalyzer::pushExpType(const ExpressionType &type) {
    expTypeStack.push_back(type);
}

ExpressionType SemanticAnalyzer::popExpType() {
    assert(!expTypeStack.empty());
    ExpressionType ret = expTypeStack.back();
    expTypeStack.pop_back();
    return ret;
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

void dumpDemarcation(const char chr) {
    for (size_t i = 0; i < 110; ++i) {
        printf("%c", chr);
    }
    puts("");
}

void SymbolTable::print() const {
    dumpDemarcation('=');
    printf("%-33s%-11s%-11s%-17s%-11s\n",
           "Name",
           "Kind",
           "Level",
           "Type",
           "Attribute");
    dumpDemarcation('-');
    for (const SymbolTableRow &row : rows) {
        printf("%-33s", row.getName());
        printf("%-11s", ktoa(row.getKind()));
        if (row.getLevel()) {
            printf("%d%-10s", row.getLevel(), "(local)");
        } else {
            printf("%d%-10s", 0, "(global)");
        }
        printf("%-17s", row.getType().getPTypeCString());
        printf("%-11s", row.getAttribute().getContentCString());
        puts("");
    }
    dumpDemarcation('-');
}

SymbolTableRow::SymbolTableRow(const char *_name, p_symbol_kind _kind,
                               int _level, const PType &_type,
                               const AttributeContent &_attr,
                               const Location &_location)
    : name(_name), kind(_kind), level(_level), type(_type), attribute(_attr), location(_location) {}

const char *SymbolTableRow::getName() const {
    return name.c_str();
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
