#include "sema/SemanticAnalyzer.hpp"

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
    push();
    insert({p_program.getNameCString(),
            PK_PROGRAM,
            currentLevel,
            p_program.getTypeCString(),
            "",
            p_program.getLocation()});

    // 3.
    currentScopeKind = PK_UNKNOWN;
    p_program.visitChildNodes(*this);

    // 4.
    // TODO

    // 5.
    pop();
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
    insert({p_variable.getNameCString(),
            kind,
            currentLevel,
            p_variable.getTypeCString(),
            "",
            p_variable.getLocation()});

    stashRow();
    p_variable.visitChildNodes(*this);
    popRow();
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

    const char *val = p_constant_value.getConstantValueCString();
    topRow().setAttribute(val);
    topRow().setKind(PK_CONSTANT);
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
            p_function.getTypeCString(),
            p_function.getParameterCString(),
            p_function.getLocation()});

    push();
    p_symbol_kind prevScopeKind = currentScopeKind;
    currentScopeKind = PK_FUNTION;
    preserveLevel();

    p_function.visitChildNodes(*this);

    currentScopeKind = prevScopeKind;
    pop();
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
    push();
    p_compound_statement.visitChildNodes(*this);
    pop();
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
}

void SemanticAnalyzer::visit(FunctionInvocationNode &p_func_invocation) {
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

    push();
    const VariableNode &varNode = p_for.getLoopVarNode();
    insert({varNode.getNameCString(),
            PK_LOOP_VAR,
            currentLevel,
            "integer",
            "",
            varNode.getLocation()});
    reserve(varNode.getNameCString());

    p_for.visitBodyNode(*this);

    release(varNode.getNameCString());
    pop();
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
}

void SemanticAnalyzer::push() {
    if (levelPreserved > 0) {
        levelPreserved--;
        tableStack.push_back(nullptr);
    } else {
        tableStack.push_back(new SymbolTable());
        effectiveTableStack.push_back(tableStack.back());
        currentLevel++;
    }
}

void SemanticAnalyzer::pop() {
    SymbolTable *t = tableStack.back();
    if (t) {
        effectiveTableStack.pop_back();
        t->print();
        delete t;
        currentLevel--;
    }
    tableStack.pop_back();
}

SymbolTable &SemanticAnalyzer::top() {
    return *effectiveTableStack.back();
}

const SymbolTable &SemanticAnalyzer::top() const {
    return *effectiveTableStack.back();
}

void SemanticAnalyzer::insert(const SymbolTableRow &row) {
    // heck key collision
    std::string key = row.getName();
    if (top().getKeySet().count(key) || reserved.count(key)) {
        std::string errMsg = "symbol '";
        errMsg += row.getName();
        errMsg += "' is redeclared";
        result.push_back({row.getLocation(),
                          errMsg});
        return;
    }

    top().getContent().push_back(row);
    top().getKeySet().insert(key);
}

void SemanticAnalyzer::stashRow() {
    tableRowStack.push_back(&top().getContent().back());
}

void SemanticAnalyzer::popRow() {
    tableRowStack.pop_back();
}

SymbolTableRow &SemanticAnalyzer::topRow() {
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
        printf("%-11s", row.getKind());
        if (row.getLevel()) {
            printf("%d%-10s", row.getLevel(), "(local)");
        } else {
            printf("%d%-10s", 0, "(global)");
        }
        printf("%-17s", row.getType());
        printf("%-11s", row.getAttribute());
        puts("");
    }
    dumpDemarcation('-');
}

SymbolTableRow::SymbolTableRow(const char *_name, p_symbol_kind _kind,
                               int _level, const char *_type, const char *_attr,
                               const Location &_location)
    : name(_name), kind(_kind), level(_level), type(_type), attribute(_attr), location(_location) {}

const char *SymbolTableRow::getName() const {
    return name.c_str();
}

const char *SymbolTableRow::getKind() const {
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

const int &SymbolTableRow::getLevel() const {
    return level;
}

const char *SymbolTableRow::getType() const {
    return type.c_str();
}

const char *SymbolTableRow::getAttribute() const {
    return attribute.c_str();
}

const std::vector<SymbolTableRow> &SymbolTable::getContent() const {
    return rows;
}

std::vector<SymbolTableRow> &SymbolTable::getContent() {
    return rows;
}

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
    attribute = _attr;
}

const Location &SymbolTableRow::getLocation() const {
    return location;
}
