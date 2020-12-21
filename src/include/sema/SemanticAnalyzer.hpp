#ifndef __SEMA_SEMANTIC_ANALYZER_H
#define __SEMA_SEMANTIC_ANALYZER_H

#include <string>
#include <unordered_set>
#include <vector>

#include "AST/ast.hpp"
#include "visitor/AstNodeVisitor.hpp"

enum p_symbol_kind {
    PK_PROGRAM,
    PK_FUNTION,
    PK_PARAMETER,
    PK_VARIABLE,
    PK_LOOP_VAR,
    PK_CONSTANT,
    PK_UNKNOWN
};

class SymbolTableRow {
   private:
    std::string name;
    p_symbol_kind kind;
    int level;
    std::string type;
    std::string attribute;
    Location location;

   public:
    SymbolTableRow(const char *_name, p_symbol_kind _kind, int _level,
                   const char *_type, const char *_attr,
                   const Location &_location);
    ~SymbolTableRow() = default;
    const char *getName() const;
    const char *getKind() const;
    const int &getLevel() const;
    const char *getType() const;
    const char *getAttribute() const;
    const Location& getLocation() const;

    void setAttribute(const char *);
    void setKind(p_symbol_kind);
};

class SymbolTable {
   private:
    std::unordered_set<std::string> key_set;
    std::vector<SymbolTableRow> rows;

   public:
    std::vector<SymbolTableRow> &getContent();
    const std::vector<SymbolTableRow> &getContent() const;
    std::unordered_set<std::string> &getKeySet();
    const std::unordered_set<std::string>& getKeySet() const;
    void print() const;
};

struct SemanticError {
    Location location;
    std::string errorMsg;
};

class SemanticAnalyzer : public AstNodeVisitor {
   public:
    SemanticAnalyzer() = default;
    ~SemanticAnalyzer() = default;

    void visit(ProgramNode &p_program) override;
    void visit(DeclNode &p_decl) override;
    void visit(VariableNode &p_variable) override;
    void visit(ConstantValueNode &p_constant_value) override;
    void visit(FunctionNode &p_function) override;
    void visit(CompoundStatementNode &p_compound_statement) override;
    void visit(PrintNode &p_print) override;
    void visit(BinaryOperatorNode &p_bin_op) override;
    void visit(UnaryOperatorNode &p_un_op) override;
    void visit(FunctionInvocationNode &p_func_invocation) override;
    void visit(VariableReferenceNode &p_variable_ref) override;
    void visit(AssignmentNode &p_assignment) override;
    void visit(ReadNode &p_read) override;
    void visit(IfNode &p_if) override;
    void visit(WhileNode &p_while) override;
    void visit(ForNode &p_for) override;
    void visit(ReturnNode &p_return) override;

    void push();
    void pop();
    void reserve(const char *keyName);
    void release(const char *keyName);
    void insert(const SymbolTableRow &row);
    void stashRow();
    void popRow();
    SymbolTableRow &topRow();
    void preserveLevel();

    SymbolTable &top();
    const SymbolTable &top() const;

    std::vector<SemanticError> result;

   private:
    std::vector<SymbolTable *> effectiveTableStack;
    std::vector<SymbolTable *> tableStack;
    std::vector<SymbolTableRow *> tableRowStack;
    std::unordered_set<std::string> reserved;
    p_symbol_kind currentScopeKind;
    int currentLevel = -1;
    int levelPreserved = 0;
};

#endif
