#ifndef __SEMA_SEMANTIC_ANALYZER_H
#define __SEMA_SEMANTIC_ANALYZER_H

#include <string>
#include <unordered_set>
#include <vector>

#include "AST/PType.hpp"
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

class AttributeContent {
   public:
    bool isConstant() const;
    const char *getContentCString() const;
    AttributeContent(const char *);
    AttributeContent(const std::vector<PType> &);
    const std::vector<PType> &getTypes() const;

   private:
    bool is_const;
    std::vector<PType> type_val;
    mutable bool is_type_val_ready = false;
    mutable std::string type_val_str = "";
    std::string const_val;
};

class SymbolTableRow {
   private:
    std::string name;
    p_symbol_kind kind;
    int level;
    PType type;
    AttributeContent attribute;
    Location location;

   public:
    SymbolTableRow(const char *_name, p_symbol_kind _kind, int _level,
                   const PType &_type, const AttributeContent &_attr,
                   const Location &_location);
    ~SymbolTableRow() = default;
    const char *getName() const;
    const p_symbol_kind &getKind() const;
    const int &getLevel() const;
    const PType &getType() const;
    const AttributeContent &getAttribute() const;
    const Location &getLocation() const;

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
    const std::unordered_set<std::string> &getKeySet() const;
    void print() const;
};

struct SemanticError {
    Location location;
    std::string errorMsg;
};

struct ExpressionType {
    Location location;
    PType type;
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

    void startScope();
    void endScope();
    void reserve(const char *keyName);
    void release(const char *keyName);
    void insert(const SymbolTableRow &row);
    void stashRow();
    void popRow();
    SymbolTableRow &topRow();
    void preserveLevel();
    SymbolTable &currentScopeTable();
    const SymbolTable &currentScopeTable() const;

    bool refer(const char *key) const;
    const SymbolTableRow &reference(const char *key) const;

    std::vector<SemanticError> result;
    ExpressionType popExpType();
    void pushExpType(const ExpressionType &);

   private:
    std::vector<SymbolTable *> effectiveTableStack;
    std::vector<SymbolTable *> tableStack;
    std::vector<SymbolTableRow *> tableRowStack;
    std::unordered_set<std::string> reserved;
    std::vector<ExpressionType> expTypeStack;
    p_symbol_kind currentScopeKind;
    int currentLevel = -1;
    int levelPreserved = 0;
};

#endif
