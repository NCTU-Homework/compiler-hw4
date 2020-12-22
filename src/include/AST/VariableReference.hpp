#ifndef __AST_VARIABLE_REFERENCE_NODE_H
#define __AST_VARIABLE_REFERENCE_NODE_H

#include <memory>
#include <vector>

#include "AST/expression.hpp"

typedef std::vector<std::unique_ptr<ExpressionNode>> Exprs;
class VariableReferenceNode : public ExpressionNode {
   public:
    VariableReferenceNode(const uint32_t line, const uint32_t col,
                          const char *p_name);
    VariableReferenceNode(const uint32_t line, const uint32_t col,
                          const char *p_name, Exprs *p_indices);
    ~VariableReferenceNode() = default;

    const char *getNameCString() const;

    void accept(AstNodeVisitor &p_visitor) override;
    void visitChildNodes(AstNodeVisitor &p_visitor) override;
    const Exprs &getArrayIndicies() const;

   private:
    const std::string name;
    Exprs indices;
};

#endif
