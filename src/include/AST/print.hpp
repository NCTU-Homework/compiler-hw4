#ifndef __AST_PRINT_NODE_H
#define __AST_PRINT_NODE_H

#include <memory>

#include "AST/ast.hpp"
#include "AST/expression.hpp"

class PrintNode : public AstNode {
   public:
    PrintNode(const uint32_t line, const uint32_t col, ExpressionNode *p_expr);
    ~PrintNode() = default;

    void accept(AstNodeVisitor &p_visitor) override;
    void visitChildNodes(AstNodeVisitor &p_visitor) override;

   private:
    std::unique_ptr<ExpressionNode> target;
};

#endif
