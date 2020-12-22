#ifndef __AST_FUNCTION_INVOCATION_NODE_H
#define __AST_FUNCTION_INVOCATION_NODE_H

#include <memory>
#include <vector>

#include "AST/expression.hpp"

typedef std::vector<std::unique_ptr<ExpressionNode>> Exprs;
class FunctionInvocationNode : public ExpressionNode {
   public:
    FunctionInvocationNode(const uint32_t line, const uint32_t col,
                           const char *p_name, Exprs *p_arguments);
    ~FunctionInvocationNode() = default;

    const char *getNameCString() const;

    void accept(AstNodeVisitor &p_visitor) override;
    void visitChildNodes(AstNodeVisitor &p_visitor) override;
    const Exprs &getArguments() const;

   private:
    const std::string name;
    Exprs arguments;
};

#endif
