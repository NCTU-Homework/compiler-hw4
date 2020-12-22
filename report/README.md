# hw4 report

|      |         |
| ---: | :------ |
| Name | 曾文鼎  |
|   ID | 0716023 |

## How much time did you spend on this project

~24 hours.

## Project overview

我沒有沿用自己 HW3 寫的 code ，而是用助教給的，因為自己寫的沒有助教的來得有彈性。

實作內容主要集中在 SemantciAnalyzer.cpp 中。此外，各 AstNode 檔案依使用需求擴充功能，但助教給的既有程式碼原則上沒有刪除或修改。最後， scanner.l 和 parser.y 也有小幅度的修改。

以下我將只講解我如何透過修改 SemantciAnalyzer.cpp 來實作 symbol table 。對於 AstNode 的一切改動將省略不提，因為這些擴充功能充其量只是應付 `SemantciAnalyzer` 的需求，其中沒有任何有關編譯器的技術含量。

在以下的講解，我會擷取部分我實作的程式碼做說明。但程式碼將會做某些幅度的修改或刪減，弄成有點類似 pseudo code 的感覺以利閱讀。閱讀時建議不要鑽研細部到底怎麼實作的，只要知道原理就好。

### Symbol Table Basics

要製作 symbol table ，需要先定義基礎類別。

- 定義 `SymbolTableRow` 用以記錄 symbol table 中的一筆變數的紀錄。
- 定義 `SymbolTable` 用以記錄一張 symbol table 。

### Lifecycle of Symbol Table

令 SemanticAnalyzer 含有一存放 `SymbolTable` 的堆疊 。當一個新的 scope 開始時，將一張空的 `SymbolTable` 放入此堆疊中；當 scope 結束時，將此 `SymbolTable` 從堆疊中移除並棄置。此外， SemantciAnalyzer 提供一個方法，能夠將一給定的 symbol table record 放入到 symbol table stack 最上方之 symbol table 中。

以下示範 symbol table 的生命週期。

```cpp
void SemanticAnalyzer::visit(ProgramNode &p_program) {
    // Put an empty symbol table into the stack
    startScope();
    // Insert a symbol table row into the symbol table at the top of the stack
    insert({p_program.getNameCString(), // name of var
            PK_PROGRAM,                 // kind of var
            currentLevel,               // current level
            p_program.getType(),        // type of var
            ""});                       // attribute of var

    // Some codes are omitted for simplicity...

    // Remove the symbol table from the stack
    endScope();
}
```

### Level Detection

應作業需求，我們需要在插入 symbol table row 時，能夠知道當前的 level 。

我們可以定義一開始的 level 為 0 ，然後當一個新的 scope 開始或結束時，令其 +1 或 -1 。因此只要將 level 的更動實作在 `startScope` 和 `endScope` 即可。

目前已知會建立 scope 的節點有

- `ProgramNode`
- `FunctionNode`
- `CompoundStatementNode`
- `ForNode`

其中要注意的是， `FunctionNode` 總是有一個 `CompoundStatementNode` 作為子節點，但後者在此時不會觸發新的
scope 。我們可以定義一函數 `reserveLevel()` ，此函數會抑制接下來的 `newScope` 不會產生新的 symbol table 。

```cpp
void SemanticAnalyzer::visit(FunctionNode &p_function) {
    // Start a new scope for for-loop
    startScope();
    // Reserve a level
    reserveLevel();

    // ...

    // The scope has ended
    endScope();
}

void SemanticAnalyzer::visit(CompoundStatementNode &p_compound_statement) {
    // Assumed that this compound statement is just after a function declaration
    // Create a new scope. However, a new symbol table is not created, since
    // we have reserved one; hence the level does not increment.
    startScope();

    // ...

    // No symbol table is removed, since the previouly called `startScope`
    // did not create a symbol table; hence the level does not decrement.
    endScope();
}

void SemanticAnalyzer::startScope() {
    if (levelReserved > 0) {
        // If a level is reserved, put a dummy symbol table
        levelPreserved--; // this var indicates the count the level is reserved
        symbolTableStack.push(nullptr);
    }
    else {
        // otherwise create new table and increment the level
        symbolTableStack.push(new SymbolTable());
        currentLevel++; // this var indicates the current level
    }
}

void SemanticAnalyzer::endScope() {
    SymbolTable *t = symbolTableStack.top();
    symbolTableStack.pop();
    if (t) {
        // If this is not a dummy symbol table, decrement the level
        delete t;
        currentLevel--;
    }
}
```

### Invalid Identifier Detection

在這份作業中，我們需要對變數或方法名稱做合法判斷。這些名稱都會記錄在 symbol table 中，我們可以查找其中的紀錄來做鑑定。要判斷是否重複宣告，可以先查找是否已有該名稱存在於任何當前的 symbol table 中。我們不用去翻 level 較低的 scope ，因為較高 level 的 variable 會遮蔽較低的 level ，這是合法的。

然而， for 迴圈的疊代變數不允許在較高 level 中的 scope 。我們可以定義一個 `lockIdentifier(const char*)` 表示此 identifier 不得再被宣告，並定義 `unlockIdentifier(cosnt char*)` 解除對上述命令。當 for scope 開始時，對其疊代變數名稱上鎖；當 for scope 結束時解鎖。

我們可以透過實作 `insert` 函數，令合法的 row 才能被加入當前的 symbol table 中，否則記錄錯誤訊息。

```cpp
void SemanticAnalyzer::insert(const SymbolTableRow &row) {
    // Get identifier name
    std::string key = row.getName();
    if (
        currentSymbolTable().has(key) // Require identifier not in current scope
        || lockedKeys.has(key)        // Require identifier not locked
    ) {
        // Redeclaration detected, print error message...
        // ...
    }
    else {
        // Insert this row to current symbol table
        SymbolTable& t = getCurrentSymbolTable();
        t.push_back(row);
    }
}

void SemanticAnalyzer::visit(ForNode &p_for) {
    startScope();

    // Lock the varirable so that further symbol table cannot have this id
    const char* varName = p_for.getVarNode().getNameCString();
    lockIdentifier(varName);

    // ...

    // Unlock
    unlockIdentifier(varName);

    endScope();
}
```

此外，對變數做宣告時，若其類別為陣列，則其長度須為正數，且要檢查其是否有 over subscript 的問題。我對 `VariableNode` 做修改，讓外部可以撈到其中陣列各維度的長度的資料，可以輕鬆地做檢查。

另外，若一個變數在宣告時有問題，則任何參考此變數的衍生錯誤都要忽略。要做到這項功能，我們可以在 symbol table row 中對其做一個失敗標記。當有衍生錯誤發生時，先檢查此失敗標記是否被設立；如果有，則忽略之。

```cpp
void SemanticAnalyzer::visit(VariableNode &p_variable) {
    // ...

    // Get the length of all dimensions
    for (int arrLen : p_variable.getType().getArrayDimensions()) {
        if (arrLen <= 0) {
            // Bad array length detected, print error message...
            // ...
            // Update the flag
            ok = false;
            break;
        }
    }

    // If the variable is declared with some problem, set a failure flag
    SymBolTableRow row = { /* ... */ };
    if (!ok) row.setFailureFlag();
    insert(row);

    // ...
}

void SemanticAnalyzer::visit(VariableReferenceNode &p_variable_ref) {
    const char* varName = p_variable_ref.getNameCString();

    // Check if this identidier exists...
    // Check if this identifier is of type variable...

    // Get the symbol table row of this variable reference
    const SymbolTableRow& row = getSymbolTableRow(varName);

    // Checks if this varirable is declared correctly
    if (row.isFailureFlagSet()) {
        // Do not do further error check
        return;
    }

    // Check over subscript...

    // ...
}
```

### Type Check

這個作業的一大主題就是型別檢查。我們需要掌握每一個 `ExpressionNode` 所代表的型別，若該節點經處理後發現有型別錯誤操作的問題，也要能夠掌握。

我透過宣告一個堆疊來解決此問題。每當 `SemanticAnalyzer` 造訪完一個 `ExpressionNode` 後，我將其代表的型別放入一指定堆疊中。當我對運算子作操作時，我可以從堆疊中準確地取出 operand 的型別，這是利用 `ExpressionNode` 的直接子節點必為其 operand 的特性。

當 `SemanticAnalyzer` 造訪完一個 `ExpressionNode` 後，若判定其型別不正確，則在堆疊中放入一稱為「錯誤型別」的 dummy data 並輸出錯誤資訊；否則放入其型別。當我要對運算子做分析時，若發現其 operand 為「錯誤型別」，則定義此運算子的運算結果亦為錯誤型別，且此時不必再輸出錯誤資訊，因為參考錯誤的資料是沒有意義的。

```cpp
void SemanticAnalyzer::visit(BinaryOperatorNode &p_bin_op) {
    // First, iterate all its children nodes so that types of operands
    // are collected.
    p_bin_op.visitChildNodes(*this);

    // Get result type (since rval is visited later than lval, the result type
    // of the former will be located on top of that of the latter)
    PType rtype = popExpressionResultType();
    PType ltype = popExpressionResultType();

    // If either lval or rval has error type, do not print error message
    // and let this operation has error type as well
    if (ltype == ERROR_TYPE || rtype == ERROR_TYPE) {
        pushExpressionResultType(ERROR_TYPE);
        return;
    }

    PType resultType = ERROR_TYPE;
    switch (p_bin_op.getOperator()) {
    case kMinusOp:
        if (!ltype.isArray() && !rtype.isArray()) {
            if (ltype == kRealType && rtype == kRealType) {
                resultType = kRealType;
            } else if (ltype == kRealType && rtype == kIntegerType) {
                resultType = kRealType;
            } else if (rtype == kRealType && ltype == kIntegerType) {
                resultType = kRealType;
            } else if (ltype == kIntegerType && rtype == kIntegerType) {
                resultType = kIntegerType;
            }
        }
        break;
    // Omit other operators...
    }

    if (resultType == ERROR_TYPE) {
        // If the resulting type is error, print error message
        // ...
    }
    pushExpressionResultType(resultType);
}
```

要檢測 return 敘述，就需要知道當前 scope 下正確的 return type 。這同樣可以透過維護一個堆疊完成。當 SematicAnaylzer 進入 `ProgramNode` 或 `FunctionNode` 時，將正確的回傳執行太放入依指定堆疊中。當離開 `ProgramNode` 或 `FunctionNode` 時，則將其取出堆疊。堆疊最上方之資料型態，就是當前正確的回傳型態。此部分不再贅述。

### Semantic Check Summation

以下給出對兩種節點的造訪的實作方式作為總結，其中使用了各種在上述提到的技巧。

```cpp
void SemanticAnalyzer::visit(FunctionNode &p_function) {
    // Insert a new symbol table row
    SymbolTableRow row = { /* omit */ };
    insert(row);
    // Push a new symbol table into the stack and increment the current level
    startScope();
    // Push the correct return type into some stack so that we can check 
    // `return` statement 
    beginProcedure(p_function.getReturnType());
    // There will be a compound statement right after this declaration,
    // but it does not increment the level, hence reserve a level.
    reserveLevel();

    p_function.visitChildNodes(*this);
    
    endProcedure();
    endScope();
}

void SemanticAnalyzer::visit(FunctionInvocationNode &p_func_invocation) {
    // Collect argument data types
    p_func_invocation.visitChildNodes(*this);

    const funcName = p_func_invocation.getNameCString();

    // Check if such identifer reference exists
    if (!hasSymbolTableRow(funcName)) {
        // print error message
        // ...
        // This expression results in error type
        pushExpType(ERROR_TYPE);
        return;
    }

    // Checks if such identirfer is of type function
    const SymbolTableRow& row = getSymbolTableRow(varName);
    if (row.getKind() != PK_FUNTION) {
        // print error message
        // ...
        // This expression results in error type
        pushExpType(ERROR_TYPE);
        return;
    }

    const std::vector<PType> &args = p_func_invocation.getArguments();
    const std::vector<PType> &params = row.getAttribute().getParameters();

    // Checks if argument count matches
    if (args.size() != params.size()) {
        // print error message
        // ...
        // This expression results in error type
        pushExpType(ERROR_TYPE);
        return;
    }

    // Check if argument type matches
    int n = params.size();
    std::vector<PType> argsResultTypes;
    for (int i = 0; i < n; i++) {
        argsResultTypes.push_back(popExpressionType());
    }

    PType resultType = p_func_invocation.getReturnType();
    for (int i = 0; i < n; i++) {
        PType argType = argsResultTypes[n - i - 1];
        PType paramType = params[i].getType();
        if (argType == ERROR_TYPE) {
            // Do not print error message
            resultType = ERROR_TYPE
        }
        else if (argType != paramType) {
            // print error message...
            // ...
            resultType = ERROR_TYPE
        }
    }

    pushExpressionType(resultType);
}
```

## What is the hardest you think in this project

處理哪些錯誤要印、哪些不用印， SPEC 看得不是很懂，覺得很困難啊 = =

## Feedback to T.A.s

NA
