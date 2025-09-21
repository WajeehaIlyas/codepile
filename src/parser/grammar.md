Formal grammar for the language

**Notation:**
- `{ ... }` : Zero or more repetitions
- `[ ... ]` : Zero or one (optional)
- `|` : A choice between alternatives

## 1. Program Structure

`Program -> { Declaration }*`

## 2. Declarations

`Declaration -> FunctionDecl | VariableDecl`

`FunctionDecl -> Type Identifier '(' [Parameters] ')' Block`

`Parameters -> Type Identifier { ',' Type Identifier }*`

`VariableDecl -> Type Declarator { ',' Declarator }* ';'`

## 3. Statements

`Block -> '{' { Statement }* '}'`

`Statement -> ReturnStmt | IfStmt | LoopStmt | SwitchStmt | ExpressionStmt | BreakStmt | ContinueStmt`

`ReturnStmt -> 'return' [Expression] ';'`

`IfStmt -> 'if' '(' Expression ')' Block [ 'else' Block ]`

`LoopStmt -> WhileStmt | ForStmt | DoWhileStmt`

`WhileStmt -> 'while' '(' Expression ')' Block`

`ForStmt -> 'for' '(' [Expression] ';' [Expression] ';' [Expression] ')'   Block`

`DoWhileStmt -> 'do' Block 'while' '(' Expression ')' ';'`

`SwitchStmt -> 'switch' '(' Expression ')' '{' { CaseStmt }* '}'`

`CaseStmt -> 'case' Literal ':' { Statement }* | 'default' ':' { Statement }*`

`ExpressionStmt -> Expression ';'`

`BreakStmt -> 'break' ';'`

`ContinueStmt -> 'continue' ';'`

## 4. Expressions and Precedence

`Expression -> AssignmentExpr`

`AssignmentExpr -> AdditiveExpr [ AssignmentOp AssignmentExpr ]`

`AssignmentOp -> '=' | '+=' | '-=' | '*=' | '/='`

`AdditiveExpr -> MultiplicativeExpr { ('+' | '-') MultiplicativeExpr }*`

`MultiplicativeExpr -> PrimaryExpr { ('*' | '/' | '%') PrimaryExpr }*`

`PrimaryExpr -> Literal | Identifier | '(' Expression ')'`

`Literal -> IntLit | FloatLit`
