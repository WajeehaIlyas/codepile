
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast_types.h"

extern int yylex();
extern int yyerror(const char *s);
%}

// DEBUG TRACING DIRECTIVE
%define parse.trace

/*
 * Semantic Value Definitions
 */
%union {
    Node node;
    char *str_val;
    long long int_val;
    double float_val;
}

/*
 * Token Definitions (Terminals)
 */

// Keywords
%token FUNCTION INT FLOAT STRING BOOL RETURN IF ELSE WHILE FOR TRUE FALSE

// Identifiers and Literals
%token <str_val> IDENTIFIER
%token <int_val> INTLIT
%token <float_val> FLOATLIT
%token <str_val> STRINGLIT

// Operators and Symbols
%token ASSIGN EQUALS NOTEQUALS LESS GREATER LESSEQ GREATEREQ
%token PLUS MINUS MUL DIV MOD
%token AND OR NOT
%token BITAND BITOR BITXOR BITNOT SHIFTLEFT SHIFTRIGHT
%token INCREMENT DECREMENT PLUSASSIGN MINUSASSIGN

// Delimiters
%token PARENL PARENR BRACEL BRACER BRACKETL BRACKETR
%token COMMA SEMICOLON COLON

// End of File
%token EOF_TOKEN

/*
 * Precedence and Associativity (Lowest to Highest)
 */
%right ASSIGN PLUSASSIGN MINUSASSIGN
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQUALS NOTEQUALS
%left LESS GREATER LESSEQ GREATEREQ
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left MUL DIV MOD
%right NOT BITNOT UMINUS // Unary operators, right associative
%left INCREMENT DECREMENT // Used for postfix
%left PARENL BRACKETL // Function call and array access

/*
 * Type Definitions (Non-terminals)
 */
%type <node> program declaration declaration_list type_specifier
%type <node> function_definition function_signature parameter_list
%type <node> statement_block statement statement_list
%type <node> variable_declaration initializer_declaration for_init_clause
%type <node> expression assignment_expression logic_or_expression logic_and_expression
%type <node> bit_or_expression bit_xor_expression bit_and_expression
%type <node> equality_expression relational_expression shift_expression
%type <node> additive_expression multiplicative_expression unary_expression
%type <node> postfix_expression primary_expression
%type <node> if_statement while_statement for_statement
%type <node> return_statement
%type <node> argument_list
%type <node> expression_opt

/*
 * Grammar Rules
 */
%%

/* 1. Program Structure */

program
    : declaration_list EOF_TOKEN
    { /* Build AST Root */ }
    ;

declaration_list
    : declaration_list declaration
    { /* Append $2 to list $1 */ }
    | declaration
    { /* Start new list */ }
    ;

declaration 
    : variable_declaration SEMICOLON
    { /* Global variable declaration */ }
    | function_definition
    { /* Function definition */ }
    ;

type_specifier
    : INT
    { /* AST type int */ }
    | FLOAT
    { /* AST type float */ }
    | STRING
    { /* AST type string */ }
    | BOOL
    { /* AST type bool */ }
    ;

/* 2. Function Definitions */

function_definition
    : type_specifier IDENTIFIER PARENL parameter_list PARENR statement_block
    { /* Function def: $1 type, $2 name, $4 params, $6 body */ }
    | type_specifier IDENTIFIER PARENL PARENR statement_block
    { /* Function def (no params) */ }
    ;

parameter_list
    : parameter_list COMMA type_specifier IDENTIFIER
    { /* Add parameter $4 to list $1 */ }
    | type_specifier IDENTIFIER
    { /* Start parameter list with $2 */ }
    ;

/* 3. Statements and Blocks */

statement_block
    : BRACEL statement_list BRACER
    { /* Block statement */ }
    | BRACEL BRACER
    { /* Empty block */ }
    ;

statement_list
    : statement_list statement
    { /* Append $2 to $1 */ }
    | statement
    { /* Start list with $1 */ }
    ;

statement
    : variable_declaration SEMICOLON
    | expression SEMICOLON
    | if_statement
    | while_statement
    | for_statement
    | return_statement SEMICOLON
    | statement_block
    | SEMICOLON /* Empty statement */
    ;

/* 4. Declarations and Returns */

initializer_declaration
    : type_specifier IDENTIFIER
    { /* Declaration without assignment */ }
    | type_specifier IDENTIFIER ASSIGN expression
    { /* Declaration with assignment $4 */ }
    ;

variable_declaration
    : initializer_declaration
    { /* Uses initializer_declaration rule */ }
    ;

return_statement
    : RETURN expression
    { /* Return expression $2 */ }
    | RETURN
    { /* Return void/default */ }
    ;

/* 5. Control Flow */

if_statement
    : IF PARENL expression PARENR statement %prec UMINUS
    { /* If $3 then $5 */ }
    | IF PARENL expression PARENR statement ELSE statement
    { /* If $3 then $5 else $7 */ }
    ;

while_statement
    : WHILE PARENL expression PARENR statement
    { /* While $3 do $5 */ }
    ;

for_statement
    : FOR PARENL for_init_clause SEMICOLON expression_opt SEMICOLON expression_opt PARENR statement
    { /* For loop: $3 init, $5 cond, $7 post, $9 body */ }
    ;

for_init_clause
    : initializer_declaration 
    | expression              
    | /* empty */             
    ;

expression_opt
    : expression
    | /* empty */
    ;

/* 6. Expressions (Following C-style precedence) */

expression
    : assignment_expression
    | logic_or_expression
    ;

assignment_expression
    : unary_expression ASSIGN expression
    { /* Assignment */ }
    | unary_expression PLUSASSIGN expression
    | unary_expression MINUSASSIGN expression
    | logic_or_expression
    ;

logic_or_expression
    : logic_or_expression OR logic_and_expression
    | logic_and_expression
    ;

logic_and_expression
    : logic_and_expression AND bit_or_expression
    | bit_or_expression
    ;

bit_or_expression
    : bit_or_expression BITOR bit_xor_expression
    | bit_xor_expression
    ;

bit_xor_expression
    : bit_xor_expression BITXOR bit_and_expression
    | bit_and_expression
    ;

bit_and_expression
    : bit_and_expression BITAND equality_expression
    | equality_expression
    ;

equality_expression
    : equality_expression EQUALS relational_expression
    | equality_expression NOTEQUALS relational_expression
    | relational_expression
    ;

relational_expression
    : relational_expression LESS shift_expression
    | relational_expression GREATER shift_expression
    | relational_expression LESSEQ shift_expression
    | relational_expression GREATEREQ shift_expression
    | shift_expression
    ;

shift_expression
    : shift_expression SHIFTLEFT additive_expression
    | shift_expression SHIFTRIGHT additive_expression
    | additive_expression
    ;

additive_expression
    : additive_expression PLUS multiplicative_expression
    | additive_expression MINUS multiplicative_expression
    | multiplicative_expression
    ;

multiplicative_expression
    : multiplicative_expression MUL unary_expression
    | multiplicative_expression DIV unary_expression
    | multiplicative_expression MOD unary_expression
    | unary_expression
    ;

unary_expression
    : postfix_expression
    | PLUS unary_expression
    | MINUS unary_expression %prec UMINUS
    | NOT unary_expression
    | BITNOT unary_expression
    | INCREMENT unary_expression // Prefix ++
    | DECREMENT unary_expression // Prefix --
    ;

postfix_expression
    : primary_expression
    | postfix_expression INCREMENT /* Postfix ++ */
    | postfix_expression DECREMENT /* Postfix -- */
    | postfix_expression PARENL argument_list PARENR /* Function call with args */
    | postfix_expression PARENL PARENR /* Function call with no args */
    | postfix_expression BRACKETL expression BRACKETR /* Array access */
    ;

primary_expression
    : IDENTIFIER
    | INTLIT
    | FLOATLIT
    | STRINGLIT
    | TRUE
    | FALSE
    | PARENL expression PARENR
    ;

argument_list
    : argument_list COMMA expression
    | expression
    ;

%%

/*
 * C Code for Error Handling
 */
int yyerror(const char *s) {
    fprintf(stderr, "Bison Parse Error: %s\n", s);
    return 0;
}