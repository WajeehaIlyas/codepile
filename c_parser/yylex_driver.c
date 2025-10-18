// c_parser/yylex_driver.c 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast_types.h" 
#include "parser.tab.h" 

extern YYSTYPE yylval;       
extern int yyerror(const char *s);

extern int rust_yylex(YYSTYPE *yylval_ptr);

int yylex(void) {
    int token_id = rust_yylex(&yylval); 
    if (token_id == 0) {
        return 0; 
    }

    return token_id;
}
