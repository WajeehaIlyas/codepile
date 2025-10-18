// c_parser/ast_types.h
#ifndef AST_TYPES_H
#define AST_TYPES_H

typedef struct {
    char *str_val;
    long long int_val;
    double float_val;
    void *node_ptr;
} Node;

#endif // AST_TYPES_H