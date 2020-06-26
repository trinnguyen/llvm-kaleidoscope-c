#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
typedef enum {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
} Token;

static char identifier_str[256]; // Filled in if tok_identifier
static double num_val;             // Filled in if tok_number

/**
 * get token
 * @return token
 */
static Token gettok() {
    static int last_char = ' ';

    // Skip any whitespace.
    while (isspace(last_char))
        last_char = getchar();

    // identifiers and specific keywords: identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(last_char)) {
        // reset
        memset(identifier_str, 0, sizeof(identifier_str));
        identifier_str[0] = 0;

        // index
        int idx = 0;
        identifier_str[idx++] = (char)last_char;

        while (isalnum((last_char = getchar())))
            identifier_str[idx++] = (char)last_char;

        if (strcmp("def", identifier_str) == 0)
            return tok_def;

        if (strcmp("extern", identifier_str) == 0)
            return tok_extern;

        return tok_identifier;
    }

    // Number: [0-9.]+
    if (isdigit(last_char) || last_char == '.') {
        char num_str[64];
        int idx = 0;
        do {
            num_str[idx++] = (char)last_char;
            last_char = getchar();
        } while (isdigit(last_char) || last_char == '.');

        num_val = strtod(num_str, 0);
        return tok_number;
    }

    // Comment until end of line.
    if (last_char == '#') {
        do
            last_char = getchar();
        while (last_char != EOF && last_char != '\n' && last_char != '\r');

        if (last_char != EOF)
            return gettok();
    }

    // Check for end of file.  Don't eat the EOF.
    if (last_char == EOF)
        return tok_eof;

    // Otherwise, just return the character as its ascii value.
    int this_char = last_char;
    last_char = getchar();
    return this_char;
}

/*
 * Parser
 */

typedef struct expr_ast expr_ast;

typedef struct {
    char name[256];
} variable_expr_ast;

typedef struct {
    char op;
    expr_ast *lhs;
    expr_ast *rhs;
} binary_expr_ast;

typedef struct {
    char callee[256];
    expr_ast *args[64];
} call_expr_ast;

typedef struct {
    char name[256];
    char args[256][64];

} prototype_ast;

typedef struct {
    prototype_ast *proto;
    expr_ast *body;
} function_ast;

typedef enum {
    expr_ast_type_num = -1,
    expr_ast_type_var = -2,
    expr_ast_type_binary = -3,
    expr_ast_type_function_call = -4,
} expr_ast_type;

struct expr_ast{
    expr_ast_type type;
    union {
        double number_val;
        variable_expr_ast variable;
        binary_expr_ast binary_expr;
        call_expr_ast call_expr;
    };
};

typedef struct {
    prototype_ast *extern_protos[64];
    function_ast *functions[64];
    function_ast *top_exprs[64];
} root_ast;

static root_ast gl_root_ast = {};
static int ast_function_idx = 0;
static int ast_extern_idx = 0;
static int ast_expr_idx = 0;

static int cur_tok;
static int get_next_token() {
    cur_tok = gettok();
    fprintf(stdout, "\tnext token %d\n", cur_tok);
    return cur_tok;
}

static void log_error(const char *str) {
    fprintf(stderr, "log error: %s\n", str);
}

// numberexpr ::= number
static expr_ast *parse_number_expr() {
    expr_ast *res = &(expr_ast){
            .type = expr_ast_type_num,
            .number_val = num_val
    };
    get_next_token();
    return res;
}

static struct expr_ast *parse_expression();

static expr_ast *parse_identifier_func_call() {
    expr_ast *var = &(expr_ast){.type = expr_ast_type_var};
    strcpy(var->variable.name, identifier_str);

    if (get_next_token() != '(') {
        return var;
    }

    // check if function call
    expr_ast *func_call = &(expr_ast){.type = expr_ast_type_function_call};
    strcpy(func_call->call_expr.callee, identifier_str);
    get_next_token(); // consume (
    int idx = 0;
    if (cur_tok != ')') {
        while (1) {
            expr_ast *expr = parse_expression();
            if (expr == NULL) {
                return NULL;
            }

            // append
            func_call->call_expr.args[idx++] = expr;

            // finish
            if (cur_tok == ')')
                break;

            // comma
            if (cur_tok != ',') {
                log_error("Expected , or ) after argument");
                return NULL;
            }

            // move
            get_next_token();
        }
    }

    // Eat the ')'.
    get_next_token();
    return func_call;
}

static expr_ast *parse_parent_expr() {
    get_next_token(); // eat (.
    expr_ast *expr = parse_expression();
    if (expr == NULL)
        return NULL;

    if (cur_tok != ')') {
        log_error("Expected ) for parent expr");
        return NULL;
    }

    get_next_token(); //eat )

    return expr;
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static expr_ast *parse_primary() {
    switch (cur_tok) {
        case tok_number:
            return parse_number_expr();
        case tok_identifier:
            return parse_identifier_func_call();
        case '(':
            return parse_parent_expr();
        default:
            log_error("unknown token when expecting an expression");
            return NULL;
    }
}

static int get_tok_precedence() {
    if (!isascii(cur_tok))
        return -1;

    switch (cur_tok) {
        case '>':
        case '<':
            return 10;
        case '+':
        case '-':
            return 20;
        case '*':
        case '/':
            return 40;
        default:
            return -1;
    }
}

static expr_ast *parse_binop_rhs(int expr_prec, expr_ast* lhs) {
    // If this is a binop, find its precedence.
    while (1) {
        int tok_prec = get_tok_precedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (tok_prec < expr_prec)
            return lhs;

        // Okay, we know this is a binop.
        int binop = cur_tok;
        get_next_token(); // eat binop

        // Parse the primary expression after the binary operator.
        expr_ast *rhs = parse_primary();
        if (rhs == NULL) {
            return NULL;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int next_prec = get_tok_precedence();
        if (tok_prec < next_prec) {
            rhs = parse_binop_rhs(tok_prec + 1, rhs);
            if (rhs == NULL)
                return NULL;
        }

        // Merge LHS/RHS.
        return &(expr_ast){
            .type = expr_ast_type_binary,
            .binary_expr = (binary_expr_ast){
                .lhs = lhs,
                .op = (char)binop,
                .rhs = rhs
            }
        };
    }
}

static int is_supported_binop() {
    return cur_tok == '+' || cur_tok == '-' || cur_tok == '*' || cur_tok == '/' || cur_tok == '>' || cur_tok == '<';
}

/// expression
///   ::= primary binoprhs
///
static expr_ast *parse_expression() {
    expr_ast *lhs = parse_primary();
    if (lhs == NULL) {
        return NULL;
    }

    if (is_supported_binop() == 1) {
        return parse_binop_rhs(0, lhs);
    }

    return lhs;
}

static prototype_ast *parse_prototype() {
    if (cur_tok != tok_identifier) {
        log_error("Expected ID for prototype");
        return NULL;
    }

    // name
    prototype_ast *res = &(prototype_ast){};
    strcpy(res->name, identifier_str);

    // args
    if (get_next_token() != '(') {
        log_error("Expected ( in prototype");
        return NULL;
    }

    // Read the list of argument names.
    int idx = 0;
    while (get_next_token() == tok_identifier) {
        strcpy(res->args[idx++], identifier_str);
    }

    if (cur_tok != ')') {
        log_error("Expected ) in prototype");
        return NULL;
    }

    // consume )
    get_next_token();

    return res;
}

static function_ast *parse_definition() {
    get_next_token();
    prototype_ast *proto = parse_prototype();
    if (proto == NULL)
        return NULL;

    expr_ast *expr = parse_expression();
    if (expr == NULL)
        return NULL;

    return &((function_ast){.proto = proto, .body = expr});
}

static prototype_ast *parse_extern() {
    // consume extern
    get_next_token();

    // parse
    return parse_prototype();
}

// toplevelexpr ::= expression
// defining anonymous nullary (zero argument) functions for them
static function_ast *parse_top_level_expr() {
    expr_ast *expr = parse_expression();
    if (expr == NULL)
        return NULL;

    prototype_ast *proto = &(prototype_ast){.name = ""};
    function_ast *func = &(function_ast){.proto = proto, .body = expr};
    return func;
}

static void handle_definition() {
    function_ast *ast = parse_definition();
    if (ast != NULL) {
        gl_root_ast.functions[ast_function_idx++] = ast;
        fprintf(stdout, "Parsed a function definition.\n");
    } else {
        // Skip token for error recovery.
        get_next_token();
    }
}

static void handle_extern() {
    prototype_ast *ast = parse_extern();
    if (ast != NULL) {
        gl_root_ast.extern_protos[ast_extern_idx++] = ast;
        fprintf(stdout, "Parsed an extern\n");
    } else {
        // Skip token for error recovery.
        get_next_token();
    }
}

static void handle_top_level_expression() {
    // Evaluate a top-level expression into an anonymous function.
    function_ast *ast = parse_top_level_expr();
    if (ast != NULL) {
        gl_root_ast.top_exprs[ast_expr_idx++] = ast;
        fprintf(stdout, "Parsed a top-level expr\n");
    } else {
        // Skip token for error recovery.
        get_next_token();
    }
}

static void parse() {
    get_next_token();

    while(1) {
        switch (cur_tok) {
            case tok_eof:
                return;
            case ';': // ignore top-level semicolons.
                get_next_token();
                break;
            case tok_def:
                handle_definition();
                break;
            case tok_extern:
                handle_extern();
                break;
            default:
                handle_top_level_expression();
                break;
        }
    }
}

static void run_lexer() {
    int stop = 0;
    while (stop == 0) {
        Token token = gettok();
        switch (token) {
            case tok_def:
                printf("keyword: def\n");
                break;
            case tok_extern:
                printf("keyword: extern\n");
                break;
            case tok_identifier:
                printf("identifier: %s\n", identifier_str);
                break;
            case tok_number:
                printf("number: %f\n", num_val);
                break;
            case tok_eof:
                stop = 1;
                break;
            default:
                break;
        }
    }
}

/*
 * entry point
 */
int main() {
    parse();

    fprintf(stdout, "parse result: %d functions, %d externs, %d top level exprs\n", ast_function_idx, ast_extern_idx, ast_expr_idx);

    for (int i = 0; i < ast_function_idx; ++i) {
        fprintf(stdout, "\tfunction: %s\n", gl_root_ast.functions[i]->proto->name);
    }

    for (int i = 0; i < ast_extern_idx; ++i) {
        fprintf(stdout, "\texterns: %s\n", gl_root_ast.extern_protos[i]->name);
    }
}
