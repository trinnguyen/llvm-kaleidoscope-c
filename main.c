#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include "khash.h"

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
    static int  last_char = ' ';

    // Skip any whitespace.
    while (isspace(last_char))
        last_char = getchar();

    // identifiers and specific keywords: identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(last_char)) {
        // reset
        size_t len = strlen(identifier_str);
        for (size_t i = 0; i < len; ++i) {
            identifier_str[i] = 0;
        }

        // index
        int  idx = 0;
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
        int  idx = 0;
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
    int  this_char = last_char;
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
    int args_len;
} call_expr_ast;

typedef struct {
    char name[256];
    char args[256][64];
    int args_len;
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

typedef enum {
    top_node_type_func = -1,
    top_node_type_expr = -2,
    top_node_type_extern = -3,
} top_node_type;

typedef struct {
    top_node_type type;
    union {
        prototype_ast *extern_proto;
        function_ast *function;
        function_ast *expr;
    };
} top_node;

typedef struct {
    top_node nodes[64];
    int  nodes_len;
} root_ast;

static root_ast gl_root_ast = {};

static int  cur_tok;
static int  get_next_token() {
    cur_tok = gettok();
    return cur_tok;
}

static void log_info(const char *str) {
    fprintf(stdout, "info: %s\n", str);
}

static void log_error(const char *str) {
    fprintf(stderr, "log error: %s\n", str);
}

// numberexpr ::= number
static expr_ast *parse_number_expr() {
    expr_ast *res = malloc(sizeof(expr_ast));
    res->type = expr_ast_type_num;
    res->number_val = num_val;
    get_next_token();
    return res;
}

static struct expr_ast *parse_expression();

static expr_ast *parse_identifier_func_call() {
    expr_ast *var = malloc(sizeof(expr_ast));
    var->type = expr_ast_type_var;
    strcpy(var->variable.name, identifier_str);

    if (get_next_token() != '(') {
        return var;
    }

    // check if function call
    expr_ast *func_call = malloc(sizeof(expr_ast));
    func_call->type = expr_ast_type_function_call;
    strcpy(func_call->call_expr.callee, identifier_str);
    get_next_token(); // consume (
    func_call->call_expr.args_len = 0;
    if (cur_tok != ')') {
        while (1) {
            expr_ast *expr = parse_expression();
            if (expr == NULL) {
                return NULL;
            }

            // append
            func_call->call_expr.args[func_call->call_expr.args_len++] = expr;

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
            fprintf(stderr, "unknown token when expecting an expression: %d\t", cur_tok);
            return NULL;
    }
}

static int  get_tok_precedence() {
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

static expr_ast *parse_binop_rhs(int  expr_prec, expr_ast* lhs) {
    // If this is a binop, find its precedence.
    while (1) {
        int  tok_prec = get_tok_precedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (tok_prec < expr_prec)
            return lhs;

        // Okay, we know this is a binop.
        int  binop = cur_tok;
        get_next_token(); // eat binop

        // Parse the primary expression after the binary operator.
        expr_ast *rhs = parse_primary();
        if (rhs == NULL) {
            return NULL;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int  next_prec = get_tok_precedence();
        if (tok_prec < next_prec) {
            rhs = parse_binop_rhs(tok_prec + 1, rhs);
            if (rhs == NULL)
                return NULL;
        }

        // Merge LHS/RHS.
        expr_ast *res = malloc(sizeof(expr_ast));
        res->type = expr_ast_type_binary;
        res->binary_expr = (binary_expr_ast){
                .lhs = lhs,
                .op = (char)binop,
                .rhs = rhs
        };
        return res;
    }
}

static int  is_supported_binop() {
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
    prototype_ast *res = malloc(sizeof(prototype_ast));
    strcpy(res->name, identifier_str);

    // args
    if (get_next_token() != '(') {
        log_error("Expected ( in prototype");
        return NULL;
    }

    // Read the list of argument names.
    while (get_next_token() == tok_identifier) {
        strcpy(res->args[res->args_len++], identifier_str);
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

    function_ast *res = malloc(sizeof(function_ast));
    res->proto = proto;
    res->body = expr;
    return res;
}

static prototype_ast *parse_extern() {
    // consume extern
    get_next_token();

    // parse
    return parse_prototype();
}

// toplevelexpr ::= expression
// defining anonymous nullary (zero argument) function for them
static function_ast *parse_top_level_expr() {
    expr_ast *expr = parse_expression();
    if (expr == NULL)
        return NULL;

    prototype_ast *proto = malloc(sizeof(prototype_ast));
    strcpy(proto->name, "__anon_expr");

    function_ast *func = malloc(sizeof(function_ast));
    func->proto = proto;
    func->body = expr;
    return func;
}

static void handle_definition() {
    function_ast *ast = parse_definition();
    if (ast != NULL) {
        gl_root_ast.nodes[gl_root_ast.nodes_len ++ ] = (top_node){
                .type = top_node_type_func,
                .function = ast
        };
        fprintf(stdout, "Parsed a function definition: %s.\n", ast->proto->name);
    } else {
        // Skip token for error recovery.
        get_next_token();
    }
}

static void handle_extern() {
    prototype_ast *ast = parse_extern();
    if (ast != NULL) {
        gl_root_ast.nodes[gl_root_ast.nodes_len ++ ] = (top_node){
                .type = top_node_type_extern,
                .extern_proto = ast
        };
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
        gl_root_ast.nodes[gl_root_ast.nodes_len ++ ] = (top_node){
                .type = top_node_type_expr,
                .expr = ast
        };
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
    int  stop = 0;
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

static LLVMContextRef context;

static LLVMModuleRef module;

static LLVMBuilderRef builder;

// hash table for name values
KHASH_MAP_INIT_STR(str, LLVMValueRef);
khash_t(str) *h;

static LLVMValueRef code_gen_expr(expr_ast *expr);

static LLVMValueRef code_gen_proto(prototype_ast *proto) {

    int param_cnt = proto->args_len;

    // set param types
    LLVMTypeRef params[param_cnt];
    for (int i = 0; i < param_cnt; i++) {
        params[i] = LLVMDoubleTypeInContext(context);
    }

    // create function
    LLVMTypeRef func_type = LLVMFunctionType(LLVMDoubleTypeInContext(context), params, param_cnt, 0);
    LLVMValueRef func = LLVMAddFunction(module, proto->name, func_type);
    LLVMSetLinkage(func, LLVMExternalLinkage);

    // set param name
    for (int i = 0; i < param_cnt; i++) {
        LLVMValueRef param = LLVMGetParam(func, i);
        LLVMSetValueName2(param, proto->args[i], strlen(proto->args[i]));
    }

    return func;
}

static LLVMValueRef code_gen_func(function_ast *ast) {

    // First, check for an existing function from a previous 'extern' declaration.
    LLVMValueRef func = LLVMGetNamedFunction(module, ast->proto->name);
    if (func == NULL) {
        func = code_gen_proto(ast->proto);
    }

    if (func == NULL) {
        return NULL;
    }

    // Create a new basic block to start insertion into.
    LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(context, func, "entry");
    LLVMPositionBuilderAtEnd(builder, block);

    // Record the function arguments in the NamedValues map.
    kh_clear_str(h);
    for (int i = 0; i < ast->proto->args_len; ++i) {
        LLVMValueRef param = LLVMGetParam(func, i);

        int ret = 0;
        khint_t k = kh_put_str(h, ast->proto->args[i], &ret);
        kh_value(h, k) = param;
    }

    // set body (return value)
    LLVMValueRef body = code_gen_expr(ast->body);
    if (body == NULL) {
        log_error("Invalid generated body");
        LLVMDeleteFunction(func);
        return NULL;
    }
    LLVMBuildRet(builder, body);

    // verify
    if (LLVMVerifyFunction(func, LLVMPrintMessageAction) == 1) {
        log_error("invalid function");
        LLVMDeleteFunction(func);
        return NULL;
    }

    return func;
}

LLVMValueRef code_gen_expr_const(double val) {
    return LLVMConstReal(LLVMDoubleTypeInContext(context), val);
}

LLVMValueRef code_gen_expr_var(variable_expr_ast var_expr) {
    khint_t k = kh_get_str(h, var_expr.name);
    if (kh_exist(h, k) == 0) {
        log_error("variable is not found in hash table");
        return NULL;
    }

    // get value
    LLVMValueRef var = kh_val(h, k);
    if (var == NULL) {
        log_error("invalid variable data");
        return NULL;
    }

    return var;
}

LLVMValueRef code_gen_expr_bin(binary_expr_ast binary_expr) {
    switch (binary_expr.op) {
        case '+':
            return LLVMBuildFAdd(builder, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "addtmp");
        case '-':
            return LLVMBuildFSub(builder, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "subtmp");
        case '*':
            return LLVMBuildMul(builder, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "multmp");
        case '/':
            return LLVMBuildFDiv(builder, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "divtmp");
        case '<':
            return LLVMBuildFCmp(builder, LLVMRealULT, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "lesstmp");
        case '>':
            return LLVMBuildFCmp(builder, LLVMRealUGT, code_gen_expr(binary_expr.lhs), code_gen_expr(binary_expr.rhs), "greatertmp");
        default:
            log_error("Op is not supported");
            return NULL;
    }
}

LLVMValueRef code_gen_expr_func_call(call_expr_ast call_expr) {
    LLVMValueRef func = LLVMGetNamedFunction(module, call_expr.callee);
    if (func == NULL) {
        log_error("Unknown function referenced");
        return NULL;
    }

    // check parameter size
    unsigned expected_size = LLVMCountParams(func);
    if (expected_size != call_expr.args_len) {
        log_error("Mismatch params count");
    }

    // type checking is ignore (single double in the language)

    // set args
    LLVMValueRef args[64];
    for (int i = 0; i < call_expr.args_len; i++) {
        args[i] = code_gen_expr(call_expr.args[i]);
    }

    LLVMTypeRef ret_type = LLVMGetCalledFunctionType(func);
    return LLVMBuildCall2(builder, ret_type, func, args, call_expr.args_len, "calltmp");
}

static LLVMValueRef code_gen_expr(expr_ast *expr) {
    switch (expr->type) {
        case expr_ast_type_num:
            return code_gen_expr_const(expr->number_val);
        case expr_ast_type_var:
            return code_gen_expr_var(expr->variable);
        case expr_ast_type_binary:
            return code_gen_expr_bin(expr->binary_expr);
        case expr_ast_type_function_call:
            return code_gen_expr_func_call(expr->call_expr);
    }
}

static void code_gen() {
    // init
    context = LLVMContextCreate();
    module = LLVMModuleCreateWithNameInContext("default", context);
    builder = LLVMCreateBuilderInContext(context);

    // hash table
    h = kh_init_str();

    // start
    for (int i = 0; i < gl_root_ast.nodes_len; i++) {
        top_node node = gl_root_ast.nodes[i];
        switch (node.type) {
            case top_node_type_func:
                code_gen_func(node.function);
                break;
            case top_node_type_expr:
                code_gen_func(node.expr);
                break;
            case top_node_type_extern:
                code_gen_proto(node.extern_proto);
                break;
        }
    }
}

static void print_ast() {
    fprintf(stdout, "AST: %d top-level items\n", gl_root_ast.nodes_len);

    for (int  i = 0; i < gl_root_ast.nodes_len; i++) {
        top_node node = gl_root_ast.nodes[i];
        switch (node.type) {
            case top_node_type_func:
                fprintf(stdout, "- function: %s, size: %d\n", node.function->proto->name, node.function->proto->args_len);
                break;
            case top_node_type_expr:
                fprintf(stdout, "- expr\n");
                break;
            case top_node_type_extern:
                fprintf(stdout, "- extern: %s, size: %d\n", node.extern_proto->name, node.extern_proto->args_len);
                break;
        }
    }
}

/*
 * entry point
 * def test1() 3 def test2() 4 test1() extern fib()
 */
int main() {

//    run_lexer();
//    return 0;

    // parse the ast
    log_info("### start parsing");
    parse();
    log_info("finished parsing");

    print_ast();


    // code gen
    log_info("### start code gen\n");
    code_gen();

    // print
    printf("%s", LLVMPrintModuleToString(module));

    return 0;
}
