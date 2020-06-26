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

static char identifier_str[255]; // Filled in if tok_identifier
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

int main() {
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
