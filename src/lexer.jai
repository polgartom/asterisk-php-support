tokenize :: (using l: *Lexer, filename: string) -> bool {
    s := read_entire_file(filename);
    s = replace_crlf_to_lf(s, autofree=true);
    
    index := find_index_from_left(s, "<?php");
    if index == -1 {
        log_error("% - Unable to locate '<?php'\n", filename);
        return false;
    }
    
    advance(*s, index + "<?php".count);
    if !s {
        log_error("% is empty after '<?php'\n", filename);
        return false;
    }
    
    array_reserve(*tokens, 2048);

    buf = s;
    buf_end = s.data + s.count;    
    
    at = buf.data;
    
    min_tok_kind, max_tok_kind := enum_range(Token_Kind);

    while at < buf_end {
        c := at.*;
/*
        if c == #char "\n" {
            t := array_add(*tokens);
            t.pos   = cast(s32)(at - buf.data);
            t.kind  = .LF;
            t.text  = string.{1, at};
            at += 1;
        }
*/
        if is_whitespace(c) {
            at += 1;
            continue;
        }
    
        if is_identifier_start_char(c) {
            t := tokenize_identifier(l);
        } else if is_digit(c) {
            tokenize_digit(l);
        } else {
            left := at;

            kind := tokenize_multitoken(l);
            if kind != .INVALID {
                if kind == .LINE_COMMENT {
                    at += 2; // jump from '//'
                    while at < buf_end {
                        defer at += 1;
                        if at.* == #char "\n" break;
                    }
                }
            
                t := array_add(*tokens);
                t.pos   = cast(s32)(at - buf.data);
                t.kind  = kind;
                t.text  = string.{at-left, left};
            } else {
                t := array_add(*tokens);
                t.pos   = cast(s32)(at - buf.data);
                t.kind  = cast(Token_Kind, c);
                t.text  = string.{1, at};
                
                at += 1;
            }
            
        }
    }
    
    t := array_add(*tokens);
    t.pos  = cast(s32)(buf_end - buf.data);
    t.kind = .EOF;

    return true;
}

tokenize_digit :: (using l: *Lexer) {
    left := at;
    
    is_float := false;
    is_hex := false;

    if at.* == #char "0" {
        if !(at+1 < buf_end) return;
        next_char := (at+1).*;

        if next_char == #char "x" || next_char == #char "X" {
            // Hexadecimal format
            
            assert(false, "TODO\n");
        } else if next_char == #char "b" {
            // Binary format

            assert(false, "TODO\n");
        }
    }

    at += 1;
    while at < buf_end {
        c := at.*;
        
        if !is_digit(c) {
            if c == #char "." {
                if is_float break;
                
                is_float = true;
                at += 1;
            }
            
            break;
        }
        
        at += 1;
    }

    t := array_add(*l.tokens);
    t.pos   = 1;
    t.text = string.{at-left, left};
    t.kind = .NUMERIC_LITERAL;
}

tokenize_identifier :: (using l: *Lexer) -> Token {
    left := at;
    
    at += 1;
    while at < buf_end {
        c := at.*;
        
        if !is_alnum(c) {
            break;
        }
        
        at += 1;
    }

    t := array_add(*tokens);
    t.pos  = cast(s32)(left - buf.data);
    t.text = string.{at-left, left};
    t.kind = .IDENTIFIER;

    if match_with_any_keyword(t.text) {
        t.kind = .KEYWORD;
    }

    return t;
}

tokenize_multitoken :: (using l: *Lexer) -> Token_Kind {
    c := at.*;
    
    if c == #char "=" {
        if eat_if_match_with("===") {
            return .EQUAL_STRICT;
        }
        if eat_if_match_with("==") {
            return .EQUAL;
        }
        if eat_if_match_with("=>") {
            return .LAMBDA_ARROW;
        }
        
        return .INVALID;
    }
    
    if c == #char "!" {
        if eat_if_match_with("!=") {
            return .NOT_EQUAL_STRICT;
        }
        if eat_if_match_with("!=") {
            return .NOT_EQUAL;
        }
        
        return .INVALID;
    }
    
    if eat_if_match_with("::") {
        return .DOUBLE_COLON;
    }
    
    if eat_if_match_with("->") {
        return .DEREFERENCE;
    }
    
    if eat_if_match_with("||") {
        return .OR;
    }
    if eat_if_match_with(">=") {
        return .GTE;
    }
    if eat_if_match_with("<=") {
        return .LTE;
    }

    if eat_if_match_with("//") {
        return .LINE_COMMENT;
    }

    if eat_if_match_with("+=") {
        return .ADD_ASSIGN;
    }
    if eat_if_match_with("-=") {
        return .SUB_ASSIGN;
    }
    if eat_if_match_with("*=") {
        return .MUL_ASSIGN;
    }
    if eat_if_match_with("++") {
        return .INCREMENT;
    }
    if eat_if_match_with("/=") {
        return .DIV_ASSIGN;
    }
    if eat_if_match_with("%=") {
        return .REMAINER_ASSIGN;
    }
    
    if eat_if_match_with("/*") {
        return .BLOCK_COMMENT_START;
    }
    if eat_if_match_with("/*") {
        return .BLOCK_COMMENT_END;
    }

    return .INVALID;
}

#scope_file

peak :: () -> u8 #expand {
    if !(`at+1 < `buf_end) return 0;
    
    return (`at+1).*;
}

eat_next_if_match :: (c: u8) -> (did_eat: bool) #expand {
    if !(`at+1 < `buf_end) return false;
    if `at[1] != c          return false;
    
    `at += 1;
    return true;
}

eat_if_match_with :: (with: string) -> (did_eat: bool) #expand {
    if !(`at+with.count-1 < `buf_end) return false;
    
    s := string.{with.count, `at};
    if s != with return false;
    
    `at += with.count;
    return true;
}

#scope_export

Lexer :: struct {    
    buf: string;
    buf_end: *u8;
    
    tokens: [..] Token;
    
    at: *u8;
}

Token :: struct {
    text: string; // rename to "name"
    kind: Token_Kind;

    // @Todo: Character position
    pos: s32; // byte
    line: s32;
}

Token_Kind :: enum u8 {    
    // Control tokens
    EOF :: 0;
    LF :: 10;
    CR :: 13;
    
    // Single character
    DOLLAR        :: #char "$";
    LEFT_PAREN    :: #char "(";
    RIGHT_PAREN   :: #char ")";
    LEFT_BRACKET  :: #char "[";
    RIGHT_BRACKET :: #char "]";
    LEFT_BRACE    :: #char "{";
    RIGHT_BRACE   :: #char "}";
    COMMA         :: #char ",";
    SEMICOLON     :: #char ";";
    DOT           :: #char ".";
    PLUS          :: #char "+";
    MINUS         :: #char "-";
    SLASH         :: #char "/";
    BACKSLASH     :: #char "\\";
    START         :: #char "*";
    SINGLE_QUOTE  :: #char "'";
    QUOTE         :: #char "\"";
    COLON         :: #char ":";
    GREATER_THAN  :: #char ">";
    LESS_THAN     :: #char ">";
    ASSIGN        :: #char "=";
    QUESTION_MARK :: #char "?";
    HASH_SIGN     :: #char "#";
    AMPERSAND     :: #char "&";
    EXCLAMATION_MARK :: #char "!";
    AT_SIGN       :: #char "@";
    PIPE          :: #char "|";
    UNDERSCORE    :: #char "_";
    PERCENT_SIGN  :: #char "%";
    
    // Multi character
    EQUAL            :: 128; // ==
    EQUAL_STRICT; // ===
    NOT_EQUAL;        // !=
    NOT_EQUAL_STRICT; // !==
    OR;  // ||
    GTE; // >=
    LTE; // <=
    BITWISE_SHIFT_LEFT;  // <<
    BITWISE_SHIFT_RIGHT; // >>
    BITWISE_NOT; // ~
    BITWISE_XOR; // ^
    
    DOUBLE_COLON;  // ::
    DEREFERENCE;   // ->
    LAMBDA_ARROW;  // =>  
    
    INCREMENT; // ++
    ADD_ASSIGN; // +=
    SUB_ASSIGN; // -=
    MUL_ASSIGN; // *=
    DIV_ASSIGN; // /=
    REMAINER_ASSIGN; // %=
    
    // we just ingore the # something comments, gosh...
    LINE_COMMENT;  // //
    BLOCK_COMMENT_START; // /*
    BLOCK_COMMENT_END;   // */

    INVALID;
    UNKNOWN;
    IDENTIFIER;
    KEYWORD;
    NUMERIC_LITERAL;
    STRING_LITERAL;
}

match_with_any_keyword :: inline (token_str: string) -> bool {
    _, found := table_find(*KEYWORDS_TABLE, token_str);
    return found;
}

KEYWORDS_TABLE :: #run -> Table(string, bool) {
    table: Table(string, bool);
    size := (KEYWORDS.count);
    init(*table, KEYWORDS.count);

    for keyword: KEYWORDS {
        table_set(*table, keyword, true);
    }

    return table;
}

KEYWORDS :: string.[
    // "true", "false", "null", "string", 
    // "bool", "boolean", "double", "array", "float", "int", "integer"

    "if", "else", "elseif", "return", "for", "while", "do", "fn", "class", "enum", "const", "define", "yield", "static",
    "continue", "break", "default", "as", "assert", "use", "namespace", "extends", "trait", "interface", "foreach",
    "public", "private", "protected", "defer", "cast", "case", "void", "never", "switch", "match", "die", "exit", "global",
    "function", "new", "try", "catch", "throw"
];
