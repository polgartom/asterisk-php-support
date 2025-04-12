parse :: (parser: *Parser) {
    t := get();
    if t.kind == .EOF {
        log_error("[%] Nothing to do!\n", #procedure_name());
        return;
    }

    while true {
        t = get();
        if t.kind == .EOF break;

        if t.kind == {
            case .KEYWORD;
                if t.text == "namespace" {
                    parse_namespace(parser);

                } else if t.text == "class" {
                    parse_class(parser);

                } else if t.text == "use" {
                    parse_use(parser);

                }
            case .LINE_COMMENT;
                parse_line_comment(parser);
            case .BLOCK_COMMENT_START;
                parse_block_comment(parser);
        }

        next();
    }

    print("\n");
    for parser.definitions {
        ll("[%] %\n", it, it.*);
    }
    print("\n");
}

parse_class :: (parser: *Parser) {
    t := get();
    assert(t.kind == .KEYWORD && t.text == "class");
    
    t = next();
    if t.kind != .IDENTIFIER {
        log_error("Unexpected class name '%', expecting an identifier.", t.text);
        parser.failed = true;
        return;
    }

    class := New(Class_Defn);
    class._type = .CLASS;
    class.scope = parser.scope;
    class.name = copy_string(t.text);
    class.resolved = true;

    t = next();
    
    if t.text == "extends" {
        t = next();
        if t.kind != .IDENTIFIER {
            log_error("Unexpected parent class name '%', expecting an identifier after 'extends' keyword.", t.text);
            parser.failed = true;
            return;
        }
        
        // @Todo: Try to find the definition
        class.extends = New(Class_Defn);
        class.extends._type = .CLASS;
        class.extends.name = copy_string(t.text);
        class.extends.resolved = false;
        
        t = next();
    } 
    
    if t.text == "implements" {
        t = next();
        
        expecting_identifier := true;
        
        while true {
            if expecting_identifier {
                if t.kind != .IDENTIFIER {
                    log_error("Unexpected interface name '%', expecting an identifier after 'implements' keyword.\n", t.text);
                    parser.failed = true;
                    return;
                }
                
                // @Todo: Try to find the definition
                defn := New(Defn);
                defn._type = .INTERFACE;
                defn.name = copy_string(t.text);
                defn.resolved = false;
                
                array_add(*class.implements, defn);
                
                t = next();
                
            } else {
                if t.kind == .COMMA {
                    expecting_identifier = true;
                    t = next();
                } else if t.kind == .LEFT_BRACE {
                    break;
                } else {
                    log_error("Expected comma ',' or left brace '{' after interface '%' identifier.\n", t.text);
                    parser.failed = true;
                    return;
                }
            }
        }
        
    }

    if t.kind != .LEFT_BRACE {
        log_error("Unexpected '{' token after '%' class declaration, expected '{'.\n", t.text);
        parser.failed = true;
        return;
    }

    array_add(*parser.definitions, class);
    parser.scope = class;

    // @Todo: Parse class definition
}

parse_namespaced_name :: (parser: *Parser) -> bool, string {
    t := get();

    starts_with_backslash := false;
    backslash             := false;

    if t.kind == .BACKSLASH {
        starts_with_backslash = true;
        backslash             = true;
    }

    c0 := t.text.data;
    c1 := t.text.data;

    while true {
        t = get();
        if t.kind == .EOF {
            parser.failed = true;
            return false, "";
        }

        c1 = t.text.data;

        if t.kind == .BACKSLASH {
            if backslash {
                log_error("Unexpected namespaced name '%', expecting an identifier\n", range_to_string(c0, c1));
                parser.failed = true;
                return false, "";
            }

            backslash = true;

        } else if t.kind == .IDENTIFIER {
            backslash = false;

        } else if t.kind == .SEMICOLON {
            if backslash {
                log_error("Unexpected namespaced name '%', expecting an identifier\n", range_to_string(c0, c1));
                return false, "";
            }

            break;
        } else {
            log_error("Unexpected token '%', expected tokens %\n", t.text, Token_Kind.[.IDENTIFIER, .BACKSLASH, .SEMICOLON]);
            return false, "";
        }

        next();
    }

    s := range_to_string(c0, c1);
    return true, s;
} 

// https://www.php.net/manual/en/language.namespaces.rules.php
parse_namespace :: (parser: *Parser) {
    t := get();
    assert(t.kind == .KEYWORD && t.text == "namespace");
    next();
    
    valid, name := parse_namespaced_name(parser);
    if !valid {
        parser.failed = true;
        return;
    }

    defn := New(Defn);
    defn.name = copy_string(name);
    defn._type = .NAMESPACE;
    defn.resolved = true;
    // defn.scope = parser.scope;
        
    array_add(*parser.definitions, defn);
    parser.scope = defn;
}

parse_use :: (parser: *Parser) {
    t := get();
    assert(t.kind == .KEYWORD && t.text == "use");
    next();

    scope := parser.scope;
    is_trait_load := scope && scope._type == .CLASS;
    
    if is_trait_load {
        
    } else {
         valid, name := parse_namespaced_name(parser); // FQN name? full qualified name
         if !valid {
             return;
         }
         
         //ll("%\n", name);
         
         if !scope || scope._type == .NAMESPACE {
             defn := New(Defn);
             defn._type = .UNKNOWN;
             defn.name = copy_string(name);
             defn.scope = null;
             defn.resolved = false;
             
             array_add(*parser.definitions, defn);
             
         } else {
             log_error("Invalid import %\n", name); // @Todo: Better message
             parser.failed = true;
             return;
         }
    }

    // defn := parser.definitions[parser.definitions.count-1];
    // if !(defn._type == .USE && (!defn.scope || defn.scope._type != .NAMESPACE)) {
    //     // log_error();
    //     return;
    // }
 
    // t = next();
}

parse_block_comment :: (parser: *Parser) {
    t := get();
    assert(t.kind == .BLOCK_COMMENT_START);

    while t.kind != .EOF {
        t = next();
        if t.kind == .BLOCK_COMMENT_END {
            next();
            return;
        }
    }

}

parse_line_comment :: (parser: *Parser) {
    t := get();
    assert(t.kind == .LINE_COMMENT);

    while t.kind != .EOF {
        t = next();
        if t.kind == .LF {
            next();
            return;
        }
    }

}

init_parser :: (parser: *Parser) {
    // defn := New(Defn);
    // defn._type = .NAMESPACE;

    // parser.scope = defn;
    // array_add(*parser.definitions, defn);
}

Class_Defn :: struct {
    #as using base: Defn;
    base._type = .CLASS;

    extends: *Class_Defn;
    implements: [..] *Defn; // @Todo: Trait_Defn
    methods: [..] Function_Defn;
}

Class_Parameter_Defn :: struct {
    #as using base: Defn;
    base._type = .CLASS_PARAMETER;
}

Function_Defn :: struct {
    #as using base: Defn;
    base._type = .FUNCTION;

    return_type: Defn;
    parameters: [..] Function_Parameter_Defn;
}

Function_Parameter_Defn :: struct {
    #as using base: Defn;

    base._type = .FUNCTION_PARAMETER;
}

Defn :: struct {
    name: string;
    _type: Defn_Type;
    
    // scope := Scope.File;
    scope: *Defn;

    line: s32;
    pos:  s32;
    
    resolved: bool;
}

Defn_Type :: enum u8 {
    UNKNOWN :: 0;
    
    NAMESPACE;
    USE;
    CLASS;
    CLASS_PARAMETER;
    INTERFACE;
    TRAIT;
    FUNCTION;
    FUNCTION_PARAMETER;
}

Parser :: struct {
    lexer: *Lexer;
    tokens: [] Token;

    failed := false;

    scope: *Defn;
    definitions: [..] *Defn;
    
    // imports [..] *Defn;
}

#scope_file

next :: () -> Token #expand {
   if `parser.tokens[0].kind == .EOF return `parser.tokens[0];
   
   `parser.tokens.data += 1;
   `parser.tokens.count -= 1;
   
   return `parser.tokens[0];
}

get :: () -> Token #expand {
   return `parser.tokens[0]; 
}

is_eof :: () -> bool #expand {
    return get().kind == .EOF;
}

eat_if_match_with :: (with: string) -> (did_eat: bool) #expand {
    if is_eof() return false;
    
    if get().text == with {
        next();
        return true;
    }
    
    return false;
}