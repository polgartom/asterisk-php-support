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

                    // if t.kind != .RIGHT_BRACE {
                    //     log_error("Expected '}' after class definition, '%' given.\n", t.kind);
                    //     parser.failed = true;
                    //     break;
                    // }

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

    namespace := "";
    if parser.scope then namespace = parser.scope.name;

    fqn := join(namespace, t.text, separator = "\\");
    _, found := table_find(*definitions, fqn);
    if found {
        log_error("Definition of the '%' is already parsed.", fqn);
        free(fqn);
        return;
    }

    class := cast(*Class_Defn)add_definition(fqn, .CLASS, parser.scope);
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

    parser.block_index += 1;
    class.block_index = parser.block_index;
    parser.scope = class;

    // @Todo: Parse class definition

    t = next();
    while t.kind != .EOF {
        if t.kind == .KEYWORD {
            if t.text == {
                case "const"; assert(false); // @Todo: 
                case "public"; #through;
                case "protected"; #through;
                case "private";
                    ok := parse_property(parser);
                    if !ok {
                        report_error(parser, "Invalid property\n");
                        assert(false); // @Temporary
                        return;
                    }
            }
        } else if t.kind == .LEFT_BRACE {
            parser.block_index += 1;
        } else if t.kind == .RIGHT_BRACE {
            parser.block_index -= 1;
            if parser.block_index == class.block_index {
                ll("found end of the '%' class.\n", class.name);
                return;
            }
        }

        t = next();
    }

}

map_property_visibility :: (s: string) -> Property_Visibility {
    if s == {
        case "public";    return .PUBLIC;
        case "protected"; return .PROTECTED;
        case "private";   return .PRIVATE;
    }

    return .NONE;
}

parse_property :: (parser: *Parser) -> bool {
    if !parser.scope || parser.scope._type != .CLASS {
        return false;
    }

    assert(parser.scope._type == .CLASS);

    t := get();

    visibility := map_property_visibility(t.text);
    if visibility == .NONE {
        log_error("Invalid property visibility\n");
        parser.failed = true;
        return false;
    }

    t = next();

    scope := cast(*Class_Defn) parser.scope;
    type_defn: *Defn;

    if t.kind == .IDENTIFIER {
        // Maybe type

        valid, name := parse_namespaced_name(parser);
        if !valid {
            parser.failed = true;
            return false;
        }

        type_defn=, found := table_find(*definitions, name);
        if !found {
            // @Todo: 
            assert(false, "TODO");
        }

        t = get();
    }
    
    if type_defn != null && t.kind != .DOLLAR {
        log_error("Expects a '$' after the type, '%' given.\n", t.text);
        parser.failed = true;
        return false;
    }

    if t.kind == .DOLLAR {
        t = next();
        if t.kind != .IDENTIFIER {
            log_error("Expected an identifier after '$', '%' given.\n", t.text);
            parser.failed = true;
            return false;
        }

        // @Todo: Primitive type inference

        defn := New(Variable_Defn);
        defn.name = copy_string(t.text);
        defn._type = .VARIABLE;
        defn.scope = scope;
        array_add(*defn.types, type_defn);
        defn.visibility = visibility;

        array_add(*scope.properties, defn);

        skip_tokens_until(parser, .SEMICOLON);

    } else if t.text == "function" {
        t = next();
        if t.kind != .IDENTIFIER {
            log_error("Expected an identifier after 'function', '%' given.\n", t.text);
            parser.failed = true;
            return false;
        }


        fn_defn := New(Function_Defn);
        
        fqn := join(parser.scope.name, t.text, separator="\\");
        fn_defn.name = fqn;

        fn_defn._type = .FUNCTION;
        fn_defn.scope = scope;
        fn_defn.visibility = visibility;

        t = next();
        if t.kind != .LEFT_PAREN {
            log_error("Expected a '(' after function name, '%' given.\n", t.text);
            parser.failed = true;
            return false;
        }
        t = next();

        ok, params := parse_comma_separated_function_parameter_list(parser);
        if !ok {
            report_error(parser, "Invalid parameter list\n");
            parser.failed = true;
            return false;
        }
        t = get();
        
        if t.kind != .RIGHT_PAREN {
            log_error("Expected a ')' after function parameters, '%' given.\n", t.text);
            parser.failed = true;
            return false;
        }
        t = next();

        fn_defn.parameters = params;
        array_add(*scope.methods, fn_defn); // At this point, it's kind of valid, so we can add to the method list

        if t.kind == .COLON {
            t = next();
            found, return_types := parse_pipe_separated_type_list(parser);
            if found {
                fn_defn.return_types = return_types;
            }
        }

        t = get();

        if t.kind == .SEMICOLON {

        } else if t.kind == .LEFT_BRACE {
            parser.block_index += 1;
            fn_defn.block_index = parser.block_index;
            
            while t.kind != .EOF {
                if t.kind == .LEFT_BRACE {
                    parser.block_index += 1;
                } else if t.kind == .RIGHT_BRACE {
                    parser.block_index -= 1;
                    if parser.block_index == fn_defn.block_index {
                        // ll("found end of the '%' method.\n", fn_defn.name);
                        return true;
                    }
                }

                t = next();
            }
        }

        return false;

    } else {
        log_error("'$' or 'function' are expected instead of '%'.\n", t.text);
        parser.failed = true;
        return false;
    }

    t = next();

    return true;
}

// @Cleanup: Naming
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

        if t.kind == .BACKSLASH {
            if backslash {
                log_error("Unexpected namespaced name '%', expecting an identifier after backslash.\n", range_to_string(c0, c1));
                parser.failed = true;
                return false, "";
            }

            backslash = true;

        } else if t.kind == .IDENTIFIER {
            backslash = false;

            c1 = t.text.data + t.text.count;

        } else {
            if backslash {
                log_error("Unexpected namespaced name '%', expecting an identifier after backslash.\n", range_to_string(c0, c1));
                parser.failed = true;
                return false, "";
            }

            // ll("> |%|\n", range_to_string(c0, c1));

            break;
        }

        next();
    }

    s := range_to_string(c0, c1);
    return s.count > 0, s;
} 

add_definition :: (fqn: string, $type: Defn_Type, scope: *Defn = null) -> *Defn {
    defn: *Defn;

    #if type == {
        case .UNKNOWN;
            defn = New(Defn);
            defn.resolved = false;
        case .PRIMITIVE;
            defn = New(Defn);
            defn.resolved = true;
            defn.intrinsic = true;
        case .CLASS;
            defn = New(Class_Defn);
        case .VARIABLE;
            defn = New(Variable_Defn);
            defn.resolved = true; // do we want to explicitly specify it. The variable always resolved, except its type
        case .FUNCTION;
            defn = New(Function_Defn);
        case .NAMESPACE;
            defn = New(Defn);
            assert(scope == null);
    }

    assert(defn != null, "% % %\n", fqn, type, scope);

    defn.name   = fqn;
    defn._type  = type;
    table_add(*definitions, defn.name, defn);

    return defn;
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

    t = get();
    if t.kind != .SEMICOLON {
        log_error("Namespace '%' must end with a ';'.\n", name);
        parser.failed = true;
        return;
    }

    assert(parser.scope == null);

    defn := New(Defn);
    defn.name = copy_string(name);
    defn._type = .NAMESPACE;
    defn.resolved = true;

    // array_add(*parser.definitions, defn);
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
            parser.failed = true;
            return;
        }
        
        //ll("%\n", name);
        
        if !scope || scope._type == .NAMESPACE {
            defn := New(Defn);
            defn._type = .UNKNOWN;
            defn.name = copy_string(name);
            defn.scope = null;
            defn.resolved = false;
            
            // array_add(*parser.definitions, defn);
            
        } else {
            log_error("Invalid import %\n", name); // @Todo: Better message
            parser.failed = true;
            return;
        }
    }
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

parse_pipe_separated_type_list :: (parser: *Parser) -> bool, [..] *Defn {
    result: [..] *Defn;

    t := get();
    if !(t.kind == .IDENTIFIER || t.kind == .BACKSLASH) return false, result;

    type_parsed := false;
    while t.kind != .EOF {
        // ll("> %\n", t);

        if t.kind == .IDENTIFIER || t.kind == .BACKSLASH {
            if type_parsed return false, result;
            type_parsed = true;

            ok, name := parse_namespaced_name(parser);
            if !ok return false, result;

            t = get();

            namespaced_globally := false;
            if starts_with(name, "\\") {
                namespaced_globally = true;
                advance(*name, 1);
            }

            type_defn, found := table_find(*definitions, name);
            if !found {
                name = copy_string(name);
                type_defn = add_definition(name, .UNKNOWN, parser.scope);
            }

            array_add(*result, type_defn);

            continue;

        } else if t.kind == .PIPE {
            if !type_parsed return false, result;
            type_parsed = false;
        } else {
            // ll("%\n", get());
            break;
        }

        t = next();
    }

    // ll("last token: %\n", t);

    return type_parsed, result;
}

parse_comma_separated_function_parameter_list :: (parser: *Parser) -> bool, [..] *Variable_Defn {
    result: [..] *Variable_Defn;

    t := get();
    if !(t.kind == .IDENTIFIER || t.kind == .DOLLAR) return true, result;

    // @Todo: The visibility keywords are valid in the __constructor(), so we need to check that at some point.

    param_parsed := false;
    while t.kind != .EOF {
        has_type_declaration, types := parse_pipe_separated_type_list(parser);
        t = get();

        if t.kind == .DOLLAR {
            if param_parsed {
                return false, result;
            }
            param_parsed = true;

            t = next();
            if t.kind != .IDENTIFIER {
                report_error(parser, "Identifier is expected after '$' in the function parameter list, '%' given\n", t.text);
                return false, result;
            }

            name := copy_string(t.text);
            defn := cast(*Variable_Defn) add_definition(name, .VARIABLE, parser.scope);
            defn.types = types;
            array_add(*result, defn);

            t = next();

            if t.kind == .ASSIGN {
                t = next();
                ok, types := parse_pipe_separated_type_list(parser);

                if !ok || types.count > 1 return false, result;
                t = get();
            }

            continue;

        } else if t.kind == .COMMA {
            if !param_parsed {
                return false, result;
            }
            param_parsed = false;

        } else {
            break;
        }

        t = next();
    }

    // ll("last token: %\n", t);

    return true, result;
}

report_error :: (parser: *Parser, fmt: string, args: .. Any) {
    line_str := get_line_as_string(parser.lexer, get().line);
    s := tprint(fmt, ..args);
    print("[error] |%| %:% -> %\n", line_str, get().line, get().pos, s);
} @PrintLike

skip_tokens_until :: (using parser: *Parser, kind: Token_Kind) -> (found: bool) {
    t := get();
    while t.kind == .EOF {
        if t.kind == kind return true;
    }

    return false;
}

init_parser :: (parser: *Parser) {
    add_definition("bool", .PRIMITIVE);
    add_definition("string", .PRIMITIVE);
    add_definition("int", .PRIMITIVE);
    add_definition("float", .PRIMITIVE);
    add_definition("double", .PRIMITIVE);
    add_definition("array", .PRIMITIVE);
    add_definition("void", .PRIMITIVE);
    add_definition("null", .PRIMITIVE);
    add_definition("null", .PRIMITIVE);
    add_definition("object", .PRIMITIVE);
    add_definition("callable", .PRIMITIVE);
    add_definition("resource", .PRIMITIVE);
    add_definition("never", .PRIMITIVE);
    add_definition("true", .PRIMITIVE);
    add_definition("false", .PRIMITIVE);

    // k := copy_string("array");

    // v, f := table_find(*definitions, k);
    // dd(">> % %\n", v.*, f);
}

Class_Defn :: struct {
    #as using base: Defn;
    base._type = .CLASS;

    extends: *Class_Defn;
    implements: [..] *Defn; // @Todo: Trait_Defn
    properties: [..] *Variable_Defn; 
    methods: [..] *Function_Defn;

    is_abstract: bool;
}

Variable_Defn :: struct {
    #as using base: Defn;
    base._type = .VARIABLE;

    types: [..] *Defn;

    visibility: Property_Visibility;
    is_static: bool;
    is_abstract: bool;
}

Function_Defn :: struct {
    #as using base: Defn;
    base._type = .FUNCTION;

    return_types: [..] *Defn;
    parameters: [..] *Variable_Defn;

    visibility: Property_Visibility;
    
    is_static: bool;
    is_abstract: bool;
}

Defn :: struct {
    name: string; // @Cleanup rename it to fqn (Full Qualified Name)
    _type: Defn_Type; // @Cleanup rename it to kind

    scope: *Defn;
    block_index: s32;

    line: s32;
    pos:  s32;
    
    resolved: bool;
    intrinsic: bool;

}

Defn_Type :: enum u8 {
    UNKNOWN :: 0;
    
    PRIMITIVE;

    NAMESPACE;
    USE;
    
    CLASS;
    INTERFACE;
    TRAIT;
    
    PARAMETER;

    VARIABLE;
    FUNCTION;
}

Property_Visibility :: enum u8 {
    NONE :: 0;
    PUBLIC;
    PROTECTED;
    PRIVATE;
}

Parser :: struct {
    lexer: *Lexer;
    tokens: [] Token;

    failed := false;

    scope: *Defn;
    block_index: s32;
    // definitions: [..] *Defn;
    
    // imports [..] *Defn;
}

definitions: Table(string, *Defn);

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