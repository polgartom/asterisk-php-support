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

    // array_add(*parser.definitions, class);
    parser.scope = class;

    // @Todo: Parse class definition

    t = next();
    while t.kind != .EOF {
        // ll("> %\n", t);

        if t.kind == .KEYWORD {
            if t.text == {
                case "const"; assert(false); // @Todo: 
                case "public";
                    parse_property(parser);
                case "protected";
                    parse_property(parser);
                case "private";
                    parse_property(parser);
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
    
    if t.kind == .DOLLAR {
        t = next();
        if t.kind != .IDENTIFIER {
            log_error("Expected an identifier after '$', '%' given.\n", t.text);
            parser.failed = true;
            return false;
        }

        defn := New(Variable_Defn);
        defn.name = copy_string(t.text);
        defn._type = .VARIABLE;
        defn.scope = scope;
        defn.type = type_defn;
        defn.visibility = visibility;

        array_add(*scope.properties, defn);

        // namespace := parser.scope.name;
        // fqn := join(namespace, var_name, separator="\\");
        
        // parser.scope.properties

        // defn := add_definition(fqn, .VARIABLE, parser.scope);

    }

    t = next();
}

parse_function :: (parser: *Parser) -> bool, string {

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
    return true, s;
} 

add_definition :: (fqn: string, $type: Defn_Type, scope: *Defn = null) -> *Defn {
    defn: *Defn;
    #if type == {
        case .PRIMITIVE;
            defn = New(Defn);
            defn.resolved = true;
            defn.intrinsic = true;
        case .CLASS;
            defn = New(Class_Defn);
        case .VARIABLE;
            defn = New(Variable_Defn);
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

// get_current_namespace :: (using parser: *Parser) -> string {
//     if !scope return "";
    
//     builder: String_Builder;

//     if scope._type == .CLASS {
//         append(*builder,     
//     }
// }

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

    type: *Defn;

    visibility: Property_Visibility;
    is_static: bool;
    is_abstract: bool;
}

Function_Defn :: struct {
    #as using base: Defn;
    base._type = .FUNCTION;

    return_type: Defn;
    parameters: [..] *Variable_Defn;

    visibility: Property_Visibility;
    
    is_static: bool;
    is_abstract: bool;
}

Defn :: struct {
    name: string; // @Cleanup rename it to fqn (Full Qualified Name)
    _type: Defn_Type; // @Cleanup rename it to kind

    scope: *Defn;

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