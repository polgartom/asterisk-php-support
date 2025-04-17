parse :: (using parser: *Parser) {
    assert(lexer != null);

    // We will skip the simicolons right here, so that's the parsing pattern we use.
    while t.kind != .EOF {

        if t.kind == .KEYWORD {
            if t.text == "namespace" {
                ok := set_namespace(parser);
                if !ok return;

            } else if t.text == "use" {
                if !scope {
                    ok := parse_global_imports(parser);
                    if !ok return;

                } else if scope.kind == .CLASS {
                    // @Todo: Parse trait imports
                } else {
                    report_error(parser, "You can't do import right here.\n");
                }
            } else if t.text == "class" {
                ok := parse_class(parser);                
                if !ok return;
            }
        }

        eat_token(parser);
    }

}

parse_class :: (using parser: *Parser) -> bool {
    assert(t.text == "class");
    eat_token(parser);

    if scope {
        report_error(parser, "Invalid class declaration/definition in '%' scope.\n", scope.name);
        return false;
    }

    // It is correct?
    if t.kind == .BACKSLASH {
        report_error(parser, "Class definition cannot start with '\\'.\n");
        return false;
    }

    location := Location.{
        pos=t.pos,
        line=t.line,
        filename=filename
    };

    name, ok := get_namespaced_name(parser);
    if !ok {
        if !error_reported {
            report_error(parser, "Invalid class name '%'\n", name);
        }
        return false;
    }

    fqn := ifx namespace then join(namespace, name, separator = "\\") else copy_string(name);

    decl := add_decl(fqn, .CLASS);
    decl.location = location;

    defn := New(Definition);
    decl.defn = defn;
    defn.decl = decl;
    defn.location = location;

    scope = decl;

    log("Class definition added '%'\n", decl.name);

    return true;
}

// @Todo: Handle this variant too:
// use Mizo\Web\ {
//    Php\WebSite,
//    Php\KeyWord,
//    Php\UnicodePrint,
//    JS\JavaScript, 
//    function JS\printTotal, 
//    function JS\printList, 
//    const JS\BUAIKUM, 
//    const JS\MAUTAM
// };
parse_global_imports :: (using parser: *Parser) -> bool {
    assert(t.text == "use");
    eat_token(parser);

    // It is correct?
    if t.kind == .BACKSLASH {
        report_error(parser, "Import FQN cannot start with '\\' (I guess).\n");
        return false;
    }

    name, ok := get_namespaced_name(parser);
    if !ok {
        if !error_reported {
            report_error(parser, "Invalid FQN '%'\n", name);
        }
        return false;
    }

    decl, found := find_decl(name);
    if !found {
        fqn := copy_string(name);
        decl = add_decl(fqn, .UNKNOWN);
    }

    if t.text == "as" {
        ok=, list := parse_comma_separated_import_aliases(parser, decl);
        if !ok return false;

        // @Todo: Check for import name conflicts
        for name: list {
            import := array_add(*imports);
            import.name = copy_string(name);
            import.decl = decl;
            import.alias_for = decl; // Do we need this?
            import.is_alias = true;
        }
    }

    // Do we want to handle imports that end with an open bracket?
    ok = expects_and_eat_token(parser, .SEMICOLON);
    if !ok return false;

    log("Import added '%'\n", decl.*);

    if t.text == "use" {
        return parse_global_imports(parser);
    }

    return true;
}

set_namespace :: (using parser: *Parser) -> bool {
    if scope {
        report_error(parser, "You can't specify a namespace here, because we're not in the global scope. Current scope: '%'\n", scope.name);
        return false;        
    }

    if namespace {
        report_error(parser, "We already have a namespace as '%'!", namespace);
        return false;
    }

    if imports.count {
        // It' not an error if we currently in the global scope
        report_error(parser, "We already have a namespace as '%'!", namespace);
        return false;
    }

    if t.kind == .BACKSLASH {
        report_error(parser, "Namespace cannot start with '\\'");
        return false;
    }

    eat_token(parser);
    name, ok := get_namespaced_name(parser);
    if !ok {
        if !error_reported {
            report_error(parser, "Invalid namespace '%'\n", name);
        }
        return false;
    }

    // Do we want to handle namespaces that end with an open bracket?
    if t.kind != .SEMICOLON {
        report_error(parser, "';' expected but '%' given.\n", t.text);
        return false;
    }

    namespace = copy_string(name);

    return true;
}

expects_and_eat_token :: (using parser: *Parser, expects: string) -> bool {
    if t.text != expects {
        report_error(parser, "'%' expected, '%' given.", expects, t.text);
        return false;
    }

    eat_token(parser);
    return true;
}

expects_and_eat_token :: (using parser: *Parser, expects: Token_Kind) -> bool {
    if t.kind != expects {
        report_error(parser, "'%' expected, '%' given.", expects, t.kind);
        return false;
    }

    eat_token(parser);
    return true;
}

get_namespaced_name :: (using parser: *Parser) -> string, bool {
    if !t.text return "", false;
    
    starts_with_backslash := false;
    backslash             := false;

    if t.kind == .BACKSLASH {
        starts_with_backslash = true;
        backslash             = true;
    }

    c0 := t.text.data;
    c1 := t.text.data;

    while true {
        if t.kind == .EOF {
            return "", false;
        }

        if t.kind == .BACKSLASH {
            if backslash {
                report_error(parser, "Unexpected namespaced name '%', expecting an identifier after backslash.\n", range_to_string(c0, c1));
                return "", false;
            }

            backslash = true;

        } else if t.kind == .IDENTIFIER {
            backslash = false;

            c1 = t.text.data + t.text.count;

        } else {
            if backslash {
                report_error(parser, "Unexpected namespaced name '%', expecting an identifier after backslash.\n", range_to_string(c0, c1));
                return "", false;
            }

            break;
        }

        eat_token(parser);
    }

    s := range_to_string(c0, c1);
    return s, true;
} 

parse_comma_separated_import_aliases :: (using parser: *Parser, alias_for: *Declaration) -> (success: bool #must, list: [] string) {
    assert(t.text == "as");
    assert(alias_for != null);

    eat_token(parser);

    if t.kind == .BACKSLASH {
        report_error(parser, "Import alias cannot start with '\\' (I guess)");
        return false, .[];
    }

    list: [..] string;
    list.allocator = temp;

    while true {
        name, ok := get_namespaced_name(parser);
        if !ok {
            if !error_reported {
                report_error(parser, "Invalid alias name '%'\n", name);
            }
            return false, .[];
        }

        if t.kind != .COMMA break;
        eat_token(parser);
    }

    return true, list;
}

find_decl :: (fqn: string) -> *Declaration, bool {
    decl, found := table_find(*declarations, fqn);
    return decl, found;
}

add_decl :: (fqn: string, kind: Declaration_Kind) -> *Declaration {
    decl := New(Declaration);
    decl.kind = kind;
    decl.name = fqn;

    _, found := table_find(*declarations, fqn);
    assert(!found, "'%' is already added.\n", fqn);

    table_add(*declarations, fqn, decl);

    return decl;
}

eat_token :: (using p: *Parser) -> Token {
    if at+1 < tokens.count {
        at += 1;
        t = tokens[at];
    }

    return t;
}

peak_next_token :: (using p: *Parser) -> Token {
    if at+1 < tokens.count {
        return tokens[at+1];
    }

    return .{kind=.EOF, line=0, pos=0};
}

report_error :: (parser: *Parser, fmt: string, args: .. Any) {
    parser.failed = true;
    parser.error_reported = true;

    line_str := get_line_as_string(parser.lexer, parser.t.line);
    s := tprint(fmt, ..args);
    log_error("[error] |%| %:% -> %\n", line_str, parser.t.line, parser.t.pos, s);
} @PrintLike


declarations: Table(string, *Declaration);


Parser :: struct {
    lexer: *Lexer;
    tokens: [] Token;
    t := Token.{kind=.EOF};
    at: s32;

    failed: bool;
    error_reported: bool;

    namespace: string;
    scope: *Declaration;
    imports: [..] Import;

    filename: string;
}

Declaration :: struct {
    name: string;
    kind: Declaration_Kind; 
    scope: *Declaration;
    
    defn: *Definition;
    location: Location;
}

Declaration_Kind :: enum u8 {
    UNKNOWN :: 0;
    PRIMITIVE;
    CLASS;
    TRAIT;
    INTERFACE;
    FUNCTION;
}

Definition :: struct {
    decl: *Declaration;
    
    location: Location;
}

Import :: struct {
    name: string;
    decl: *Declaration;
    is_alias: bool;
    alias_for: *Declaration; // Do we need this? We have "name" and "decl" already, so what's for?
}

Location :: struct {
    pos: s32;
    line: s32;
    filename: string;
}