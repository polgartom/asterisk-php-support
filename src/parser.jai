parse :: (using parser: *Parser) {
    assert(lexer != null);

    // We will skip the simicolons right here, so that's the parsing pattern we use.
    while t.kind != .EOF {
        // ll("> '%'\n", t.text);

        if t.kind == .KEYWORD {
            if t.text == "namespace" {
                ok := set_namespace(parser);
                if !ok return;

            } else if t.text == "use" {
                if !scope {
                    ok := parse_global_imports(parser);
                    if !ok return;
                } else {
                    report_error(parser, "You can't do import right here.\n");
                    return;
                }
            } else if t.text == "class" {
                ok := parse_class(parser);                
                if !ok return;
            }
        } else {
            if t.kind == .BLOCK_COMMENT_START {
                ok := parse_block_comment(parser);
                if !ok return;
            } else {
                report_error(parser, "Unexpected identifier: '%'\n", t.text);
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
    if !ok return false;

    fqn := ifx namespace then join(namespace, name, separator = "\\") else copy_string(name);

    decl := add_decl(fqn, .CLASS);
    decl.location = location;

    defn := New(Definition);
    decl.defn = defn;
    defn.decl = decl;
    defn.location = location;

    log("Class definition added '%'\n", decl.*);

    if t.text == "extends" {
        // @Todo:

        eat_token(parser);
        
        name, ok := get_namespaced_name(parser);
        if !ok return false;

        extends_decl: *Declaration;
        namespaced_globally := false;

        if starts_with(name, "\\") {
            namespaced_globally = true;

            name = advance(name, 1);
            extends_decl = find_decl(name);

        } else {
            // @Todo: We may not find the class because it has the current namespace, so we 
            // may need to make a queue for these unresolved type declarations or something
            import, found := find_global_import(parser, name);
            if found {
                assert(import.decl != null);
                extends_decl = import.decl;

            } else {
                log("Class global import not found '%', maybe it's in the same namespace but we didn't find yet.\n", name);
            }
        }

        if !extends_decl {
            fqn := name;
            if !namespaced_globally {
                // Maybe it's in the current namespace, because we didn't find it in among imports.
                fqn = ifx namespace then join(namespace, name, separator = "\\") else copy_string(name);
            }

            // @Todo We do know that is a .CLASS declaration but we don't know its definition's location, so
            // we should check by giving it an "unresolved" flag instead of checkin the .UNKNOWN kind declarations.
            extends_decl = add_decl(fqn, .UNKNOWN);
        }

        log("Extends with '%'\n", extends_decl.*);
    }

    if t.text == "imports" {
        // @Todo:
    }

    if t.kind != .LEFT_BRACE {
        report_error(parser, "'{' expected after class declaration but '%' given.\n", t.text);
        return false;
    }

    block_index += 1;
    scope = decl;

    while true {
        eat_token(parser);

        if t.kind == .EOF {
            report_error(parser, "Invalid end of the '%' class.\n", decl.name);
            return false;
        }

        if t.text == "use" {
            if scope.defn.traits.count {
                report_error(parser, "Invalid trait import\n");
                return false;
            }

            ok := parse_trait_imports(parser);
            if !ok return false;
        }
    }

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
    if !ok return false;

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

            log("Import added '%'\n", import.*);
        }

    } else {
        import := array_add(*imports);
        import.name = copy_string(name);
        import.decl = decl;
        import.alias_for = decl; // Do we need this?

        log("Import added '%'\n", import.*);
    }

    if peak_next_token(parser).text == "use" {
        // Do we want to handle imports that end with an open bracket?
        ok = expects_and_eat_token(parser, .SEMICOLON);
        if !ok return false;

        return parse_global_imports(parser);
    }

    if t.kind != .SEMICOLON {
        report_error(parser, "';' expected after import but '%' given.\n", t.text);
        return false;
    }

    return true;
}

parse_trait_imports :: (using parser: *Parser) -> bool {
    assert(t.text == "use");
    eat_token(parser);

    name, ok := get_namespaced_name(parser);
    if !ok return false;

    trait_decl: *Declaration;
    namespaced_globally := false;
    if starts_with(name, "\\") {
        namespaced_globally = true;

        name = advance(name, 1);
        trait_decl = find_decl(name);
    } else {
        // @Todo: We may not find the trait because it has the current namespace, so we 
        // may need to make a queue for these unresolved type declarations or something
        import, found := find_global_import(parser, name);
        if found {
            assert(import.decl != null);
            trait_decl = import.decl;

        } else {
            log("Trait global import not found '%', maybe it's in the same namespace but we didn't find yet.\n", name);
        }
    }

    if !trait_decl {
        fqn := name;
        if !namespaced_globally {
            // Maybe it's in the current namespace, because we didn't find it among imports.
            fqn = ifx namespace then join(namespace, name, separator = "\\") else copy_string(name);
        }

        // @Todo We do know that is a .TRAIT declaration but we don't know its definition's location, so
        // we should check by giving it an "unresolved" flag instead of checkin the .UNKNOWN kind declarations.
        trait_decl = add_decl(fqn, .UNKNOWN);
    }

    log("Trait added '%' ; decl: [%]\n", trait_decl.*, trait_decl);

    // @Todo: Handle aliases
    ok = expects_and_eat_token(parser, .SEMICOLON);
    if !ok return false;

    if t.text == "use" {
        return parse_trait_imports(parser);
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
    if !ok return false;

    // Do we want to handle namespaces that end with an open bracket?
    if t.kind != .SEMICOLON {
        report_error(parser, "';' expected but '%' given.\n", t.text);
        return false;
    }

    namespace = copy_string(name);

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
        if t.kind == .BACKSLASH {
            if backslash {
                report_error(parser, "Unexpected namespaced name '%', expecting an identifier after backslash.\n", range_to_string(c0, c1));
                return "", false;
            }

            backslash = true;

        } else if t.kind == .IDENTIFIER || t.kind == .KEYWORD { // .KEYWORD is valid only if we have .BACKSLASH before or after
            if !backslash && c0 != c1 {
                break;
            }

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
    return s, s.count > 0;
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

        array_add(*list, name);

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

find_global_import :: (using parser: *Parser, name: string) -> *Import, bool {
    assert(!starts_with(name, "\\"));

    // log("imports: %\n", imports);
    for * imports {
        // log("> compare: '%' <> '%'\n", fqn_last_piece(it.name), name);
        if fqn_last_piece(it.name) == name {
            return it, true;
        }
    }

    return null, false;
}

parse_block_comment :: (using parser: *Parser) -> bool {
    assert(t.kind == .BLOCK_COMMENT_START);
    
    while true {
        eat_token(parser);

        if t.kind == .EOF {
            report_error(parser, "Invalid block comment end\n");
            return false;
        }

        // It is valid in php?
        if t.kind == .BLOCK_COMMENT_START {
            report_error(parser, "Nested block comment found!\n");
            return false;
        }

        if t.kind == .BLOCK_COMMENT_END {
            break;
        }
    }

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

eat_token :: (using p: *Parser) -> Token {
    if at+1 < tokens.count {
        at += 1;
        t = tokens[at];
    }

    return t;
}

peak_token :: inline (using p: *Parser, ahead: s32 = 1) -> Token {
    if at + ahead < tokens.count {
        return tokens[at + ahead];
    }

    return .{kind=.EOF, line=0, pos=0};
}

peak_next_token :: inline (using parser: *Parser) -> Token {
    return peak_token(parser, ahead=1);
}

fqn_last_piece :: (fqn: string) -> string {
    index := find_index_from_right(fqn, "\\");
    if index == -1 return fqn;
    index += 1;
    return slice(fqn, index, fqn.count-index);
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
    imports: [..] Import;
    scope: *Declaration;
    block_index: s32;

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

    traits: [..] *Declaration; // @Temporary
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