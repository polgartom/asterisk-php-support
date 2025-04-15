
main :: () {
    format := *context.print_style.default_format_struct;
    format.use_newlines_if_long_form = true;
    format.indentation_width = 2;

    
    //scan_dir(*lexer, "./subject");
    
    time := seconds_since_init();
    defer {
        print("\n\n--------\n\ntotal: %ms\n\n", (seconds_since_init()-time)*1000);
    }

    #if 0 {
        print("\n");
        for lexer.tokens {
            ll("%\n", it);
        }
        print("\n");
    }
    
    init(*definitions, 256);
    
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

    scan_dir("./src/app");
    
    // print_scan_result();
}

scan_file :: (filename: string) -> (success: bool) {
    lexer: Lexer;    
    ok := tokenize(*lexer, filename);
    if !ok return false;
    
    parser: Parser;
    parser.lexer  = *lexer;
    parser.tokens = lexer.tokens;
    parser.filename = filename;
    parse(*parser);
    
    if parser.failed return false;
    
    return true;
}

print_scan_result :: () {
    for definitions {
        if it._type == {
            case .CLASS;
                class := cast(*Class_Defn, it).*;
                print("\n--------\n---> %\n", class.name);
    
                print("\n[%] %\n", it, class);
    
                print("\n[properties]:\n");
                for p: class.properties {
                    print("%\n", cast(*Variable_Defn, p).*);
                }
    
                print("\n[methods]:\n");
                for p: class.methods {
                    defn := cast(*Function_Defn, p);
                    print("% -> ", defn.name);
                    print("args: ");
                    if !defn.parameters then print("<NO PARAMS> ; ");
                    for param : defn.parameters {
                        print("% (", param.name);
                        for type, i : param.types {
                            print("%", type.name);
                            if i != param.types.count-1 print(",");
                        }
                        print(") ; ");
                    }
                    print("return types: ");
                    if !defn.return_types then print("<UNKNOWN>");
                    for type : defn.return_types {
                        print("%", type.name);
                    }
                    print("\n");
                }
                    
                continue;
        }
    
        // ll("%\n", it.*);
    }
    print("\n-------------\n");    
}

scan_dir :: (dir: string) {
    visitor :: (info: *File_Visit_Info, l: *Lexer) {
        path, basename, extension, basename_with_extension := path_decomp(info.full_name);
        if extension != "php" return;
        
        //print("\n----------\n");
    
        {
            //log("% -- parsing\n", info.full_name);
            ok := scan_file(info.full_name);
            //log("% -- %\n", info.full_name, ok);
        }
        

        //ll("> % % % % ; %\n", path, basename, extension, basename_with_extension, info.full_name);
    }

    l: Lexer;
    visit_files(dir, recursive=true, *l, visitor, visit_directories=true, follow_directory_symlinks=false);
}

dd :: (fmt: string, args: .. Any) #expand {
    print("\n---------DD---------\n");

    ll(fmt, ..args);
    
    assert(false, "\n---------DD---------\n");
}

ll :: (fmt: string, args: .. Any) #expand {
    s := tprint(fmt, ..args);
    print("[%] %", #procedure_name(), s);
} @PrintLike



#load "lexer.jai";
#load "parser.jai";
#load "utils.jai";

#import "File";
#import "System";
#import "Basic";
#import "File_Utilities";
#import "String";
#import "Math";
#import "Window_Creation";
#import "Input";
#import "Unicode";
#import "GetRect";
#import "Command_Line";
#import "Hash_Table";
#import "Sort";
#import "Random";