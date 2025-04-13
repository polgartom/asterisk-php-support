
main :: () {
    format := *context.print_style.default_format_struct;
    format.use_newlines_if_long_form = true;
    format.indentation_width = 2;

    lexer: Lexer;    
    
    //scan_dir(*lexer, "./subject");
    
    time := seconds_since_init();
    defer {
        print("\n\n--------\n\ntotal: %ms\n\n", (seconds_since_init()-time)*1000);
    }

    ok := tokenize(*lexer, "./src/input.php");
    if !ok return;

    #if 0 {
        print("\n");
        for lexer.tokens {
            ll("%\n", it);
        }
        print("\n");
    }

    init(*definitions, 256);

    parser: Parser;
    init_parser(*parser);

    parser.lexer  = *lexer;
    parser.tokens = lexer.tokens;

    parse(*parser);

    #if 0 {
        print("\n%\n", definitions.entries.count);
        for definitions {
            if it._type == {
                case .CLASS;
                    class := cast(*Class_Defn, it).*;
                    print("%\n", class);

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
                        for type : defn.return_types {
                            print("%", type.name);
                        }
                        print("\n");
                    }

                    continue;
            }

            // ll("%\n", it.*);
        }
        print("\n");
    }

}

scan_dir :: (lexer: *Lexer, dir: string) {
    
    visitor :: (info: *File_Visit_Info, lexer: *Lexer) {
        path, basename, extension, basename_with_extension := path_decomp(info.full_name);
        if extension != "php" return;
        
        //ll("> % % % %\n", path, basename, extension, basename_with_extension);
    }

    visit_files(dir, recursive=true, lexer, visitor, visit_directories=true, follow_directory_symlinks=false);
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