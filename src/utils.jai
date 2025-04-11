range_to_string :: inline (c0: *u8, c1: *u8) -> string {
    return .{
        data  = c0,
        count = c1-c0
    };
}

advance :: (arr: [..] $T, step := 1) {
}

is_identifier_start_char :: (c: u8) -> bool {
    return is_alpha(c) || c == #char "_";
}

is_sepa :: inline (c: u8) -> bool {
    return c == #char " " || c == #char "\n" || c == #char "\r" || c == #char "\t" ||
           c == #char "{" || c == #char "}"  ||
           c == #char "." || c == #char ";"  ||
           c == #char ">" || c == #char "<"  ||
           c == #char "=" || c == #char "!"  ||
           c == #char "[" || c == #char "]"  ||
           c == #char "(" || c == #char ")"  ||
           c == #char "#" || c == #char "?"  ||
           c == #char "/" || c == #char "*"  ||
           c == #char "-" || c == #char "+"  ||
           c == #char "&" || c == #char "@"  ||
           c == #char "$" || c == #char ","  ||
           c == #char ":" || c == #char "|"  ||
           c == #char "^" || c == #char "%"  ||
           c == #char "~" || c == #char "\\" ||
           c == #char "\"" || c == #char "'" ||
           c == #char "`"
           ;
}

is_alpha :: inline (c: u32) -> bool{
    return (c >= #char "a" && c <= #char "z") || (c >= #char "A" && c <= #char "Z");
}

is_digit :: inline (c: u32) -> bool{
    return (c >= #char "0" && c <= #char "9");
}

is_alphanum :: inline (c: u32) -> bool {
    return is_alpha(c) || is_digit(c) || c == #char "_";
}

is_whitespace :: inline (c: u32) -> bool {
    return c == #char " " || c == #char "\n";
}

is_line_end :: inline (c: u32) -> bool {
    assert(c != #char "\r", "We should convert all of the \\r (CRs) to \\n (LRs)");
    return c == #char "\n";
}

is_hex_digit :: (c: u32) -> bool {
    return (c >= #char "a" && c <= #char "f") || (c >= #char "A" && c <= #char "F") || (c >= #char "0" && c <= #char "9");
}

is_upper :: inline (char: u8) -> bool {
    return char >= #char "A" && char <= #char "Z";
}

replace_crlf_to_lf :: inline (input: string, $autofree := false) -> (result: string, ocurrences: int) {
    #if autofree {
        defer free(input);
    }

    result, ocurrences := replace(input, "\r\n", "\n");
    return result, ocurrences;
}