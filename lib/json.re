import std

// A state object for the deserializer.
// CORRECTED: Simplified to use a single pointer for the cursor. We will pass a pointer
// to this struct (*ParserState) so its fields can be modified by helper functions.
pub type ParserState = (cursor: *char, end: *char);

// -- Serialization --

// CORRECTED: String escaping was completely wrong. It needs to push a backslash
// followed by the character code (e.g., '\' and then 'n').
fn escape_and_push_string(builder: *[char], s: <char>) {
    for c in s {
        if c == '"' {
            push(builder, '\\');
            push(builder, '"');
        } else if c == '\\' {
            push(builder, '\\');
            push(builder, '\\');
        } else if c == '\n' {
            push(builder, '\\');
            push(builder, 'n');
        } else if c == '\r' {
            push(builder, '\\');
            push(builder, 'r');
        } else if c == '\t' {
            push(builder, '\\');
            push(builder, 't');
        } else {
            push(builder, c);
        }
    }
}

fn to_json_recursive<T>(value: T, builder: *[char]) {
    match <K, V> T {
        int {
            push(builder, str(value));
        }
        bool {
            push(builder, str(value));
        }
        <char> {
            push(builder, '"');
            escape_and_push_string(builder, value);
            push(builder, '"');
        }
        *char {
            // CORRECTED: The original implementation caused infinite recursion.
            // Treat it as a standard string slice (<char>).
            push(builder, '"');
            escape_and_push_string(builder, str(value)); // Assuming str(*char) -> <char>
            push(builder, '"');
        }
        ?K {
            if let v = value {
                to_json_recursive(v, builder);
            } else {
                push(builder, "null");
            }
        }
        [K] {
            push(builder, '[');
            let first = true;
            for element in value {
                if !first { push(builder, ','); }
                to_json_recursive(element, builder);
                first = false;
            }
            push(builder, ']');
        }
        <K> {
            push(builder, '[');
            let first = true;
            for element in value {
                if !first { push(builder, ','); }
                to_json_recursive(element, builder);
                first = false;
            }
            push(builder, ']');
        }
        /*{K: V} {
            // CORRECTED: Implemented the missing logic for map serialization.
            push(builder, '{');
            let first = true;
            for (key, val) in value { // Assumes map iteration support.
                if !first { push(builder, ','); }
                // JSON object keys MUST be strings.
                to_json_recursive(str(key), builder);
                push(builder, ':');
                to_json_recursive(val, builder);
                first = false;
            }
            push(builder, '}');
        }*/
        struct {
            push(builder, '{');
            let first = true;
            for field in value { // Assumes runtime reflection support.
                if !first { push(builder, ','); }
                to_json_recursive(field.name, builder);
                push(builder, ':');
                to_json_recursive(field.value, builder);
                first = false;
            }
            push(builder, '}');
        }
    }
}

// -- Deserialization --

fn skip_whitespace(p: *ParserState) {
    while p.cursor < p.end {
        let c = *p.cursor;
        if c == ' ' || c == '\n' || c == '\r' || c == '\t' {
            p.cursor += 1;
        } else {
            break;
        }
    }
}

fn expect_char(p: *ParserState, expected: char) -> int ? <char> {
    skip_whitespace(p);
    if p.cursor >= p.end || *p.cursor != expected {
        // CORRECTED: Improved error message.
        if p.cursor >= p.end {
            return ?"Expected '{expected}' but found end of input.";
        } else {
            return ?"Expected '{expected}' but found '{*p.cursor}'.";
        }
    }
    p.cursor += 1; // Correctly advance the cursor.
    return 0;
}

fn consume_literal(p: *ParserState, literal: <char>) -> int ? <char> {
    skip_whitespace(p);
    let remaining_len = p.end - p.cursor;
    if remaining_len < len(literal) {
        return ?"Unexpected end of input while parsing literal '{literal}'.";
    }
    // This can be replaced with a library function if one exists, e.g., memcmp.
    for i in 0..len(literal) {
        if p.cursor[i] != literal[i] {
            return ?"Expected literal '{literal}'.";
        }
    }
    p.cursor += len(literal); // Correctly advance the cursor.
    return 0;
}

fn parse_number_from_stream(p: *ParserState, out: *int) -> int ? <char> {
    skip_whitespace(p);
    let start_ptr = p.cursor;
    let negative = false;
    if p.cursor < p.end && *p.cursor == '-' {
        negative = true;
        p.cursor += 1;
    }
    *out = 0;
    let found_digit = false;
    while p.cursor < p.end {
        let c = *p.cursor;
        if c >= '0' && c <= '9' {
            *out = *out * 10 + (c - '0') as int;
            p.cursor += 1;
            found_digit = true;
        } else {
            break;
        }
    }
    if !found_digit {
        p.cursor = start_ptr; // Rewind on failure
        return ?"Invalid JSON number.";
    }
    if negative { *out = -*out; }
    return 0;
}

fn parse_bool_from_stream(p: *ParserState, out: *bool) -> int ? <char> {
    skip_whitespace(p);
    // Peek at the character without consuming it.
    if p.cursor >= p.end { return ?"Expected 'true' or 'false' but found end of input."; }
    let current_char = *p.cursor;

    if current_char == 't' {
        consume_literal(p, "true")?;
        *out = true;
        return 0;
    } else if current_char == 'f' {
        consume_literal(p, "false")?;
        *out = false;
        return 0;
    }
    return ?"Expected 'true' or 'false'.";
}

fn parse_string_from_stream(p: *ParserState, out: *<char>) -> int ? <char> {
    expect_char(p, '"')?;

    decl builder: [char];
    while p.cursor < p.end {
        let c = *p.cursor;
        p.cursor += 1; // Consume the character immediately.

        if c == '"' {
            *out = builder; // Assumes `builder` can be converted to `<char>`
            return 0;
        } else if c == '\\' {
            if p.cursor >= p.end { break; } // End of input after a backslash.
            let escaped = *p.cursor;
            p.cursor += 1; // Consume the escaped character.

            if escaped == '"' { push(&builder, '"'); }
            else if escaped == '\\' { push(&builder, '\\'); }
            else if escaped == 'n' { push(&builder, '\n'); }
            else if escaped == 'r' { push(&builder, '\r'); }
            else if escaped == 't' { push(&builder, '\t'); }
            // Note: Unicode escapes like \uXXXX are not supported.
            // Other escapes like \b and \f are also not supported.
            else { 
                // JSON spec says any other escape is an error, but many parsers
                // are lenient and just pass the character through.
                return ?"Invalid escape sequence: \\{escaped}";
            }
        } else {
            push(&builder, c);
        }
    }
    return ?"Unterminated JSON string.";
}


fn parse_array<T>(p: *ParserState, out: *[T]) -> int ? <char> {
    expect_char(p, '[')?;
    skip_whitespace(p);

    if p.cursor < p.end && *p.cursor == ']' {
        p.cursor += 1;
        return 0; // Empty array
    }

    loop {
        decl element: T;
        parse_value(p, &element)?;
        push(out, element);

        skip_whitespace(p);
        if p.cursor >= p.end { return ?"Unterminated JSON array."; }
        
        let next_char = *p.cursor;
        if next_char == ']' {
            p.cursor += 1;
            break;
        } else if next_char == ',' {
            p.cursor += 1;
            // Handle invalid trailing comma like `[1, 2, ]`
            skip_whitespace(p);
            if p.cursor < p.end && *p.cursor == ']' {
                return ?"Trailing comma in JSON array.";
            }
        } else {
            return ?"Expected ',' or ']' in array.";
        }
    }
    return 0;
}

fn parse_object_into_struct<T>(p: *ParserState, out: *T) -> int ? <char> {
    expect_char(p, '{')?;
    skip_whitespace(p);

    if p.cursor < p.end && *p.cursor == '}' {
        p.cursor += 1;
        return 0; // Empty object
    }

    loop {
        decl key: <char>;
        parse_string_from_stream(p, &key)?;
        expect_char(p, ':')?;

        let found_field = false;
        // This relies on runtime reflection to iterate struct fields.
        for field in out {
            if cmp(key, field.name) {
                parse_value(p, field.value)?;
                found_field = true;
                break;
            }
        }

        if !found_field {
            // A key in the JSON did not match any field in the struct.
            // Parse and discard the value to continue parsing.
            parse_and_discard_value(p)?;
        }

        skip_whitespace(p);
        if p.cursor >= p.end { return ?"Unterminated JSON object."; }

        let next_char = *p.cursor;
        if next_char == '}' {
            p.cursor += 1;
            break;
        } else if next_char == ',' {
            p.cursor += 1;
            // Handle invalid trailing comma like `{"a":1,}`
            skip_whitespace(p);
            if p.cursor < p.end && *p.cursor == '}' {
                return ?"Trailing comma in JSON object.";
            }
        } else {
            return ?"Expected ',' or '}' in object.";
        }
    }
        print(1);

    return 0;
}


fn parse_and_discard_value(p: *ParserState) -> int ? <char> {
    skip_whitespace(p);
    if p.cursor >= p.end { return ?"Unexpected end of input."; }
    let c = *p.cursor;

    if c == '"' { decl dummy: <char>; return parse_string_from_stream(p, &dummy); }
    if c == '-' || (c >= '0' && c <= '9') { decl dummy: int; return parse_number_from_stream(p, &dummy); }
    if c == 't' || c == 'f' { decl dummy: bool; return parse_bool_from_stream(p, &dummy); }
    if c == 'n' { return consume_literal(p, "null"); }

    if c == '[' {
        p.cursor += 1; // Consume '['
        skip_whitespace(p);
        if *p.cursor == ']' { p.cursor += 1; return 0; }
        loop {
            parse_and_discard_value(p)?;
            skip_whitespace(p);
            let next_char = *p.cursor;
            if next_char == ']' { p.cursor += 1; return 0; }
            if next_char != ',' { return ?"Expected ',' or ']' in discarded array."; }
            p.cursor += 1;
        }
    }
    if c == '{' {
        p.cursor += 1; // Consume '{'
        skip_whitespace(p);
        if *p.cursor == '}' { p.cursor += 1; return 0; }
        loop {
            decl dummy_key: <char>;
            parse_string_from_stream(p, &dummy_key)?;
            expect_char(p, ':')?;
            parse_and_discard_value(p)?;
            skip_whitespace(p);
            let next_char = *p.cursor;
            if next_char == '}' { p.cursor += 1; return 0; }
            if next_char != ',' { return ?"Expected ',' or '}' in discarded object."; }
            p.cursor += 1;
        }
    }
    return ?"Invalid JSON value to discard.";
}

fn parse_value<T>(p: *ParserState, out: *T) -> int ? <char> {
    skip_whitespace(p);

    match <K> T {
        int { return parse_number_from_stream(p, out as *int); }
        bool { return parse_bool_from_stream(p, out as *bool); }
        <char> { return parse_string_from_stream(p, out as *<char>); }
        [K] { return parse_array(p, out); }
        struct { return parse_object_into_struct(p, out); }
        ?K {
            skip_whitespace(p);
            if p.cursor < p.end && *p.cursor == 'n' {
                consume_literal(p, "null")?;
                *out = none; // Set to 'none' using a safer, language- idiomatic way.
                return 0;
            } else {
                // NOTE: The original implementation used unsafe pointer arithmetic.
                // This corrected version assumes there's a safe way to construct a
                // 'some' value. The exact syntax depends on your language.
                decl val: K;
                parse_value(p, &val)?;
                *out = some(val); // Construct the 'some' variant.
                return 0;
            }
        }
    }
    return ?"Unsupported type for JSON deserialization.";
}

// --- Public API ---

pub fn to_json<T>(value: T) -> <char> {
    decl builder: [char];
    to_json_recursive(value, &builder);
    return builder;
}

pub fn from_json<T>(json_str: <char>, out: *T) -> int ? <char> {
    let start_ptr = json_str as *char;
    let end_ptr = start_ptr + len(json_str);

    // CORRECTED: The state struct `p` is now created directly and its address is passed.
    decl p: ParserState;
    p.cursor = start_ptr;
    p.end = end_ptr;

    parse_value(&p, out)?;
    skip_whitespace(&p);

    // CORRECTED: The check now correctly compares the final cursor position with the end.
    if p.cursor != p.end {
        return ?"Unexpected trailing characters in JSON input.";
    }

    return 0;
}
