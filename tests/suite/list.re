import lib/std
import lib/json

// --- Test Cases ---

fn test_basic_int_list() {
    print("--- Running test_basic_int_list ---\n");

    // 1. Create an empty list.
    decl l: [int];
    assert(len(l) == 0, "Newly created list should have length 0.");

    // 2. Push elements and check length/values by index.
    push(&l, 10);
    push(&l, 20);
    push(&l, 30);
    
    assert(len(l) == 3, "List length should be 3 after pushing 3 elements.");
    assert(l[0] == 10, "l[0] should be 10.");
    assert(l[1] == 20, "l[1] should be 20.");
    assert(l[2] == 30, "l[2] should be 30.");

    // 3. Test pop to remove the last element.
    if let val = pop(&l) {
        assert(val == 30, "Popped value should be 30.");
        assert(len(l) == 2, "List length should be 2 after pop.");
        assert(l[1] == 20, "Last element should now be 20.");
    } else {
        assert(false, "Pop should have returned a value.");
    }
    
    // 4. Push again after a pop to ensure the list is still valid.
    push(&l, 40);
    assert(len(l) == 3, "List length should be 3 after pushing again.");
    assert(l[2] == 40, "The new element should be at the end.");

    print("OK\n");
}

fn test_string_list() {
    print("--- Running test_string_list ---\n");
    
    decl l: [<char>];
    push(&l, "hello");
    push(&l, "world");

    assert(len(l) == 2, "String list length should be 2.");
    assert(cmp(l[0], "hello"), "First string should be 'hello'.");
    assert(cmp(l[1], "world"), "Second string should be 'world'.");

    if let val = pop(&l) {
        assert(cmp(val, "world"), "Popped string should be 'world'.");
    } else {
        assert(false, "Pop on string list failed.");
    }

    assert(len(l) == 1, "String list length should be 1 after pop.");
    assert(cmp(l[0], "hello"), "The remaining string should be 'hello'.");

    print("OK\n");
}

fn test_list_resize() {
    print("--- Running test_list_resize ---\n");

    decl l: [int];
    // Push enough elements to force several reallocations/resizes.
    for i in 0..1000 {
        push(&l, i * 2);
    }
    
    assert(len(l) == 1000, "List length should be 1000 after mass push.");
    
    // Verify all elements are correct by iterating and checking their values.
    for i in 0..1000 {
        assert(l[i] == i * 2, "Element at index {i} is incorrect.");
    }

    print("OK\n");
}

fn test_pop_behavior() {
    print("--- Running test_pop_behavior ---\n");

    // 1. Pop from an empty int list
    decl empty_list: [int];
    if let val = pop(&empty_list) {
        assert(false, "Pop on empty list should return none.");
    } else {
        assert(true, "Correctly returned none when popping empty list.");
    }

    // 2. Pop repeatedly until empty
    decl l: [int];
    push(&l, 1);
    push(&l, 2);
    push(&l, 3);
    
    if let val = pop(&l) {
        assert(val == 3, "First pop should return 3.");
    } else {
        assert(false, "First pop failed unexpectedly.");
    }

    if let val = pop(&l) {
        assert(val == 2, "Second pop should return 2.");
    } else {
        assert(false, "Second pop failed unexpectedly.");
    }

    if let val = pop(&l) {
        assert(val == 1, "Third pop should return 1.");
    } else {
        assert(false, "Third pop failed unexpectedly.");
    }

    // List should now be empty
    assert(len(l) == 0, "List should be empty after popping all elements.");
    if let val = pop(&l) {
        assert(false, "Pop on now-empty list should return none.");
    }

    print("OK\n");
}


fn test_list_json() {
    print("--- Running test_list_json ---\n");
    
    // 1. Create a list and serialize it to JSON.
    decl source_list: [int];
    push(&source_list, 100);
    push(&source_list, -200);
    push(&source_list, 300);

    let json_data = to_json(source_list);
    
    // 2. Write the JSON data to a file.
    let filename = "list_test.json";
    write(filename, json_data)!;

    // 3. Read the data back and deserialize into a new list.
    let read_data = read(filename)!;
    decl dest_list: [int];
    from_json(read_data, &dest_list)!;
    
    // 4. Verify the deserialized list is identical to the original.
    assert(len(dest_list) == len(source_list), "Deserialized list has wrong length.");
    for i in 0..len(source_list) {
        assert(source_list[i] == dest_list[i], "Deserialized element at {i} is wrong.");
    }
    
    // 5. Cleanup
    rm(filename)!;

    print("OK\n");
}

fn test_nested_list() {
    print("--- Running test_nested_list ---\n");
    
    decl outer: [[int]];
    
    decl inner1: [int];
    push(&inner1, 1);
    push(&inner1, 2);
    
    decl inner2: [int];
    push(&inner2, 3);

    
    push(&outer, inner1);
    push(&outer, inner2);

    assert(len(outer) == 2, "Outer list should have 2 elements.");
    assert(len(outer[0]) == 2, "First inner list should have 2 elements.");
    assert(len(outer[1]) == 1, "Second inner list should have 1 element.");
    
    assert(outer[0][0] == 1, "Nested access outer[0][0] failed.");
    assert(outer[0][1] == 2, "Nested access outer[0][1] failed.");
    assert(outer[1][0] == 3, "Nested access outer[1][0] failed.");
    
    // Also test serialization of nested lists.
    let json_data = to_json(outer);
    
    decl dest_list: [[int]];
    from_json(json_data, &dest_list)!;
    
    assert(len(dest_list) == 2, "Deserialized nested list has wrong outer length.");
    assert(len(dest_list[0]) == 2, "Deserialized nested list has wrong inner length for element 0.");
    assert(dest_list[0][1] == 2, "Deserialized nested list has wrong value.");

    print("OK\n");
}

fn test_std_list_functions() {
    print("--- Running test_std_list_functions ---\n");

    // 1. Test split and join
    let s1 = "alpha,beta,gamma";
    let parts = split(s1, ',');
    assert(len(parts) == 3, "split should produce 3 parts.");
    assert(cmp(parts[0], "alpha"), "split part 0 is wrong.");
    assert(cmp(parts[1], "beta"), "split part 1 is wrong.");
    assert(cmp(parts[2], "gamma"), "split part 2 is wrong.");

    let joined = join(parts, "; ");
    assert(cmp(joined, "alpha; beta; gamma"), "join produced wrong string.");

    // 2. Test find and contains (single element)
    let int_list = [10, 20, 30, 20];
    
    if let idx = find(int_list, 20) {
        assert(idx == 1, "find(20) should return index 1.");
    } else {
        assert(false, "find(20) should have found a value.");
    }
    
    assert(find(int_list, 99) == none, "find(99) should return none.");

    assert(contains(int_list, 30), "contains(30) should be true.");
    assert(!contains(int_list, 99), "contains(99) should be false.");
    
    // 3. Test find and contains (sub-slice)
    let haystack = "hello wonderful world";
    let needle1 = "wonderful";
    let needle2 = "goodbye";
    
    if let idx = find(haystack, needle1) {
         assert(idx == 6, "find for sub-slice failed, expected 6.");
    } else {
         assert(false, "find should have found sub-slice 'wonderful'.");
    }
    
    assert(contains(haystack, needle1), "contains for sub-slice 'wonderful' failed.");
    assert(!contains(haystack, needle2), "contains for sub-slice 'goodbye' should be false.");

    // 4. Test starts_with and ends_with
    let s2 = "prefix_data_suffix";
    assert(starts_with(s2, "prefix"), "starts_with('prefix') failed.");
    assert(!starts_with(s2, "suffix"), "starts_with('suffix') should be false.");
    assert(ends_with(s2, "suffix"), "ends_with('suffix') failed.");
    assert(!ends_with(s2, "prefix"), "ends_with('prefix') should be false.");
    
    // 5. Test to_lowercase and to_uppercase
    let mixed = "AbCdEf-123";
    let lower = to_lowercase(mixed);
    let upper = to_uppercase(mixed);
    assert(cmp(lower, "abcdef-123"), "to_lowercase failed.");
    assert(cmp(upper, "ABCDEF-123"), "to_uppercase failed.");

    print("OK\n");
}


// The entry point of the test program.
fn main() {
    test_basic_int_list();
    test_string_list();
    test_list_resize();
    test_pop_behavior();
    test_list_json();
    test_nested_list();
    test_std_list_functions();

    print("\nAll list tests passed successfully!\n");
}