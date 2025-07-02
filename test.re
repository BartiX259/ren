import lib/std
import lib/json

// --- Test Cases ---

fn test_basic_int_map() {
    print("--- Running test_basic_int_map ---\n");
    
    // 1. Create a null map reference and insert into it.
    //    This tests the lazy initialization in `insert`.
    decl m: {int: int};
    insert(&m, 10, 100);
    insert(&m, 20, 200);

    // 2. Test basic `get`
    if let val = get(m, 10) {
        assert(val == 100, "Get key 10 should return 100.");
    } else {
        assert(false, "Key 10 was not found.");
    }

    if let val = get(m, 20) {
        assert(val == 200, "Get key 20 should return 200.");
    } else {
        assert(false, "Key 20 was not found.");
    }

    // 3. Test `get` for a non-existent key
    if let val = get(m, 99) {
        assert(false, "Key 99 should not exist, but a value was found.");
    } else {
        // This is the expected path.
    }

    // 4. Test updating an existing key
    insert(&m, 10, 101);
    if let val = get(m, 10) {
        assert(val == 101, "Update for key 10 failed. Expected 101.");
    } else {
        assert(false, "Key 10 was not found after update.");
    }

    print("OK\n");
}

fn test_string_map_with_init() {
    print("--- Running test_string_map_with_init ---\n");
    
    // 1. Create a map using `init_map` with some initial data.
    decl initial_data: [(<char>, <char>)];
    push(&initial_data, ("key1", "value1"));
    push(&initial_data, ("hello", "world"));
    
    let m = init_map(initial_data[..]);

    // 2. Test getting the initial values.
    if let val = get(m, "key1") {
        assert(cmp(val, "value1"), "init_map failed for 'key1'");
    } else {
        assert(false, "'key1' not found after init_map.");
    }
    
    if let val = get(m, "hello") {
        assert(cmp(val, "world"), "init_map failed for 'hello'");
    } else {
        assert(false, "'hello' not found after init_map.");
    }

    // 3. Insert a new string key and test it.
    insert(&m, "key3", "value3");
    if let val = get(m, "key3") {
        assert(cmp(val, "value3"), "Get for 'key3' failed after insert.");
    } else {
        assert(false, "Newly inserted 'key3' not found.");
    }

    print("OK\n");
}

fn test_collisions() {
    print("--- Running test_collisions ---\n");
    
    // Assuming the initial capacity is 16.
    // Keys 0, 16, and 32 will all hash to the same initial bucket index (0).
    // This tests the linear probing collision resolution.
    decl m: {int: int};
    insert(&m, 0, 100);
    insert(&m, 16, 200);
    insert(&m, 32, 300);

    // Verify all colliding keys can be retrieved correctly.
    if let val = get(m, 0) {
        assert(val == 100, "Collision get for key 0 failed.");
    } else {
        assert(false, "Colliding key 0 not found.");
    }

    if let val = get(m, 16) {
        assert(val == 200, "Collision get for key 16 failed.");
    } else {
        assert(false, "Colliding key 16 not found.");
    }

    if let val = get(m, 32) {
        assert(val == 300, "Collision get for key 32 failed.");
    } else {
        assert(false, "Colliding key 32 not found.");
    }

    // Verify a non-existent key that also collides returns none.
    if let val = get(m, 48) {
        assert(false, "Key 48 should not be found, but it was.");
    } else {
        // Expected outcome
    }

    print("OK\n");
}

fn test_resize() {
    print("--- Running test_resize ---\n");
    let m = {5: 6};
    for i in 0..1000 {
        m[i] = i;
    }
    for i in 0..1000 {
        let s = "expected {i} got {m[i]!}";
        assert(m[i]! == i, s);
    }
    for field in iter(m) {
        assert(field.key == field.value, "q");
    }
    print("Resize ok\n");
    let j = to_json(m);
    write("asd", j);
    print("OK\n");
}


// The entry point of the test program.
fn main() {
    test_basic_int_map();
    test_string_map_with_init();
    test_collisions();
    test_resize();

    print("\nAll map tests passed successfully!\n");
}
