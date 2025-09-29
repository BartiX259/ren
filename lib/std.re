syscall 0: read(int, *char, int) -> int;
syscall 1: write(int, *char, int) -> int;
syscall 2: open(*char, int, int) -> int;
syscall 5: fstat(int, *any) -> int;
syscall 6: close(int) -> int;
syscall 9: mmap(*any, int, int, int, int, int) -> *any;
syscall 10: munmap(*any, int);
syscall 57: fork() -> int;
syscall 59: execve(*char, **char, **char) -> int;
syscall 61: wait4(int, *int, int, *any) -> int;
syscall 82: rename(*char, *char) -> int;
syscall 83: mkdir(*char, int) -> int;
syscall 87: unlink(*char) -> int;
syscall 84: rmdir(*char) -> int;
syscall 217: getdents64(int, *any, int) -> int;
pub syscall 60: exit(int);

// -- ARG PARSE --

pub fn print_help(name: *char, expected: <<char>>) {
    print("Usage: {name} ");
    let start = true;
    for e in expected {
        if start {
            start = false;
        } else {
            print(" ");
        }
        print("[{e}]");
    }
    print('\n');
}

pub fn arg_parse(args: <*char>, expected: <<char>>) -> int ? <char> {
    for a in args {
        if a[0] == '-' {
            let ret = "Invalid flag '{a}'.\n";
            return ?ret;
        }
    }
    if len(args) - 1 != len(expected) {
        let ret = "Invalid argument count, expected {len(expected)} argument(s), got {len(args) - 1}.\n";
        return ?ret;
    }
    return 0;
}

pub fn parse(slice: <char>, res: *int) -> int ? <char> {
    *res = 0;
    let mult = 1;
    let i = len(slice);
    loop {
        i -= 1;
        if i < 0 {
            break;
        }
        let temp = slice[i] - '0';
        if temp > 9 || temp < 0 {
            return ?"Couldn't parse '{slice}' into an integer.\n";
        }
        *res += temp as int * mult;
        mult *= 10;
    }
    return 0;
}

pub fn parse(slice: <char>, res: *<char>) -> int ? <char> {
    copy(&slice, res, 16);
    return 0;
}

pub fn parse(str: *char, res: *int) -> int ? <char> {
    let slice = str(str);
    return parse(slice, res);
}

pub fn parse(str: *char, res: *<char>) -> int ? <char> {
    let slice = str(str);
    return parse(slice, res);
}

fn shift_args(argc: *int, argv: **char, start: int, count: int) {
    for i in start..*argc-count {
        let ptr = argv + i;
        *ptr = argv[i+count];
    }
    *argc -= count;
}

fn single_flag(arg: <char>, expected: char) -> (found: bool, new_flag: <char>) {
    if len(arg) < 2 {
        return (found: false, new_flag: arg);
    }
    if arg[0] != '-' {
        return (found: false, new_flag: arg);
    }
    let found = false;
    let new_flag = +"-";
    for c in arg[1..] {
        if !is_alpha(c) {
            print("not alpha {c}\n");
            return (found: false, new_flag: arg);
        }
        if c == expected {
            found = true;
        } else {
            push(&new_flag, c);
        }
    }
    return (found: found, new_flag: new_flag[..]);
}

pub fn parse_opt<T>(argc: *int, argv: **char, name: <char>, opt: *?T) -> int ? <char> {
    let flag = "--{name}";
    for i in 0..*argc {
        let arg = argv[i];
        if cmp(arg, flag) {
            *(opt as *int) = 0;
            if i >= *argc - 1 {
                return ?"Optional argument required for '{name}'.\n";
            }
            let res = parse(argv[i+1], (opt as *int + 1) as *T);
            shift_args(argc, argv, i, 2);
            return res;
        } else {
            let found, new_flag = single_flag(str(arg), name[0]);
            if found {
                *(opt as *int) = 0;
                if i >= *argc - 1 {
                    return ?"Optional argument required for '{name}'.\n";
                }
                let res = parse(argv[i+1], (opt as *int + 1) as *T);
                if len(new_flag) > 1 {
                    let ptr = null_terminate(new_flag);
                    argv[i] = ptr;
                } else {
                    shift_args(argc, argv, i, 2);
                }
                return res;
            }
        }
    }
    *(opt as *int) = 1;
    return 0;
}

pub fn parse_flag(argc: *int, argv: **char, name: <char>, opt: *bool) -> int ? <char>  {
    let flag = "--{name}";
    for i in 0..*argc {
        let arg = argv[i];
        if cmp(arg, flag) {
            *(opt as *bool) = true;
            shift_args(argc, argv, i, 1);
            return 0;
        }
    }
    *(opt as *bool) = false;
    return 0;
}

pub fn cmp(l: int, r: int) -> bool {
    return l == r;
}

pub fn cmp(l: <char>, r: <char>) -> bool {
    if len(l) != len(r) {
        return false;
    }
    for i in 0..len(r) {
        if l[i] != r[i] {
            return false;
        }
    }
    return true;
}

pub fn cmp(l: *char, r: <char>) -> bool {
    let found = true;
    for i in 0..len(r) {
        if l[i] != r[i] {
            found = false;
            break;
        }
    }
    if found && l[len(r)] == 0 {
        return true;
    }
    return false;
}

// -- PRINTING --

fn int2str(x: int, buf: *char) -> *char {
    if x < 10 {
        *buf = '0' + x;
        return buf + 1;
    }
    buf = int2str(x / 10, buf);
    *buf = '0' + (x % 10);
    return buf + 1;
}

pub fn str(x: int) -> <char> {
    let start_ptr = alloc(32) as *char;
    let current_ptr = start_ptr;
    if x < 0 {
        *current_ptr = '-';
        current_ptr += 1;
        x = -x;
    }
    let end_ptr = int2str(x, current_ptr);
    return (end_ptr - start_ptr, start_ptr);
}

pub fn str(x: <char>) -> <char> {
    return x;
}

pub fn str(x: bool) -> <char> {
    if x {
        return "true";
    } else {
        return "false";
    }
}

pub fn str(x: char) -> <char> {
    let ptr = alloc(1) as *char;
    *ptr = x;
    return (1, ptr);
}

pub fn str(x: *char) -> <char> {
    let len = 0;
    let ptr = x;
    while *ptr != 0 {
        len += 1;
        ptr += 1;
    }
    return (len, x);
}

pub fn str(x: *any) -> <char> {
    return str(x as int);
}

pub fn str<T>(x: ?T) -> <char> {
    if let s = x {
        return str(s);
    } else {
        return "none";
    }
}

pub fn str<T, E>(x: T ? E) -> <char> {
    let ok = x else err {
        return str(err);
    };
    return str(ok);
}

pub fn str<T>(x: T) -> <char> {
    decl s: [char];
    match <K, V> T {
        struct { push(&s, '('); }
        tuple { push(&s, '('); }
        {K: V} { push(&s, '{'); }
        K { push(&s, '['); }
    }
    let start = true;
    for field in x {
        if start {
            start = false;
        } else {
            push(&s, ", ");
        }
        match <K, V> T {
            struct { push(&s, "{field.name}: {field.value}"); }
            {K: V} { push(&s, "{field.key}: {field.value}"); }
            K { push(&s, str(field)); }
        }
    }
    match <K, V> T {
        struct { push(&s, ')'); }
        tuple { push(&s, ')'); }
        {K: V} { push(&s, '}'); }
        K { push(&s, ']'); }
    }
    return s;
}

pub fn print<T>(msg: T) {
    let sl = str(msg);
    write(1, sl as *char, len(sl));
}

pub fn eprint<T>(msg: T) {
    let sl = str(msg);
    write(2, sl as *char, len(sl));
}

pub fn panic() {
    eprint("PANIC\n");
    exit(1);
}

pub fn panic<T>(x: T) {
  eprint("PANIC: ");
  eprint(x);
  eprint('\n');
  exit(1);
}

pub fn assert(condition: bool, message: <char>) {
    if !condition {
        eprint("ASSERTION FAILED: ");
        eprint(message);
        eprint('\n');
        exit(1);
    }
}

// -- LISTS AND SLICES --

pub fn push<T>(list: *[T], el: T) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(T) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(T);
    if size >= cap {
		let new_ptr = alloc(cap * 2);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(&el, ptr as *T + len(list), sizeof(T));
	*(list as *any as *int) += 1;
}

pub fn push<T>(list: *[T], sl: <T>) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(T) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(T);
	let add_size = len(sl) * sizeof(T);
    if size + add_size > cap {
		let new_ptr = alloc(cap + add_size);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(sl as *T, ptr as *T + len(list), add_size);
	*(list as *int) += len(sl);
}

pub fn pop<T>(list: *[T]) -> ?T {
    let ptr = *(list as **any + 1);
	if ptr == null {
		return none;
	}
    if len(list) == 0 {
        return none;
    }
    *(list as *int) -= 1;
    return *(ptr as *T + len(list));
}

pub fn split<T>(sl: <T>, split: T) -> [<T>] {
	let base = 0;
	decl res: [<T>];
	for i in 0..len(sl) {
		if sl[i] == split {
			let s = sl[base..i];
			push(&res, s);
			base = i + 1;
		}
	}
	push(&res, sl[base..]);
	return res;
}

// Finds the first index of a value in a slice.
pub fn find<T>(slice: <T>, value: T) -> ?int {
    for i in 0..len(slice) {
        if slice[i] == value {
            return i;
        }
    }
    return none;
}

// Checks if a slice contains a given value.
pub fn contains<T>(slice: <T>, value: T) -> bool {
    if let ok = find(slice, value) {
        return true;
    }
    return false;
}

// Finds the starting index of the first occurrence of a "needle" slice within a "haystack" slice.
pub fn find<T>(haystack: <T>, needle: <T>) -> ?int {
    if len(needle) == 0 {
        return 0;
    }
    if len(needle) > len(haystack) {
        return none;
    }

    let last_possible_start = len(haystack) - len(needle);
    
    for i in 0..last_possible_start + 1 {
        let window = haystack[i..i + len(needle)];

        let found = true;
        for j in 0..len(needle) {
            if haystack[i + j] != needle[j] {
                found = false;
                break;
            }
        }
        if found {
            return i;
        }
    }

    return none;
}

pub fn map<T, U>(sl: <T>, f: fn(T)->U) -> <U> {
    let new_sl = (len(sl), alloc(len(sl) * sizeof(U))) as [U];
    for i in 0..len(sl) {
        new_sl[i] = f(sl[i]);
    }
    return new_sl;
}

pub fn filter<T>(sl: <T>, f: fn(T)->bool) -> <T> {
    decl new_sl: [T];
    for s in sl {
        if f(s) {
            push(&new_sl, s);
        }
    }
    return new_sl;
}

pub fn reduce<T, U>(sl: <T>, f: fn(U, T) -> U, initial: U) -> U {
    let accumulator = initial;
    for item in sl {
        accumulator = f(accumulator, item);
    }
    return accumulator;
}

pub fn some<T>(sl: <T>, f: fn(T) -> bool) -> bool {
    for item in sl {
        if f(item) {
            return true;
        }
    }
    return false;
}

pub fn all<T>(sl: <T>, f: fn(T) -> bool) -> bool {
    for item in sl {
        if !f(item) {
            return false;
        }
    }
    return true;
}

pub fn sum(sl: <int>) -> int {
    let s = 0;
    for item in sl {
        s += item;
    }
    return s;
}

pub fn zip<T, U>(sl1: <T>, sl2: <U>) -> <(T, U)> {
    let min_len = len(sl1);
    if len(sl2) < min_len {
        min_len = len(sl2);
    }
    let new_sl = (min_len, alloc(min_len * sizeof((T, U)))) as [(T, U)];
    for i in 0..min_len {
        new_sl[i] = (sl1[i], sl2[i]);
    }
    return new_sl;
}

pub fn is_alpha(ch: char) -> bool {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z');
}

// Checks if a "haystack" slice contains a "needle" slice.
pub fn contains<T>(haystack: <T>, needle: <T>) -> bool {
    if let ok = find(haystack, needle) {
        return true;
    }
    return false;
}

// Checks if a slice starts with a given prefix.
pub fn starts_with<T>(slice: <T>, prefix: <T>) -> bool {
    if len(prefix) > len(slice) {
        return false;
    }
    
    for i in 0..len(prefix) {
        if slice[i] != prefix[i] {
            return false;
        }
    }
    return true;
}

// Checks if a slice ends with a given suffix.
pub fn ends_with<T>(slice: <T>, suffix: <T>) -> bool {
    if len(suffix) > len(slice) {
        return false;
    }
    
    let slice_len = len(slice);
    let suffix_len = len(suffix);
    for i in 0..suffix_len {
        if slice[slice_len - suffix_len + i] != suffix[i] {
            return false;
        }
    }
    return true;
}

// Joins a slice of slices into a single new list, separated by a separator slice.
pub fn join<T>(list_of_slices: <<T>>, separator: <T>) -> [T] {
    decl builder: [T];
    if len(list_of_slices) == 0 {
        return builder;
    }

    let first = true;
    for s in list_of_slices {
        if !first {
            push(&builder, separator);
        }
        push(&builder, s);
        first = false;
    }
    return builder;
}

// Creates a new string with all ASCII characters converted to lowercase.
pub fn to_lowercase(s: <char>) -> [char] {
    decl builder: [char];
    for c in s {
        if c >= 'A' && c <= 'Z' {
            push(&builder, c + 32); // 'a' - 'A' = 32
        } else {
            push(&builder, c);
        }
    }
    return builder;
}

// Creates a new string with all ASCII characters converted to uppercase.
pub fn to_uppercase(s: <char>) -> [char] {
    decl builder: [char];
    for c in s {
        if c >= 'a' && c <= 'z' {
            push(&builder, c - 32);
        } else {
            push(&builder, c);
        }
    }
    return builder;
}

// -- HASHMAP ---

pub fn hash(x: int) -> int {
    let hash = 14695981039346656037;
    for i in 0..8 {
        hash ^= (x >> (i * 8)) & 255;
        hash *= 1099511628211;
    }
    return hash;
}

pub fn hash(x: <char>) -> int {
    let hash = 14695981039346656037;
    for c in x {
        hash ^= c as int;
        hash *= 1099511628211;
    }
    return hash;
}

pub fn init_map<K, V>(fields: <(K, V)>) -> {K: V} {
    let capacity = 100;
    let ptr = calloc(capacity * sizeof((int, K, V))) as *(int, K, V);
    for f in fields {
        let i = hash(f[0]) % capacity;
        let f_ptr = (ptr + i) as *int;
        *f_ptr = 1;
        *((f_ptr + 1) as *(K, V)) = f;
    }
    return ptr as {K: V};
}

pub fn get<K, V>(map: {K: V}, key: K) -> ?V {
    if map as *any == null {
        return none;
    }
    let capacity = *(map as *int - 1) / sizeof((int, K, V));
    let i = hash(key) % capacity;
    let original_i = i;
    let f_ptr = (map as *(int, K, V) + i) as *int;
    loop {
        if *f_ptr == 0 {
            return none;
        }
        if *f_ptr == 1 && cmp(*((f_ptr + 1) as *any as *K), key) {
            break;
        }
        i = (i+1) % capacity;
        if i == original_i {
            return none;
        }
        f_ptr = (map as *(int, K, V) + i) as *int;
    }
    return *((f_ptr as *(int, K) + 1) as *any as *V);
}

pub fn insert<K, V>(map_ref: *{K: V}, key: K, value: V) {
    if *(map_ref as **any) == null {
        *map_ref = init_map((0, null) as <(K, V)>);
    }
    let map = *map_ref;
    let capacity = *(map as *int - 1) / sizeof((int, K, V));
    let i = hash(key) % capacity;
    let original_i = i;
    let f_ptr = (map as *(int, K, V) + i) as *int;
    loop {
        if *f_ptr != 1 {
            break;
        }
        if cmp(*((f_ptr + 1) as *K), key) {
            break;
        }
        i = (i+1) % capacity;
        if i == original_i {
            let new_ref = calloc(capacity * 2 * sizeof((int, K, V))) as {K: V};
            for j in 0..capacity {
                let entry_ptr = (map as *(int, K, V) + j) as *int;
                let k_ptr = (entry_ptr + 1) as *K;
                let v_ptr = (k_ptr + 1) as *V;
                insert(&new_ref, *k_ptr, *v_ptr);
            }
            insert(&new_ref, key, value);
            *map_ref = new_ref;
            return;
        }
        f_ptr = (map as *(int, K, V) + i) as *int;
    }
    *f_ptr = 1;
    *((f_ptr + 1) as *K) = key;
    *((f_ptr as *(int, K) + 1) as *V) = value;
}

pub fn remove<K, V>(map: {K: V}, key: K) -> ?V {
    if map == null {
        return none;
    }
    let capacity = *(map as *int - 1) / sizeof((int, K, V));
    let i = hash(key) % capacity;
    let original_i = i;
    let f_ptr = (map as *(int, K, V) + i) as *int;
    loop {
        if *f_ptr == 0 {
            return none;
        }
        if *f_ptr == 1 && cmp(*((f_ptr + 1) as *K), key) {
            break;
        }
        i = (i+1) % capacity;
        if i == original_i {
            return none;
        }
        f_ptr = (map as *(int, K, V) + i) as *int;
    }
    *f_ptr = 2;
    return *((f_ptr as *(int, K) + 1) as *V);
}

pub fn iter<K, V>(map: {K: V}) -> [(key: K, value: V)] {
    decl res: [(key: K, value: V)];
    if map as *any == null {
        return res;
    }

    let capacity = *(map as *int - 1) / sizeof((int, K, V));
    
    // --- PASS 1: Count the items. Read-only and GC-safe. ---
    // This loop performs no allocations, so it cannot trigger the GC.
    let count = 0;
    for i in 0..capacity {
        // Pointer to the start of the i-th entry in the map's internal array.
        let entry_base_ptr = (map as *char) + (i * sizeof((int, K, V)));
        
        // The 'occupied' flag is the first int in the entry.
        let occupied_flag = *(entry_base_ptr as *int);
        
        if occupied_flag == 1 {
            count += 1;
        }
    }

    if count == 0 {
        return res;
    }

    // --- ALLOCATION: Allocate a single block for the result list's data. ---
    // This `calloc` might trigger the GC, which could move the original `map`.
    // This is safe because we are finished with Pass 1 and will re-read `map`'s
    // (potentially new) location in Pass 2.
    let data_ptr = calloc(count * sizeof((key: K, value: V))) as *(key: K, value: V);
    
    // Manually construct the slice header for `res`.
    // We assume the slice/list layout is `(length: int, data: *any)`.
    *(&res as *int) = count;
    *((&res as *int) + 1) = data_ptr as int;

    // --- PASS 2: Copy items into the pre-allocated buffer. ---
    // This loop is now guaranteed to be allocation-free and thus GC-safe.
    let current_index = 0;
    for i in 0..capacity {
        let entry_base_ptr = (map as *char) + (i * sizeof((int, K, V)));
        let occupied_flag = *(entry_base_ptr as *int);

        if occupied_flag == 1 {
            // Pointer to the (K, V) pair within the map entry. It starts right after the int flag.
            let source_kv_ptr = (entry_base_ptr + sizeof(int)) as *(K, V);
            
            // Pointer to the destination slot in our new data buffer.
            let dest_kv_ptr = data_ptr + current_index;

            // Perform the struct copy.
            *dest_kv_ptr = *source_kv_ptr;

            current_index += 1;
        }
    }

    return res;
}

// --- FILESYSTEM ---

pub fn null_terminate(s: <char>) -> *char {
    let new = alloc(len(s) + 1) as *char;
    copy(s as *char, new, len(s));
    new[len(s)] = 0;
    return new;
}

pub fn read(path: <char>) -> [char] ? <char> {
    let fd = open(null_terminate(path), 0, 0);
    if fd < 0 {
        return ?"Failed to open file.";
    }

    decl st: int[18];
    if fstat(fd, &st) < 0 {
        close(fd);
        return ?"Failed to stat file.";
    }

    let size = st[6];
    let ptr = mmap(null, size, 3, 2, fd, 0) as *char;
    close(fd);
    
    let res = +(ptr[..size]);
    munmap(ptr, size);
    return res;
}

pub fn write(path: <char>, data: <char>) -> int ? <char> {
    let fd = open(null_terminate(path), 577, 420);
    if fd < 0 {
        return ?"Failed to open file for writing.\n";
    }

    let len = len(data);
    let ptr = data as *char;
    let written = write(fd, ptr, len);
    close(fd);

    if written < len {
        return ?"Failed to write full data to file.\n";
    }
}

pub fn read_char() -> ?char {
    decl buf: char;
    let bytes_read = read(0, &buf as *char, 1);
    if bytes_read == 1 {
        return buf;
    } else if bytes_read == 0 {
        return none;
    } else {
        eprint("read_char error\n");
        return none;
    }
}

pub fn input() -> [char] {
    decl line: [char];
    loop {
        if let c = read_char() {
            if c == '\n' {
                break;
            }
            if c == '\r' {
                continue;
            }
            push(&line, c);
        } else {
            break;
        }
    }
    return line;
}

pub fn exists(path: <char>) -> bool {
    let nt_path = null_terminate(path);
    let fd = open(nt_path, 0, 0);
    if fd >= 0 {
        close(fd);
        return true;
    }
    return false;
}

pub fn is_dir(path: <char>) -> bool {
    let nt_path = null_terminate(path);
    let fd = open(nt_path, 0, 0);
    if fd < 0 {
        return false;
    }

    decl st: int[18];
    if fstat(fd, &st) < 0 {
        close(fd);
        return false;
    }
    close(fd);

    let mode = st[3];
    return (mode & 16384) == 16384;
}

pub fn mkdir(path: <char>) -> int ? <char> {
    let nt_path = null_terminate(path);
    let res = mkdir(nt_path, 448 | 32 | 8 | 4 | 1);
    if res == 0 {
        return 0;
    } else {
        return ?"Failed to create directory.";
    }
}

pub fn rm(path: <char>) -> int ? <char> {
    let nt_path = null_terminate(path);
    let res = unlink(nt_path);
    if res == 0 {
        return 0;
    } else {
        return ?"Failed to delete file.";
    }
}

pub fn rmdir(path: <char>) -> int ? <char> {
    let nt_path = null_terminate(path);
    let res = rmdir(nt_path);
    if res == 0 {
        return 0;
    } else {
        return ?"Failed to remove directory (must be empty).";
    }
}

pub fn rename(old_path: <char>, new_path: <char>) -> int ? <char> {
    let nt_old_path = null_terminate(old_path);
    let nt_new_path = null_terminate(new_path);
    let res = rename(nt_old_path, nt_new_path);
    if res == 0 {
        return 0;
    } else {
        return ?"Failed to rename file/directory.";
    }
}

pub fn list_dir(path: <char>) -> [<char>] ? <char> {
    let nt_path = null_terminate(path);
    let fd = open(nt_path, 0 | 65536, 0);
    if fd < 0 {
        return ?"Failed to open directory.";
    }

    let BUF_SIZE_INLINED = 4096;
    let buf = alloc(BUF_SIZE_INLINED) as *char;

    decl entries: [<char>];
    let offset_to_reclen_bytes = 16;
    let header_fixed_size_bytes = 19;

    loop {
        let bytes_read = getdents64(fd, buf as *any, BUF_SIZE_INLINED);
        if bytes_read <= 0 {
            break;
        }

        let current_pos = 0;
        while current_pos < bytes_read {
            let entry_ptr = buf + current_pos;

            let value_at_reclen_offset = *(entry_ptr as *int + (offset_to_reclen_bytes / 8));
            let d_reclen = value_at_reclen_offset & 65535;

            let name_ptr = entry_ptr + header_fixed_size_bytes;

            let name_len = d_reclen - header_fixed_size_bytes - 1;

            if name_len < 0 {
                name_len = 0;
            }

            for i in 0..name_len {
                if name_ptr[i] == '\0' {
                    name_len = i;
                    break;
                }
            }

            let name_slice = (name_len, name_ptr) as <char>;
            push(&entries, name_slice);

            current_pos += d_reclen;
        }
    }

    close(fd);
    return entries;
}

// --- PROCESS ---

pub fn run(command: <char>, argv: <<char>>) -> int ? <char> {
    let pid = fork();

    if pid < 0 {
        return ?"Failed to fork process.";
    }

    if pid == 0 {
        // --- This code runs in the CHILD process ---

        // We allocate space for the pointers from argv plus one for the null terminator.
        let exec_argv = alloc((len(argv) + 1) * sizeof(*char)) as **char;
        
        // Iterate through the user-provided argument slices, null-terminate each one,
        for i in 0..len(argv) {
            exec_argv[i] = null_terminate(argv[i]);
        }
        exec_argv[len(argv)] = null; // The array must end with a null pointer.

        // We'll pass a null environment pointer for simplicity.
        let envp = null as **char;

        // Convert the command path to a null-terminated string for the syscall.
        let nt_command = null_terminate(command);

        // execve replaces the current process image with a new one.
        // If it succeeds, it never returns.
        execve(nt_command, exec_argv, envp);

        // If execve returns, it means an error occurred.
        eprint("execve failed\n");
        exit(127); // Exit with a standard code indicating command execution failure.
    } else {
        // --- This code runs in the PARENT process ---

        // Allocate memory to store the exit status from wait4.
        let status_ptr = alloc(sizeof(int)) as *int;

        // wait4 waits for a child process to change state (e.g., terminate).
        let wait_res = wait4(pid, status_ptr, 0, null);

        if wait_res < 0 {
            return ?"wait4 failed while waiting for child process.";
        }

        // The integer returned by wait4 contains encoded status information.
        // We need to decode it to get the actual exit code.
        let status = *status_ptr;
        let exit_code = 0;

        // If the low byte is 0, the process exited normally.
        if (status & 0xff) == 0 {
            // The actual exit code is in the second byte.
            exit_code = (status >> 8) & 0xff;
        } else {
            // The process was terminated by a signal. The signal number is in the low 7 bits.
            // By convention, the exit code is 128 + signal number.
            let signal_num = status & 0x7f;
            exit_code = 128 + signal_num;
        }

        return exit_code;
    }
}

// --- GC ---

let SPACE_SIZE = 20;

decl allocator: (
    base: *any,
    size: int,
    offset: int,
    stack: *any
);

pub fn init() {
    allocator.offset = 0;
    allocator.stack = sp() + 8;
    allocator.base = null;
}

pub fn alloc(size: int) -> *any {
    if size <= 0 { size = 1; }
    size = (size + 7) & ~7; // Align to 8 bytes
    let required_block_size = size + 8; // Add space for header

    // --- Initial Heap Creation ---
    if allocator.base == null {
        let initial_size = *SPACE_SIZE;
        if (required_block_size > initial_size) {
            initial_size = required_block_size;
        }
        allocator.base = mmap(null, initial_size, 3, 34, 0, 0);
        allocator.size = initial_size;
    }

    // --- Collection and Growth Logic ---
    if allocator.offset + required_block_size > allocator.size {
        collect();
    }
    if allocator.offset + required_block_size > allocator.size {
        let old_size = allocator.size;
        
        let new_size = old_size * 2;
        let required_total = allocator.offset + required_block_size;
        while (new_size < required_total) {
            new_size *= 2;
        }
        *SPACE_SIZE = new_size;
        
        collect();
    }

    // --- Allocation ---
    let header_ptr = allocator.base + allocator.offset;
    
    *(header_ptr as *int) = required_block_size;
    allocator.offset += required_block_size;
    
    return header_ptr + 8; // Return pointer to payload
}

pub fn calloc(size: int) -> *any {
    let ptr = alloc(size);

    if ptr == null {
        return null;
    }

    // Phase 1: Zero out the bulk of the memory using `int` (8-byte) writes.
    let num_ints = size / 8;
    let i = 0;
    while i < num_ints {
        (ptr as *int)[i] = 0;
        i += 1;
    }

    // Phase 2: Zero out the remaining 1-7 bytes for precision.
    let remainder_offset = num_ints * 8;
    let j = remainder_offset;
    while j < size {
        (ptr as *char)[j] = 0;
        j += 1;
    }

    return ptr;
}

// Given any pointer `ptr` into the heap, find the header of the object it belongs to.
fn find_header_for(ptr: *any) -> *any {
    // This is a robust but slow linear scan of the heap. It replaces BiBOP.
    let current_header_ptr = allocator.base;

    // Walk the heap from the beginning.
    while current_header_ptr < allocator.base + allocator.offset {
        let object_size_val = *(current_header_ptr as *int);
        
        let object_size = object_size_val;
        if (object_size < 0) {
            // A forwarded object's header contains -size.
            object_size = -object_size;
        }

        if object_size == 0 {
            // Safeguard to prevent infinite loops on corrupted heap.
            return null;
        }

        let object_end_ptr = current_header_ptr + object_size;

        // Check if our target pointer `ptr` falls within this object's data payload.
        if ptr >= (current_header_ptr + 8) && ptr < object_end_ptr {
            return current_header_ptr;
        }

        // Move to the next object in the heap.
        current_header_ptr = object_end_ptr;
    }

    // Pointer was not found in any object in the heap.
    return null;
}

fn collect() {
    // --- Setup New Space ---
    let new_heap_size = *SPACE_SIZE;
    let to_space = mmap(null, new_heap_size, 3, 34, 0, 0);
    let to_space_offset = 0;

    // --- Scan Roots (Stack) ---
    let scan_ptr = allocator.stack;
    while scan_ptr > (sp() + 8) {
        scan_ptr = scan_ptr - 8;
        to_space_offset = collect_ptr(scan_ptr as **any, to_space, to_space_offset);
    }

    // --- Cheney's Algorithm: Scan the new heap ---
    let scan = to_space;
    while scan < to_space + to_space_offset {
        let object_header = scan;
        let object_size = *(object_header as *int);
        
        let payload_ptr = object_header + 8;
        let payload_size = object_size - 8;
        let payload_scan = payload_ptr;
        while payload_scan < payload_ptr + payload_size {
            to_space_offset = collect_ptr(payload_scan as **any, to_space, to_space_offset);
            payload_scan = payload_scan + 8;
        }
        scan = scan + object_size;
    }

    // --- Teardown Old Space ---
    munmap(allocator.base, allocator.size);

    // --- Activate New Space ---
    allocator.base = to_space;
    allocator.size = new_heap_size;
    allocator.offset = to_space_offset;
}

fn collect_ptr(ref_ptr: **any, to_space: *any, offset: int) -> int {
    let ptr = *ref_ptr;

    // Check if ptr is a valid heap pointer into the from_space.
    if ptr < allocator.base || ptr >= allocator.base + allocator.offset {
        return offset;
    }
    
    let header_ptr = find_header_for(ptr);
    if header_ptr == null { return offset; }

    let header_val = *(header_ptr as *int);

    if header_val < 0 { // Is already forwarded
        let new_header_loc = get_forward(header_ptr);
        let interior_offset = ptr - header_ptr;
        *ref_ptr = new_header_loc + interior_offset;
    } else { // Not forwarded, needs to be copied
        let object_size = header_val;
        let new_header_loc = to_space + offset;
        
        copy(header_ptr, new_header_loc, object_size);
        
        set_forward(header_ptr, new_header_loc);
        
        let interior_offset = ptr - header_ptr;
        *ref_ptr = new_header_loc + interior_offset;
        
        offset = offset + object_size;
    }
    return offset;
}

fn set_forward(ptr: *any, new_loc: *any) {
    let size = *(ptr as *int);
    *(ptr as *int) = -size;
    *((ptr + 8) as **any) = new_loc;
}

fn get_forward(ptr: *any) -> *any {
    return *((ptr + 8) as **any);
}

pub fn print_heap() {
    print("HEAP: ");
    let ptr = (allocator.base) as *int;
    while ptr < allocator.base + allocator.offset {
        print(*ptr);
        print(" :: ");
        ptr += 1;
    }
    print('\n');
}

pub fn print_stack(offset: int, len: int) {
    print("STACK: |");
    let ptr = sp() as *int + offset + 3;
    print(ptr);
    print("| :: ");
    for i in 0..len {
        print(*ptr);
        print(" :: ");
        ptr += 1;
    }
    print('|');
    print(ptr);
    print("|");
    print('\n');
}

