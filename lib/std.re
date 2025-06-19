syscall 1: write(int, *char, int) -> int;
syscall 2: open(*char, int, int) -> int;
syscall 5: fstat(int, *any) -> int;
syscall 6: close(int) -> int;
syscall 9: mmap(*any, int, int, int, int, int) -> *any;
syscall 10: munmap(*any, int);
syscall 60: exit(int);

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
    if len(args) - 1 != len(expected) {
        let ret = "Invalid argument count, expected {len(expected)} argument(s), got {len(args) - 1}.\n";
        return ?ret;
    }
    return 0;
}

pub fn parse(str: *char, res: *int) -> int ? <char> {
    *res = 0;
    let slice = str(str);
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

pub fn parse(str: *char, res: *<char>) -> int ? <char> {
    let slice = str(str);
    copy(&slice, res, 16);
    return 0;
}

fn shift_args(argc: *int, argv: **char, start: int, count: int) {
    for i in start..*argc-count {
        let ptr = argv + i;
        *ptr = argv[i+count];
    }
    *argc -= count;
}

pub fn parse_opt<T>(argc: *int, argv: **char, name: <char>, opt: *?T) -> int ? <char> {
    let flag = +"--";
    push(&flag, name);
    for i in 0..*argc {
        let arg = argv[i];
        if cmp(arg, flag) {
            *(opt as *int) = 0;
            if i >= argc {
                return ?"Optional argument required for '{name}'.\n";
            }
            let res = parse(argv[i+1], (opt as *int + 1) as *T);
            shift_args(argc, argv, i, 2);
            return res;
        }
    }
    *(opt as *int) = 1;
    return 0;
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
    let ptr = alloc(32) as *char;
    if x < 0 {
        *ptr = '-';
        ptr += 1;
        x = -x;
    }
    let end = int2str(x, ptr);
    return (end - ptr, ptr);
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

pub fn str<T>(x: ?T) -> <char> {
    if let s = x {
        return str(s);
    } else {
        return "none";
    }
}

pub fn str<T>(x: <T>) -> <char> {
    let s = +"[";
    let start = true;
    for el in x {
        if start {
            start = false;
        } else {
            push(&s, ", ");
        }
        push(&s, str(el));
    }
    push(&s, ']');
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

pub fn push<T>(list: *[T], el: T) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(el) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(el);
    if size >= cap {
		let new_ptr = alloc(cap * 2);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(&el, ptr as *T + len(list), sizeof(el));
	*(list as *any as *int) += 1;
}

pub fn push<T>(list: *[T], sl: <T>) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(*(sl as *T)) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(*(sl as *T));
	let add_size = len(sl) * sizeof(*(sl as *T));
    if size + add_size > cap {
		let new_ptr = alloc(cap + add_size);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(sl as *T, ptr as *T + len(list), add_size);
	*(list as *int) += len(sl);
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

pub fn hash(x: int) -> int {
    let hash = 14695981039346656037;
    for i in 0..8 {
        hash ^= (x >> (i * 8)) & 255;
        hash *= 1099511628211;
    }
    return hash;
}

pub fn init_map<K, V>(fields: <(K, V)>) -> {K: V} {
    let capacity = 100;
    let ptr = alloc(capacity * sizeof(fields[0])) as *(K, V);
    for f in fields {
        let i = hash(f[0]) % capacity;
        ptr[i] = f;
    }
    return ptr;
}

pub fn get<K, V>(map: {K: V}, key: K) -> ?V {
    if map as *any == null {
        return none;
    }
    let capacity = *(map as *int - 1) / sizeof(*(map as *(K, V)));
    let i = hash(key) % capacity;
    let k_ptr = (map as *(K, V) + i) as *K;
    if *(k_ptr as *int) == 0 {
        return none;
    }
    return *((k_ptr + 1) as *V);
}

pub fn insert<K, V>(map_ref: *{K: V}, key: K, value: V) {
    if *map_ref == null {
        *map_ref = init_map((0, null) as <(K, V)>);
    }
    let map = *map_ref;
    let capacity = *(map as *int - 1) / sizeof(*(map as *(K, V)));
    let i = hash(key) % capacity;
    let k_ptr = (map as *(K, V) + i) as *K;
    *k_ptr = key;
    *((k_ptr + 1) as *V) = value;
}

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


decl allocator: (base: *any, size: int, offset: int, stack: *any);
let SPACE_SIZE = 20;

pub fn init() {
    allocator.offset = 0;
    allocator.stack = sp() + 8;
}

pub fn alloc(size: int) -> *any {
    if size <= 0 {
        size = 1;
    }
    size = (size + 7) & ~7;
    let new_offset = allocator.offset + size + 8;
    if allocator.base == null {
        while new_offset > *SPACE_SIZE {
            *SPACE_SIZE = *SPACE_SIZE * 2;
        }
        allocator.base = mmap(null, *SPACE_SIZE, 3, 34, 0, 0);
        allocator.size = *SPACE_SIZE;
    }
    if new_offset > allocator.size {
        collect();
        new_offset = allocator.offset + size + 8;
        if new_offset > allocator.size {
            while new_offset > *SPACE_SIZE {
                *SPACE_SIZE = *SPACE_SIZE * 2;
            }
            let to_space = mmap(null, *SPACE_SIZE, 3, 34, 0, 0);
            copy(allocator.base, to_space, allocator.offset);
            munmap(allocator.base, allocator.size);
            allocator.base = to_space;
            allocator.size = *SPACE_SIZE;
        }
    }
    let res = allocator.base + allocator.offset;
    *(res as *int) = size + 8;
    *(res as *int + 1) = 0;
    allocator.offset = new_offset;
    return res + 8;
}

fn is_forwarded(ptr: *any) -> bool {
    return (*(ptr as *int)) < 0;
}

fn get_forward(ptr: *any) -> *any {
    return *((ptr + 8) as **any);
}

fn set_forward(ptr: *any, new_loc: *any) {
    *(ptr as *int) = -*(ptr as *int);
    *((ptr + 8) as **any) = new_loc;
}

fn collect() {
    let stack_end = sp() + 8;
    let scan = allocator.stack;
    let to_space = mmap(null, *SPACE_SIZE, 3, 34, 0, 0);
    let offset = 0;

    while scan > stack_end {
        scan -= 8;
        offset = collect_ptr(scan as **any, to_space, offset);
    }
    scan = to_space;
    while scan < to_space + offset {
        offset = collect_ptr(scan as **any, to_space, offset);
        scan += 8;
    }

    munmap(allocator.base, allocator.size);
    allocator.base = to_space;
    allocator.offset = offset;
}

fn collect_ptr(ref_ptr: **any, to_space: *any, offset: int) -> int {
    let ptr = *ref_ptr;
    if ptr < allocator.base || ptr >= allocator.base + allocator.offset {
        return offset;
    }

    let scan = allocator.base;
    while scan < allocator.base + allocator.offset {
        let size = *(scan as *int);
        if size == 0 {
            break;
        }
        let real_size = size;
        if size < 0 {
            real_size = -size;
        }
        if ptr >= scan && ptr < scan + real_size {
            if size < 0 {
                *ref_ptr = get_forward(scan) + (ptr - scan);
            } else {
                copy(scan, to_space + offset, size);
                set_forward(scan, to_space + offset);

                *ref_ptr = (to_space + offset) + (ptr - scan);
                offset += size;
            }
            break;
        }
        scan += real_size;
    }
    return offset;
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
    for let i = 0; i < len; i += 1 {
        print(*ptr);
        print(" :: ");
        ptr += 1;
    }
    print('|');
    print(ptr);
    print("|");
    print('\n');
}
