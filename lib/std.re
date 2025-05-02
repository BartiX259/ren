syscall 1: write(int, *char, int);

fn int2str(x: int, buf: *char) -> *char {
    if x < 10 {
        *buf = '0' + x;
        return buf + 1;
    }
    buf = int2str(x / 10, buf);
    *buf = '0' + (x % 10);
    return buf + 1;
}

pub fn strlen(x: int) -> int {
    if x == 0 {
        return 1;
    }
    let count = 0;
    decl n: int;
    if x < 0 {
        n = -x;
    } else {
        n = x;
    }
    while n > 0 {
        count += 1;
        n /= 10;
    }
    if x < 0 {
        count += 1;
    }
    return count;
}

pub fn str(x: int, buffer: *char) -> *char {
    let i = 0;
    let is_negative = false;
    if x == 0 {
        buffer[i] = '0';
        return buffer + 1;
    }
    if x < 0 {
        is_negative = true;
        x = -x;
    }
    while (x > 0) {
        buffer[i] = (x % 10) + '0';
        i += 1;
        x /= 10;
    }
    if is_negative {
        buffer[i] = '-';
        i += 1;
    }
    let j = 0;
    let k = i - 1;
    while j < k {
        let temp = buffer[j];
        buffer[j] = buffer[k];
        buffer[k] = temp;
        j += 1;
        k -= 1;
    }
    return buffer + i;
}

pub fn strlen(x: <char>) -> int {
    return len(x);
}

pub fn str(x: <char>, buffer: *char) -> *char {
    for let i = 0; i < len(x); i += 1 {
        buffer[i] = (x as *char)[i];
    }
    return buffer + len(x);
}

pub fn print(x: int) {
    decl buf: char[32];
    let ptr = &buf;
    if x < 0 {
        *ptr = '-';
        ptr += 1;
        x = -x;
    }
    let end = int2str(x, ptr);
    write(1, &buf, end - &buf);
}

pub fn print(x: <char>) {
    write(1, x as *char, len(x));
}

pub fn print(x: bool) {
    if (x) {
        print("true");
    } else {
        print("false");
    }
}

pub fn print(x: char) {
    write(1, &x, 1);
}

pub fn print(x: *char) {
    let original_x = x;
    let len = 0;
    while *x != 0 {
        len += 1;
        x += 1;
    }
    write(1, original_x, len);
}

pub fn print<T>(x: <T>) {
    print('[');
    let start = true;
    for let i = 0; i < len(x); i += 1 {
        if start {
            start = false;
        } else {
            print(", ");
        }
        print(x[i]);
    }
    print(']');
}

syscall 9: mmap(int, int, int, int, int, int) -> *any;
syscall 10: munmap(*any, int);

decl allocator: (base: *any, size: int, offset: int, stack: *any);
let SPACE_SIZE = 20;

pub fn init() {
    allocator.offset = 0;
    allocator.stack = sp() + 8;
}

pub fn alloc(size: int) -> *any {
    size = (size + 7) & ~7;
    let new_offset = allocator.offset + size + 8;
    if allocator.base == null {
        while new_offset > *SPACE_SIZE {
            print("DOUBLE\n");
            *SPACE_SIZE = *SPACE_SIZE * 2;
        }
        allocator.base = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
        allocator.size = *SPACE_SIZE;
    }
    if new_offset > allocator.size {
        collect();
        new_offset = allocator.offset + size + 8;
        if new_offset > allocator.size {
            while new_offset > *SPACE_SIZE {
                print("DOUBLE\n");
                *SPACE_SIZE = *SPACE_SIZE * 2;
            }
            let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
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
    print("gc: ");
    print_heap();

    let stack_end = sp() + 8;
    let scan = allocator.stack;
    let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
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
    print("gc ok: ");
    print_heap();
}

fn collect_ptr(ref_ptr: **any, to_space: *any, offset: int) -> int {
    let ptr = *ref_ptr;
    if ptr < allocator.base || ptr >= allocator.base + allocator.offset {
        return offset;
    }
    print("found: ");
    print(ptr);
    print('\n');

    let scan = allocator.base;
    while scan < allocator.base + allocator.offset {
        print("scan: ");
        print(ptr - scan);
        print(',');
        let size = *(scan as *int);
        if size == 0 {
            print("err\n");
            print_heap();
            break;
        }
        print(size);
        print('\n');
        let real_size = size;
        if size < 0 {
            real_size = -size;
        }
        if ptr >= scan && ptr < scan + real_size {
            print(size);
            if size < 0 {
                print('a');
                *ref_ptr = get_forward(scan) + (ptr - scan);
            } else {
                print('b');
                copy(scan, to_space + offset, size);
                set_forward(scan, to_space + offset);

                *ref_ptr = (to_space + offset) + (ptr - scan);
                offset += size;
            }
            print("xd\n");
            break;
        }
        scan += real_size;
    }
    return offset;
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
	*(list as *any as *int) += len(sl);
}

pub fn split<T>(sl: <T>, split: T) -> [<T>] {
	let base = 0;
	decl res: [<T>];
	for let i = 0; i < len(sl); i += 1 {
		if sl[i] == split {
			let s = sl[base..i];
			push(&res, s);
			base = i + 1;
		}
	}
	push(&res, sl[base..]);
	return res;
}

syscall 2: open(*char, int, int) -> int;
syscall 5: fstat(int, *any) -> int;
syscall 6: close(int) -> int;

pub fn read(path: *char) -> <char> ? <char> {
    let fd = open(path, 0, 0);
    if fd < 0 {
        return ?"Failed to open file.\n";
    }

    decl st: int[18];
    if fstat(fd, &st) < 0 {
        close(fd);
        return ?"Failed to stat file.\n";
    }

    let size = st[6];
    let mapped = mmap(0, size, 3, 2, fd, 0);
    close(fd);

    return (mapped as *char)[..size];
}

