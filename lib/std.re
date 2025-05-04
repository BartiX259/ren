syscall 1: write(int, *char, int) -> int;
syscall 2: open(*char, int, int) -> int;
syscall 5: fstat(int, *any) -> int;
syscall 6: close(int) -> int;
syscall 9: mmap(*any, int, int, int, int, int) -> *any;
syscall 10: munmap(*any, int);
syscall 60: exit(int);

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
    write(1, "alloc\n", 6);
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
    write(1, "gc\n", 3);
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
