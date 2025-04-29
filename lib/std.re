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

fn strlen(x: int) -> int {
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

fn str(x: int, buffer: *char) -> *char {
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

fn strlen(x: <char>) -> int {
    return len(x);
}

fn str(x: <char>, buffer: *char) -> *char {
    for let i = 0; i < len(x); i += 1 {
        buffer[i] = (x as *char)[i];
    }
    return buffer + len(x);
}

fn print(x: int) {
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

fn print(x: <char>) {
    write(1, x as *char, len(x));
}

fn print(x: bool) {
    if (x) {
        print("true");
    } else {
        print("false");
    }
}

fn print(x: char) {
    write(1, &x, 1);
}

fn print(x: *char) {
    let original_x = x;
    let len = 0;
    while *x != 0 {
        len += 1;
        x += 1;
    }
    write(1, original_x, len);
}

fn print<T>(x: <T>) {
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
let SPACE_SIZE = 2000;

fn alloc(size: int) -> *any {
    size = (size + 7) & ~7;
    if allocator.base == null {
        allocator.base = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
        allocator.size = *SPACE_SIZE;
        allocator.offset = 0;
        allocator.stack = sp() + 16;
    }
    let new_offset = allocator.offset + size + 8;
    if new_offset > allocator.size {
        collect();
        new_offset = allocator.offset + size + 8;
        if new_offset > allocator.size {
            while new_offset > *SPACE_SIZE {
                print("DOUBLE\n");
                *SPACE_SIZE = *SPACE_SIZE * 2;
            }
            let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
            copy(allocator.base, to_space, allocator.size);
            munmap(allocator.base, allocator.size);
            allocator.base = to_space;
            allocator.size = *SPACE_SIZE;
        }
    }
    let res = allocator.base + allocator.offset;
    *(res as *int) = size + 8;
    allocator.offset = new_offset;
    return res + 8;
}

fn print_heap() {
    print("HEAP: ");
    let ptr = (allocator.base) as *int;
    while ptr < allocator.base + allocator.offset {
        print(*ptr);
        print(" :: ");
        ptr += 1;
    }
    print('\n');
}

fn print_stack(len: int) {
    print("STACK: ");
    let ptr = sp() as *int;
    for let i = 0; i < len; i += 1 {
        print(*ptr);
        print(" :: ");
        ptr -= 1;
    }
    print('\n');
}

fn is_forwarded(ptr: *any) -> bool {
    return (*(ptr as *int)) < 0;
}

fn get_forward(ptr: *any) -> *any {
    return *((ptr + 8) as **any);
}

fn set_forward(ptr: *any, new_loc: *any) {
    *(ptr as *int) = -1;
    *((ptr + 8) as **any) = new_loc;
}

fn collect() {
    print("gc\n");

    let stack_end = sp();
    let stack_start = allocator.stack;
    let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
    let offset = 0;

    while stack_start > stack_end {
        stack_start -= 8;
        let ref_ptr = *(stack_start as **any) - 8;

        if ref_ptr >= allocator.base && ref_ptr < allocator.base + allocator.size {
            print("found: ");
            print(ref_ptr as int);
            print('\n');

            if is_forwarded(ref_ptr) {
                *(stack_start as **any) = get_forward(ref_ptr) + 8;
            } else {
                let size = *(ref_ptr as *int);
                print("size: ");
                print(size);
                print('\n');

                copy(ref_ptr, to_space + offset, size);
                set_forward(ref_ptr, to_space + offset);

                *(stack_start as **any) = (to_space + offset) + 8;
                offset += size;
            }
        }
    }

    munmap(allocator.base, allocator.size);
    allocator.base = to_space;
    allocator.offset = offset;
    print("gc ok\n");
}

